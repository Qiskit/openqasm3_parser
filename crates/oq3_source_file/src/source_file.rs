// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use oq3_syntax::ast as synast; // Syntactic AST
use oq3_syntax::Parse;
use oq3_syntax::TextRange;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

// traits
use oq3_syntax::ast::HasModuleItem;

use crate::api::{inner_print_compiler_errors, parse_source_file, print_compiler_errors};

pub(crate) fn parse_source_and_includes<P: AsRef<Path>>(
    source: &str,
    search_path_list: Option<&[P]>,
) -> (ParsedSource, Vec<SourceFile>) {
    let syntax_ast: ParsedSource = synast::SourceFile::parse(source);
    let included = parse_included_files(&syntax_ast, search_path_list);
    (syntax_ast, included)
}

// I think SourceFile actually just works with the source as a string.
// Knowledge of any file is not used by synast::SourceFile;
pub(crate) type ParsedSource = Parse<synast::SourceFile>;

pub trait ErrorTrait {
    fn message(&self) -> String;

    fn range(&self) -> TextRange;
}

pub(crate) fn range_to_span(range: &TextRange) -> std::ops::Range<usize> {
    let r1: usize = range.start().into();
    let r2: usize = range.end().into();
    // maybe rowan and ariadne differ on def of span
    // In any case, for ariadne, r2 > r1 is a requirement. Often not satisfied by the r-a crates
    // r1..(r2+1) <--- However, this sometimes is past EOF.
    r1..r2
}

impl ErrorTrait for oq3_syntax::SyntaxError {
    fn message(&self) -> String {
        self.message().to_string()
    }

    fn range(&self) -> TextRange {
        self.range()
    }
}

pub trait SourceTrait {
    /// Return `true` if the source file or any included files produced a parse error.
    fn any_parse_errors(&self) -> bool {
        if !&self.syntax_ast().errors().is_empty() {
            return true;
        }
        self.included()
            .iter()
            .any(|inclusion| inclusion.any_parse_errors())
    }

    fn included(&self) -> &Vec<SourceFile>;
    fn syntax_ast(&self) -> &ParsedSource;
    fn print_syntax_errors(&self);
    fn file_path(&self) -> PathBuf;
}

impl SourceTrait for SourceFile {
    fn syntax_ast(&self) -> &ParsedSource {
        &self.syntax_ast
    }

    fn included(&self) -> &Vec<SourceFile> {
        self.included.as_ref()
    }

    fn file_path(&self) -> PathBuf {
        self.file_path().clone()
    }

    fn print_syntax_errors(&self) {
        print_compiler_errors(self.syntax_ast().errors(), &self.file_path);
        for source_file in self.included().iter() {
            source_file.print_syntax_errors()
        }
    }
}

impl SourceFile {
    pub fn new<F: AsRef<Path>>(
        file_path: F,
        syntax_ast: ParsedSource,
        included: Vec<SourceFile>,
    ) -> SourceFile {
        let file_path = match fs::canonicalize(file_path.as_ref()) {
            Ok(file_path) => file_path,
            Err(e) => panic!("Unable to find {:?}\n{:?}", file_path.as_ref(), e),
        };
        SourceFile {
            file_path,
            syntax_ast,
            included,
        }
    }

    pub fn file_path(&self) -> &PathBuf {
        &self.file_path
    }
}

pub fn search_paths() -> Option<Vec<PathBuf>> {
    env::var_os("QASM3_PATH").map(|paths| env::split_paths(&paths).collect())
}

/// Expand path with search paths. Return input if expansion fails.
pub(crate) fn expand_path<T: AsRef<Path>, P: AsRef<Path>>(
    file_path: T,
    search_path_list: Option<&[P]>,
) -> PathBuf {
    let file_path = PathBuf::from(file_path.as_ref());
    if file_path.is_absolute() {
        return file_path;
    }
    let try_path = |dir: &Path| {
        let full_path = dir.join(&file_path);
        full_path.is_file().then_some(full_path)
    };

    if let Some(paths) = search_path_list {
        for path in paths {
            if let Some(full_path) = try_path(path.as_ref()) {
                return full_path;
            }
        }
    } else if let Some(paths) = search_paths() {
        for path in paths {
            if let Some(full_path) = try_path(path.as_ref()) {
                return full_path;
            }
        }
    }
    file_path
}

/// Read QASM3 source file, respecting env variable `QASM3_PATH` if set.
pub(crate) fn read_source_file(file_path: &Path) -> String {
    match fs::read_to_string(file_path) {
        Ok(source) => source,
        Err(err) => panic!(
            "Unable to read OpenQASM source file '{}': {}",
            file_path.display(),
            err
        ),
    }
}

// FIXME: prevent a file from including itself. Then there are two-file cycles, etc.
///  Recursively parse any files `include`d in the program `syntax_ast`.
pub(crate) fn parse_included_files<P: AsRef<Path>>(
    syntax_ast: &ParsedSource,
    search_path_list: Option<&[P]>,
) -> Vec<SourceFile> {
    syntax_ast
        .tree()
        .statements()
        .filter_map(|parse_item| match parse_item {
            synast::Stmt::Item(synast::Item::Include(include)) => {
                let file: synast::FilePath = include.file().unwrap();
                let file_path = file.to_string().unwrap();
                Some(parse_source_file(file_path, search_path_list))
            }
            _ => None,
        })
        .collect::<Vec<_>>()
}

/// Structure for managing parsing QASM from a string with no associated
/// source file. `fake_file_path` contains something like "no file" and is
/// present in order to make it easier to avoid duplicating code supporing
/// QASM read from source files.
#[derive(Clone, Debug)]
pub struct SourceString {
    pub(crate) fake_file_path: PathBuf, // Option<String>, // Typical name is "no file".
    pub(crate) source: String,
    pub(crate) syntax_ast: ParsedSource,
    pub(crate) included: Vec<SourceFile>,
}

#[derive(Clone, Debug)]
pub struct SourceFile {
    file_path: PathBuf,
    syntax_ast: ParsedSource,
    included: Vec<SourceFile>,
}

impl SourceTrait for SourceString {
    fn syntax_ast(&self) -> &ParsedSource {
        &self.syntax_ast
    }

    fn included(&self) -> &Vec<SourceFile> {
        self.included.as_ref()
    }

    fn file_path(&self) -> PathBuf {
        self.fake_file_path().clone()
    }

    fn print_syntax_errors(&self) {
        // Print errors from top level source.
        inner_print_compiler_errors(
            self.syntax_ast().errors(),
            self.fake_file_path(),
            self.source(),
        );
        // Print from included source files (recursively).
        for source_file in self.included().iter() {
            source_file.print_syntax_errors()
        }
    }
}

impl SourceString {
    pub fn new<T: AsRef<str>, P: AsRef<Path>>(
        source: T,
        fake_file_path: P,
        syntax_ast: ParsedSource,
        included: Vec<SourceFile>,
    ) -> SourceString {
        SourceString {
            source: source.as_ref().to_owned(),
            fake_file_path: fake_file_path.as_ref().to_owned(),
            syntax_ast,
            included,
        }
    }

    pub fn fake_file_path(&self) -> &PathBuf {
        &self.fake_file_path
    }

    pub fn source(&self) -> &str {
        &self.source
    }
}
