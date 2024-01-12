// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use oq3_syntax::ast as synast; // Syntactic AST
use oq3_syntax::Parse;
use oq3_syntax::TextRange;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

// traits
use oq3_syntax::ast::HasModuleItem;

use crate::api::{inner_print_compiler_errors, parse_source_file, print_compiler_errors};

pub(crate) fn parse_source_and_includes(source: &str) -> (ParsedSource, Vec<SourceFile>) {
    let syntax_ast: ParsedSource = synast::SourceFile::parse(source);
    let included = parse_included_files(&syntax_ast);
    (syntax_ast, included)
}

// I think SourceFile actually just works with the source as a string.
// Knowledge of any file is not used by synast::SourceFile;
pub(crate) type ParsedSource = Parse<synast::SourceFile>;

/// This trait requires implementing `normalize` which returns an absolute path. `normalize`
/// essentially wraps `fs::canonicalize`. It exists so the consumer can initialize paths
/// with `PathBuf`, `&PathBuf`, `&str`, `&OsStr` etc.
/// The method `to_path_buf` which converts to `PathBuf` without `canonicalize` is also required.
pub trait Normalizeable {
    fn normalize(&self) -> PathBuf;
    fn to_path_buf(self) -> PathBuf;
}

impl Normalizeable for PathBuf {
    fn to_path_buf(self) -> Self {
        self
    }

    /// `normalize` essentially wraps `fs::canonicalize`. See `Normalizeable`.
    fn normalize(&self) -> PathBuf {
        match fs::canonicalize(self) {
            Ok(file_path) => file_path,
            Err(e) => panic!("Unable to find {}\n{e}", self.display()),
        }
    }
}

impl Normalizeable for &PathBuf {
    fn to_path_buf(self) -> PathBuf {
        self.clone()
    }

    fn normalize(&self) -> PathBuf {
        match fs::canonicalize(self) {
            Ok(file_path) => file_path,
            Err(e) => panic!("Unable to find {}\n{e}", self.display()),
        }
    }
}

// FIXME: Can probably use From trait in order to minimize code supporting
// this trait, or perhaps eliminate it.
impl Normalizeable for &str {
    fn to_path_buf(self) -> PathBuf {
        PathBuf::from(self)
    }

    fn normalize(&self) -> PathBuf {
        PathBuf::from(self).normalize()
    }
}

impl Normalizeable for &OsStr {
    fn to_path_buf(self) -> PathBuf {
        PathBuf::from(&self)
    }

    fn normalize(&self) -> PathBuf {
        PathBuf::from(self).normalize()
    }
}

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
    pub fn new<F: Normalizeable>(
        file_path: F,
        syntax_ast: ParsedSource,
        included: Vec<SourceFile>,
    ) -> SourceFile {
        SourceFile {
            file_path: file_path.normalize(),
            syntax_ast,
            included,
        }
    }

    pub fn file_path(&self) -> &PathBuf {
        &self.file_path
    }
}

pub fn search_paths() -> Option<Vec<PathBuf>> {
    env::var_os("QASM3_PATH").map(|paths| env::split_paths(&paths).collect::<Vec<_>>())
}

// Expand path with search paths. Return input if expansion fails.
pub(crate) fn expand_path(file_path: &PathBuf) -> PathBuf {
    if file_path.is_absolute() {
        return file_path.clone();
    }
    let mut maybe_full_path = file_path.clone();
    if let Some(paths) = search_paths() {
        for path in paths {
            let fqpn = path.join(file_path);
            if fqpn.is_file() {
                maybe_full_path = fqpn;
                break;
            }
        }
    }
    maybe_full_path
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
// FIXME: I want to disable filter_map_identity globally, but there is no option for clippy.toml
#[allow(clippy::filter_map_identity)]
///  Recursively parse any files `include`d in the program `syntax_ast`.
pub(crate) fn parse_included_files(syntax_ast: &ParsedSource) -> Vec<SourceFile> {
    syntax_ast
        .tree()
        .statements()
        .map(|parse_item| match parse_item {
            synast::Stmt::Item(synast::Item::Include(include)) => {
                let file: synast::FilePath = include.file().unwrap();
                let file_path = file.to_string().unwrap();
                Some(parse_source_file(&PathBuf::from(file_path)))
            }
            _ => None,
        })
        .filter_map(|x| x)
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
    pub fn new<T: ToString, F: Normalizeable>(
        source: T,
        fake_file_path: F,
        syntax_ast: ParsedSource,
        included: Vec<SourceFile>,
    ) -> SourceString {
        let fake_file_path_buf = fake_file_path.to_path_buf();
        SourceString {
            source: source.to_string(),
            fake_file_path: fake_file_path_buf,
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
