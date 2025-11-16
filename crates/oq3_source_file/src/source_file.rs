// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

use crate::api::{inner_print_compiler_errors, print_compiler_errors};
use crate::parse_source_file_with_search;
use oq3_syntax::ast as synast; // Syntactic AST
use oq3_syntax::ParseOrErrors;
use oq3_syntax::TextRange;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

// `SourceFile` is a misnomer. It actually just works with the source as a string.
// `synast::SourceFile` has no knowledge of path names, filesystems, io streams, etc.
pub(crate) type ParsedSource = ParseOrErrors<synast::SourceFile>;

/// Lex and parse the OpenQASM code in `source_string` into an AST.
/// If any errors occur during lexing, these are stored in place of the syntax errors.
/// and no parsing of the lexed code is actually done.
/// In case `source_string` is successfully parsed, also parse any source code inlcuded
/// via `include` statements.
pub(crate) fn parse_source_and_includes<P: AsRef<Path>>(
    source_string: &str,
    search_path_list: Option<&[P]>,
) -> (ParsedSource, Vec<SourceFile>) {
    let parsed_source = synast::SourceFile::parse_check_lex(source_string);
    let parsed_included_source = if parsed_source.have_parse() {
        parse_included_files(&parsed_source, search_path_list)
    } else {
        Vec::<SourceFile>::new()
    };
    (parsed_source, parsed_included_source)
}

/// The crate text-range defines `TextRange`.
/// Errors are displayed with the crate `ariadne`, which uses `Range` (I think).
/// We have to convert from the former to the latter.
///
/// The origin of `TextRange` is confusing (maybe to my lsp). I think it is imported like this:
/// text-range -> rowan -> oq3_syntax -> here.
pub(crate) fn range_to_span(range: &TextRange) -> std::ops::Range<usize> {
    let r1: usize = range.start().into();
    let r2: usize = range.end().into();
    // maybe rowan and ariadne differ on def of span
    // In any case, for ariadne, r2 > r1 is a requirement. Often not satisfied by the r-a crates
    // r1..(r2+1) <--- However, this sometimes is past EOF.
    r1..r2
}

pub trait ErrorTrait {
    /// Return a message describing the error.
    fn message(&self) -> String;

    /// Return the character range in the source associated with the error.
    fn range(&self) -> TextRange;
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

/// Read the environment variable `QASM3_PATH` and return a list of directory paths to search
/// qasm source files.
pub fn get_file_search_paths_from_env() -> Option<Vec<PathBuf>> {
    env::var_os("QASM3_PATH").map(|paths| env::split_paths(&paths).collect())
}

/// Try to find `file_path`, possibly by expanding with paths in `search_path_list`.
/// Return successfully expanded path, or return input if expansion fails.
///
/// 1) If `file_path` is absolute, return `file_path`.
/// 2) Else, iterate through any (directory) paths in `search_path_list`,
///    joining `file_path` to each directory path. Return the first full path that
///    exists, if one exists, on the filesystem.
/// 3) Else search in the same way the path list given in `QASM3_PATH`.
/// 4) Else, finding an existing full file path failed. Return the input `file_path`.
pub(crate) fn resolve_file_path<T: AsRef<Path>, P: AsRef<Path>>(
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
    } else if let Some(paths) = get_file_search_paths_from_env() {
        for path in paths {
            if let Some(full_path) = try_path(path.as_ref()) {
                return full_path;
            }
        }
    }
    // `file_path` is not absolute, and it is not found on any of the search paths.
    // Return the input `file_path`.
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
/// `syntax_ast` -- the already-parsed parent source file.
/// `search_path_list` -- a list of paths used for resolving filenams in `include` statements.
pub(crate) fn parse_included_files<P: AsRef<Path>>(
    syntax_ast: &ParsedSource,
    search_path_list: Option<&[P]>,
) -> Vec<SourceFile> {
    syntax_ast
        .tree()
        .statements()
        .filter_map(|parse_stmt| match parse_stmt {
            synast::Stmt::Include(include) => {
                let file: synast::FilePath = include.file().unwrap();
                let file_path = file.to_string().unwrap();
                // stdgates.inc will be handled "as if" it really existed.
                if file_path == "stdgates.inc" {
                    None
                } else {
                    Some(parse_source_file_with_search(file_path, search_path_list))
                }
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
