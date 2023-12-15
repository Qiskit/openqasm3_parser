use std::path::PathBuf;
use std::ffi::OsStr;
use std::fs;
use oq3_syntax::ast as synast; // Syntactic AST
use oq3_syntax::{Parse};
use oq3_syntax::TextRange;

// traits
use oq3_syntax::ast::HasModuleItem;
use crate::error_report;

// I think SourceFile actually just works with the source as a string.
// Knowledge of any file is not used by synast::SourceFile;
type ParsedSource = Parse<synast::SourceFile>;

#[derive(Clone, Debug)]
pub struct SourceFile {
    file_path: PathBuf,
    syntax_ast: ParsedSource,
    included: Vec<SourceFile>,
}

pub trait Normalizeable {
    fn normalize(&self) -> PathBuf;
}

impl Normalizeable for PathBuf {
    fn normalize(&self) -> PathBuf {
        match fs::canonicalize(self) {
            Ok(file_path) => file_path,
            Err(e) => panic!("Unable to find {}\n{e}", self.display()),
        }
    }
}

impl Normalizeable for &PathBuf {
    fn normalize(&self) -> PathBuf {
        match fs::canonicalize(self) {
            Ok(file_path) => file_path,
            Err(e) => panic!("Unable to find {}\n{e}", self.display()),
        }
    }
}

impl Normalizeable for &str {
    fn normalize(&self) -> PathBuf {
        PathBuf::from(self).normalize()
    }
}

impl Normalizeable for &OsStr {
    fn normalize(&self) -> PathBuf {
        PathBuf::from(self).normalize()
    }
}

pub trait ErrorTrait {
    fn message(&self) -> String;

    fn range(&self) -> TextRange;
}

pub fn range_to_span(range: &TextRange) -> std::ops::Range<usize> {
    let r1: usize  = range.start().into();
    let r2: usize  = range.end().into();
    // maybe rowan and ariadne differ on def of span
    // In any case, for ariadne, r2 > r1 is a requirement. Often not satisfied by the r-a crates
    // r1..(r2+1) <--- However, this sometimes is past EOF.
    r1..r2
}

pub fn inner_print_compiler_errors<T: ErrorTrait>(errors: &[T], file_path_str: &str, source: &str) {
    for err in errors.iter() {
        let err_string = err.message();
        let err_span = range_to_span(&err.range());
        error_report::report_error(&err_string, &err_span, file_path_str, source);
        println!();
    }
}

pub fn print_compiler_errors<T: ErrorTrait>(errors: &[T], file_path: &PathBuf) {
    // ariadne seems to want path only as &str, not PathBuf.
    let file_path_str = file_path.as_os_str().to_str().unwrap(); // FIXME: check this
    let source = &read_source_file(file_path);
    inner_print_compiler_errors(errors, file_path_str, source);
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
        if ! &self.syntax_ast().errors().is_empty() {
            return true
        }
        self.included().iter().any(|inclusion| inclusion.any_parse_errors())
    }

    fn included(&self) -> &Vec<SourceFile>;
    fn syntax_ast(&self) -> &ParsedSource;
    fn print_syntax_errors(&self);
}

impl SourceTrait for SourceFile {
    fn syntax_ast(&self) -> &ParsedSource {
        &self.syntax_ast
    }

    fn included(&self) -> &Vec<SourceFile> {
        self.included.as_ref()
    }

    fn print_syntax_errors(&self) {
        print_compiler_errors(self.syntax_ast().errors(), &self.file_path);
        for source_file in self.included().iter() {
            source_file.print_syntax_errors()
        }
    }
}


impl SourceFile {
    pub fn new<F: Normalizeable>(file_path: F, syntax_ast: ParsedSource, included: Vec<SourceFile>)
                                 -> SourceFile {
        SourceFile { file_path: file_path.normalize(), syntax_ast, included }
    }


    pub fn file_path(&self) -> &PathBuf {
        &self.file_path
    }
}

fn read_source_file(file_path: &PathBuf) -> String {
    match fs::read_to_string(file_path) {
        Ok(source) => source,
        Err(err) => panic!("Unable to read OpenQASM source file '{}': {}", file_path.display(), err)
    }
}


/// Read source from `file_path` and parse to the syntactic AST.
/// Parse and store included files recursively.
pub fn parse_source_file(file_path: &PathBuf) -> SourceFile {
    let source = read_source_file(file_path);
    let syntax_ast: ParsedSource = synast::SourceFile::parse(source.as_str());
    let included = parse_included_files(&syntax_ast);
    SourceFile::new(file_path, syntax_ast, included)
}

// FIXME: prevent a file from including itself. Then there are two-file cycles, etc.
// FIXME: I want to disable filter_map_identity globally, but there is no option for clippy.toml
#[allow(clippy::filter_map_identity)]
///  Recursively parse any files `include`d in the program `syntax_ast`.
pub fn parse_included_files(syntax_ast: &ParsedSource) -> Vec<SourceFile> {
    syntax_ast.tree().statements()
        .map(|parse_item|
             match parse_item {
                 synast::Stmt::Item(synast::Item::Include(include)) => {
                     let file: synast::FilePath = include.file().unwrap();
                     let file_path = file.to_string().unwrap();
                     Some(parse_source_file(&PathBuf::from(file_path)))
                 },
                 _ => None,
             }
        ).filter_map(|x| x).collect::<Vec<_>>()
}


// Source that may not come from a file.
// `fake_file_path` is only printed, never expected to be path.
#[derive(Clone, Debug)]
pub struct SourceString {
    source: String,
    fake_file_path: Option<String>, // Typical name is "no file".
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

    fn print_syntax_errors(&self) {
        let file_path = self.fake_file_path().unwrap_or("no file");
        inner_print_compiler_errors(self.syntax_ast().errors(), file_path, self.source());
        for source_file in self.included().iter() {
            source_file.print_syntax_errors()
        }
    }
}

impl SourceString {
    pub fn fake_file_path(&self) -> Option<&str> {
        self.fake_file_path.as_deref()
    }

    pub fn source(&self) -> &str {
        &self.source
    }

}

/// Read source from `file_path` and parse to the syntactic AST.
/// Parse and store included files recursively.
pub fn parse_source_string<T: ToString>(source: T, fake_file_path: Option<&str>) -> SourceString {
    let source = source.to_string();
    let fake_file_path = fake_file_path.map(|x| x.to_string());
    let syntax_ast: ParsedSource = synast::SourceFile::parse(source.as_str());
    let included = parse_included_files(&syntax_ast);
    SourceString {source, fake_file_path, syntax_ast, included }
}
