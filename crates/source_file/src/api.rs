// It might be nice to put these functions in lib.rs.
// But they are also used in this crate, so we put them here.

use ariadne::Config;
use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use std::ops::Range;
use std::path::PathBuf;

use oq3_syntax::ast as synast; // Syntactic AST

use crate::source_file::{
    expand_path, parse_included_files, parse_source_and_includes, range_to_span, read_source_file,
    ErrorTrait, Normalizeable, ParsedSource, SourceFile, SourceString,
};

/// Read source from `file_path` and parse to the syntactic AST.
/// Parse and store included files recursively.
pub fn parse_source_file(file_path: &PathBuf) -> SourceFile {
    let full_path = expand_path(file_path).normalize();
    let (syntax_ast, included) = parse_source_and_includes(read_source_file(&full_path).as_str());
    SourceFile::new(full_path, syntax_ast, included)
}

/// Read source from `file_path` and parse to the syntactic AST.
/// Parse and store included files recursively.
pub fn parse_source_string<T: ToString>(source: T, fake_file_path: Option<&str>) -> SourceString {
    let source = source.to_string();
    let (syntax_ast, included) = parse_source_and_includes(source.as_str());
    let fake_file_path = PathBuf::from(fake_file_path.unwrap_or("no file"));
    SourceString::new(source, fake_file_path, syntax_ast, included)
}

/// Print compiler errors. Diagnostics include text take from `source`.
/// The file `info_file_path` is only used for printing error messages. In particular,
/// it does not need to correspond to an existing file. In case `info_file_path` is indeed
/// the source file, then `source` is read before calling this function.
pub fn inner_print_compiler_errors<T: ErrorTrait>(
    errors: &[T],
    info_file_path: &PathBuf,
    source: &str,
) {
    let file_path_str = info_file_path.as_os_str().to_str().unwrap();
    for err in errors.iter() {
        let err_string = err.message();
        let err_span = range_to_span(&err.range());
        report_error(&err_string, &err_span, file_path_str, source);
        println!();
    }
}

pub fn print_compiler_errors<T: ErrorTrait>(errors: &[T], file_path: &PathBuf) {
    // ariadne seems to want path only as &str, not PathBuf.
    let source = &read_source_file(file_path);
    inner_print_compiler_errors(errors, file_path, source);
}

pub fn report_error(message: &str, span: &Range<usize>, file_path: &str, source: &str) {
    let mut colors = ColorGenerator::new();
    // Generate & choose some colours for each of our elements
    let a = colors.next();
    // `offset` is a zero-indexed character offset from beginning of file.
    // Here `offset` is character index of start of error.
    let offset = span.start;
    Report::build(ReportKind::Error, file_path, offset)
        //        .with_code(3)
        .with_message(message)
        .with_config(Config::default().with_compact(true))
        .with_label(
            Label::new((file_path, span.clone()))
                .with_message("Near this point")
                .with_color(a),
        )
        .finish()
        .print((file_path, Source::from(source)))
        .unwrap();
}
