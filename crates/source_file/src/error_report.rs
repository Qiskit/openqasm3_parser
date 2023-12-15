use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use ariadne::Config;
use std::ops::Range;

// pub fn report_error(message: &str, span: &Range<usize>, file_path: &str, source: &str) {
//     let mut colors = ColorGenerator::new();
//     // Generate & choose some colours for each of our elements
//     let a = colors.next();
//     // `offset` is a zero-indexed character offset from beginning of file.
//     // Here `offset` is character index of start of error.
//     let offset = span.start;
//     Report::build(ReportKind::Error, file_path, offset)
// //        .with_code(3)
//         .with_message(message)
//         .with_config(Config::default().with_compact(true))
//         .with_label(
//             Label::new((file_path, span.clone()))
//                 .with_message("Near this point")
//                 .with_color(a),
//         )
//         .finish()
//         .print((file_path, Source::from(source)))
//         .unwrap();
// }


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
