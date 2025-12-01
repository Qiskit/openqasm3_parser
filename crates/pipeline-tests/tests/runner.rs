// Only build the real suite on non-Windows
#[cfg(not(target_os = "windows"))]
mod suite {

    use std::path::{Path, PathBuf};

    use oq3_semantics::asg::Program;
    use oq3_syntax::ast as synast;

    // Root for snippet discovery (relative to this crate)
    pub const ROOT: &str = "tests/snippets";

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Expect {
        /// Stage must complete with no diagnostics.
        Ok,
        /// Stage must complete and produce diagnostics (≥1); snapshot verifies content.
        Diag,
        /// Stage must complete and produce diagnostics (>=1); snapshot verifies content.
        Todo,
        /// Stage is expected to panic; harness catches the unwind.
        Panic,
        /// Do not assert this stage (optionally skip running it).
        Skip,
    }

    #[derive(Default, Debug)]
    pub(crate) struct Expectations {
        pub(crate) lex: Option<Expect>,
        pub(crate) parse: Option<Expect>,
        pub(crate) sema: Option<Expect>,
    }

    pub fn strip_header_body(src: &str) -> &str {
        let mut offset = 0usize;
        let mut seen = 0usize;

        for line in src.split_inclusive('\n') {
            if seen >= 16 {
                break;
            }
            let l = line.trim_start();
            if !l.starts_with("//") {
                break;
            }
            let t = l.trim_start_matches("//").trim();
            let mut parts = t.splitn(2, ':');
            let stage = parts.next().unwrap_or_default().trim();
            let tag = parts.next().unwrap_or_default().trim();

            let is_stage = matches!(stage, "lex" | "parse" | "sema");
            let is_tag = matches!(tag, "ok" | "diag" | "todo" | "panic" | "skip");

            if is_stage && is_tag {
                offset += line.len();
                seen += 1;
                continue;
            } else {
                break;
            }
        }

        &src[offset..]
    }

    use tempfile::{Builder, NamedTempFile};

    use std::io::{self, Write};

    pub struct TempQasm {
        file: NamedTempFile, // auto-deletes on drop
    }

    impl TempQasm {
        pub fn new(body: &str) -> io::Result<Self> {
            let mut file = Builder::new().prefix("oq3-").suffix(".qasm").tempfile()?; // unique random name
            file.write_all(body.as_bytes())?;
            file.flush()?;
            Ok(Self { file })
        }

        pub fn path(&self) -> &Path {
            self.file.path()
        }
    }

    // Enforce: if parse tag != ok, sema tag must be skip/panic (or absent).
    pub fn validate_expectations(exp: &Expectations) -> Result<(), String> {
        let parse_ok = matches!(exp.parse, Some(Expect::Ok));
        let sema_is_skip = matches!(exp.sema, Some(Expect::Skip | Expect::Panic) | None);
        if !parse_ok && !sema_is_skip {
            return Err(format!(
                 "invalid expectations: parse={:?}, sema={:?}. When parse is not \"ok\", sema must be \"skip\" or \"panic\".",
                 exp.parse, exp.sema
             ));
        }
        Ok(())
    }

    pub(crate) fn parse_expectations(src: &str) -> Expectations {
        let mut e = Expectations::default();
        for line in src.lines().take(16) {
            // Require comment lines with exact lowercase tags
            // // lex: ok|diag|todo|panic|skip
            let l = line.trim_start();
            if !l.starts_with("//") {
                continue;
            }
            let l = l.trim_start_matches("//").trim();

            let mut parts = l.splitn(2, ':');
            let stage = match parts.next() {
                Some(s) => s.trim(),
                None => continue,
            };
            let tag = match parts.next() {
                Some(t) => t.trim(),
                None => continue,
            };

            let val = match tag {
                "ok" => Some(Expect::Ok),
                "diag" => Some(Expect::Diag),
                "todo" => Some(Expect::Todo),
                "panic" => Some(Expect::Panic),
                "skip" => Some(Expect::Skip),
                _ => None,
            };
            if val.is_none() {
                continue;
            }

            match stage {
                "lex" => e.lex = val,
                "parse" => e.parse = val,
                "sema" => e.sema = val,
                _ => {}
            }
        }
        e
    }

    fn escape(s: &str) -> String {
        let mut out = String::with_capacity(s.len());
        for ch in s.chars() {
            match ch {
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\t' => out.push_str("\\t"),
                _ => out.push(ch),
            }
        }
        out
    }

    // Lexer stage: token stream + oq3_parser::LexedStr diagnostics summary
    // We must not call `tokenize` directly. Diagnostic errors are hidden in struct fields.
    // `LexedStr` converts them to stored errors that can be retrieved.
    pub(crate) fn run_lex(src: &str) -> (bool, usize, String) {
        let lexed = oq3_parser::LexedStr::new(src);
        let diag_count = if lexed.errors_is_empty() { 0 } else { 1 };

        // Token dump
        let mut pos = 0usize;
        let mut dump = String::new();
        for (i, tok) in oq3_lexer::tokenize(src).enumerate() {
            let len = tok.len as usize;
            let end = pos + len;
            if let Some(text) = src.get(pos..end) {
                use std::fmt::Write as _;
                let _ = writeln!(
                    &mut dump,
                    "[{}] {:?} \"{}\" @{}..{}",
                    i,
                    tok.kind,
                    escape(text),
                    pos,
                    end
                );
            }
            pos = end;
        }

        let ok = diag_count == 0;
        (ok, diag_count, dump)
    }

    // Pretty AST dump
    fn dump_ast(root: &synast::SourceFile) -> String {
        use synast::AstNode;
        let mut out = String::new();
        for item in root.syntax().descendants() {
            use std::fmt::Write as _;
            let _ = writeln!(&mut out, "{item:?}: {item:}");
        }
        out
    }

    /// Print the ASG using the `Debug` trait.
    pub fn dump_asg(root: &Program) -> String {
        let mut out = String::new();
        for stmt in root.iter() {
            use std::fmt::Write as _;
            let _ = writeln!(&mut out, "{stmt:?}\n");
        }
        out
    }

    // Parser via oq3_source_file; collect syntax diagnostics and AST text; catch panics
    pub(crate) fn run_parser_from_path(path: &Path) -> (bool, usize, String, bool, String) {
        use std::panic::{catch_unwind, AssertUnwindSafe};

        let mut ok = false;
        let mut diag_count = 0usize;
        let mut ast_dump = String::new();
        let mut panicked = false;
        let mut panic_msg = String::new();

        let res = catch_unwind(AssertUnwindSafe(|| {
            let parsed = oq3_source_file::parse_source_file(path);

            if let Some(ast) = parsed.ast() {
                let errs = ast.errors();
                diag_count = errs.len();
                let have_tree = ast.have_parse();
                ok = have_tree && diag_count == 0;
                if have_tree {
                    ast_dump = dump_ast(&ast.tree());
                }
            } else {
                // e.g., read/lex failed in wrapper
                diag_count = 1;
            }
        }));

        if let Err(p) = res {
            panicked = true;
            panic_msg = if let Some(s) = p.downcast_ref::<&'static str>() {
                s.to_string()
            } else if let Some(s) = p.downcast_ref::<String>() {
                s.clone()
            } else {
                "panic (unknown payload)".into()
            };
        }
        (ok, diag_count, ast_dump, panicked, panic_msg)
    }

    // Semantics via oq3_semantics; catch panics
    pub(crate) fn run_sema_from_path(path: &Path) -> (bool, usize, String, bool, String) {
        use std::panic::{catch_unwind, AssertUnwindSafe};

        let mut asg_dump = String::new();
        let mut ok = false;
        let mut diag_count = 0usize;
        let mut panicked = false;
        let mut panic_msg = String::new();

        let res = catch_unwind(AssertUnwindSafe(|| {
            let pr = oq3_semantics::syntax_to_semantics::parse_source_file(path);
            let any = pr.any_errors();
            ok = !any;
            // This is wrong. should be the number of semantic errors
            diag_count = if any { 1 } else { 0 };
            asg_dump = dump_asg(pr.program());
        }));

        if let Err(p) = res {
            panicked = true;
            panic_msg = if let Some(s) = p.downcast_ref::<&'static str>() {
                s.to_string()
            } else if let Some(s) = p.downcast_ref::<String>() {
                s.clone()
            } else {
                "panic (unknown payload)".into()
            };
        }

        (ok, diag_count, asg_dump, panicked, panic_msg)
    }

    /// Treat Panic same as Skip for gating
    /// If we were to run this stage, the result would be a panic.
    /// We dont do anything with the information returned and don't want to.
    /// So, we simply skip this test.
    pub fn should_run(stage: Option<Expect>) -> bool {
        !matches!(stage, Some(Expect::Skip | Expect::Panic) | None)
    }

    pub(crate) fn snapshot_for(
        id: &str,
        src: &str,
        exp: &Expectations,
        lex: &(bool, usize, String),
        parse: &(bool, usize, String, bool, String),
        sema: &(bool, usize, String, bool, String),
    ) -> (String, String, String) {
        use std::fmt::Write as _;

        // --- lexer ---
        let mut lex_snap = String::new();
        writeln!(&mut lex_snap, "id: {}", id).ok();
        writeln!(
            &mut lex_snap,
            "expect-lex: {:?}",
            exp.lex.unwrap_or(Expect::Skip)
        )
        .ok();
        writeln!(&mut lex_snap, "--- source ---").ok();
        lex_snap.push_str(src);
        if !src.ends_with('\n') {
            lex_snap.push('\n');
        }
        writeln!(&mut lex_snap, "--- lexer ---").ok();
        writeln!(&mut lex_snap, "ok: {}", lex.0).ok();
        writeln!(&mut lex_snap, "errors: {}", lex.1).ok();
        lex_snap.push_str(&lex.2);

        // --- parser ---
        let mut parse_snap = String::new();
        writeln!(&mut parse_snap, "id: {}", id).ok();
        writeln!(
            &mut parse_snap,
            "expect-parse: {:?}",
            exp.parse.unwrap_or(Expect::Skip)
        )
        .ok();
        writeln!(&mut parse_snap, "--- parser ---").ok();
        writeln!(&mut parse_snap, "ok: {}", parse.0).ok();
        writeln!(&mut parse_snap, "panicked: {}", parse.3).ok();
        if parse.3 && !parse.4.is_empty() {
            writeln!(&mut parse_snap, "panic: {}", parse.4).ok();
        }
        writeln!(&mut parse_snap, "errors: {}", parse.1).ok();
        writeln!(&mut parse_snap, "--- ast ---").ok();
        if parse.2.is_empty() {
            writeln!(&mut parse_snap, "(no ast)").ok();
        } else {
            parse_snap.push_str(&parse.2);
        }

        // --- sema ---
        let mut sema_snap = String::new();
        writeln!(&mut sema_snap, "id: {}", id).ok();
        writeln!(
            &mut sema_snap,
            "expect-sema: {:?}",
            exp.sema.unwrap_or(Expect::Skip)
        )
        .ok();
        writeln!(&mut sema_snap, "--- sema ---").ok();
        writeln!(&mut sema_snap, "ok: {}", sema.0).ok();
        writeln!(&mut sema_snap, "panicked: {}", sema.3).ok();
        if sema.3 && !sema.4.is_empty() {
            writeln!(&mut sema_snap, "panic: {}", sema.4).ok();
        }
        writeln!(&mut sema_snap, "errors: {}", sema.1).ok();
        let _ = writeln!(&mut sema_snap, "--- asg ---");
        if sema.2.is_empty() {
            let _ = writeln!(&mut sema_snap, "(no asg)");
        } else {
            sema_snap.push_str(&sema.2);
        }

        (lex_snap, parse_snap, sema_snap)
    }

    pub(crate) fn apply_expect(
        stage: Option<Expect>,
        diag_count: usize,
        panicked: bool,
    ) -> Result<(), String> {
        match stage {
            Some(Expect::Skip) | Some(Expect::Panic) | None => Ok(()),
            Some(Expect::Ok) => {
                if panicked {
                    Err("expected ok, but stage panicked".into())
                } else if diag_count > 0 {
                    Err(format!("expected no diagnostics, got {}", diag_count))
                } else {
                    Ok(())
                }
            }
            Some(Expect::Diag) => {
                if panicked {
                    Err("expected diagnostics, but stage panicked".into())
                } else if diag_count == 0 {
                    Err("expected diagnostics, got none".into())
                } else {
                    Ok(())
                }
            }
            Some(Expect::Todo) => {
                if panicked {
                    Err("expected completion (todo), but stage panicked".into())
                } else {
                    Ok(())
                }
            }
        }
    }

    pub(crate) fn rel_id(path: &Path) -> String {
        let root_abs = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(ROOT);
        path.strip_prefix(&root_abs)
            .unwrap_or(path)
            .to_string_lossy()
            .replace('\\', "/")
    }

    // One test per .qasm under tests/snippets (recursive). Requires harness=false in Cargo.toml.
}

#[cfg(not(target_os = "windows"))]
use std::path::Path;

#[cfg(not(target_os = "windows"))]
pub fn check(path: &Path) -> datatest_stable::Result<()> {
    use std::fs;

    let id = suite::rel_id(path);
    let src = fs::read_to_string(path)?;

    let exp = suite::parse_expectations(&src);

    // Validate: if parse != ok, sema must be skip (or absent)
    if let Err(msg) = suite::validate_expectations(&exp) {
        return Err(format!("{}: {}", id, msg).into());
    }

    // Headerless body
    let body = suite::strip_header_body(&src);

    // Gating
    let run_lex = suite::should_run(exp.lex);
    let run_parse = suite::should_run(exp.parse);
    let run_sema = suite::should_run(exp.sema);

    // Parser/sema temp file (only if needed)
    let temp = if run_parse || run_sema {
        Some(suite::TempQasm::new(body)?)
    } else {
        None
    };

    // Stages (lex from body string; parse/sema from temp file)
    let lex = if run_lex {
        suite::run_lex(body)
    } else {
        (false, 0, String::new())
    };
    let parse = if run_parse {
        suite::run_parser_from_path(temp.as_ref().unwrap().path())
    } else {
        (false, 0, String::new(), false, String::new())
    };
    let sema = if run_sema {
        suite::run_sema_from_path(temp.as_ref().unwrap().path())
    } else {
        (false, 0, String::new(), false, String::new())
    };

    // Snapshots use the headerless body so header edits don't affect stage output
    let (lex_snap, parse_snap, sema_snap) =
        suite::snapshot_for(&id, body, &exp, &lex, &parse, &sema);
    if run_lex {
        insta::assert_snapshot!(format!("{id}-lex"), lex_snap);
    }
    if run_parse {
        insta::assert_snapshot!(format!("{id}-parse"), parse_snap);
    }
    if run_sema {
        insta::assert_snapshot!(format!("{id}-sema"), sema_snap);
    }

    // Expectations (unchanged)
    let mut errs = Vec::new();
    if let Err(msg) = suite::apply_expect(exp.lex, lex.1, false) {
        errs.push(format!("lex: {msg}"));
    }
    if let Err(msg) = suite::apply_expect(exp.parse, parse.1, parse.3) {
        errs.push(format!("parse: {msg}"));
    }
    if let Err(msg) = suite::apply_expect(exp.sema, sema.1, sema.3) {
        errs.push(format!("sema: {msg}"));
    }
    if !errs.is_empty() {
        return Err(errs.join("; ").into());
    }

    Ok(())
}

#[cfg(not(target_os = "windows"))]
datatest_stable::harness! {
    { test = check, root = suite::ROOT, pattern = r".*\.qasm$" },
}

#[cfg(target_os = "windows")]
fn main() {
    eprintln!("Skipping snippet snapshots on Windows");
}
