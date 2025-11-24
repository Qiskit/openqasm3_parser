use std::fs;
use std::path::{Path, PathBuf};

use oq3_semantics::asg::Program;
use oq3_syntax::ast as synast;

// Root for snippet discovery (relative to this crate)
const ROOT: &str = "tests/snippets";

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
struct Expectations {
    lex: Option<Expect>,
    parse: Option<Expect>,
    sema: Option<Expect>,
}

fn parse_expectations(src: &str) -> Expectations {
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
fn run_lex(src: &str) -> (bool, usize, String) {
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
fn run_parser_from_path(path: &Path) -> (bool, usize, String, bool, String) {
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
fn run_sema_from_path(path: &Path) -> (bool, usize, String, bool, String) {
    use std::panic::{catch_unwind, AssertUnwindSafe};

    let mut asg_dump = String::new();
    let mut ok = false;
    let mut diag_count = 0usize;
    let mut panicked = false;
    let mut panic_msg = String::new();

    let res = catch_unwind(AssertUnwindSafe(|| {
        let pr = oq3_semantics::syntax_to_semantics::parse_source_file(path);
        let any = pr.any_semantic_errors();
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

fn snapshot_for(
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
        writeln!(&mut sema_snap, "panic: {}", sema.3).ok();
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

// fn snapshot_forold(
//     id: &str,
//     src: &str,
//     exp: &Expectations,
//     lex: &(bool, usize, String),
//     parse: &(bool, usize, String, bool, String),
//     sema: &(bool, usize, bool, String),
// ) -> (String, String, String) {
//     use std::fmt::Write as _;
//     let mut s = String::new();
//     writeln!(&mut s, "id: {}", id).ok();

//     writeln!(&mut s, "--- expectations ---").ok();
//     writeln!(&mut s, "lex:   {:?}", exp.lex.unwrap_or(Expect::Skip)).ok();
//     writeln!(&mut s, "parse: {:?}", exp.parse.unwrap_or(Expect::Skip)).ok();
//     writeln!(&mut s, "sema:  {:?}", exp.sema.unwrap_or(Expect::Skip)).ok();

//     let mut lex_snap = String::new();
//     let _ = writeln!(&mut lex_snap, "id: {}", id);
//     let _ = writeln!(&mut lex_snap, "--- lexer ---");
//     let _ = writeln!(&mut lex_snap, "ok: {}", lex.0);
//     let _ = writeln!(&mut lex_snap, "errors: {}", lex.1);
//     lex_snap.push_str(&lex.2);

//     let mut parse_snap = String::new();
//     let _ = writeln!(&mut parse_snap, "id: {}", id);
//     let _ = writeln!(&mut parse_snap, "--- parser ---");
//     let _ = writeln!(&mut parse_snap, "ok: {}", parse.0);
//     let _ = writeln!(&mut parse_snap, "panicked: {}", parse.3);
//     if parse.3 && !parse.4.is_empty() { let _ = writeln!(&mut parse_snap, "panic: {}", parse.4); }
//     let _ = writeln!(&mut parse_snap, "errors: {}", parse.1);
//     let _ = writeln!(&mut parse_snap, "--- ast ---");
//     if parse.2.is_empty() { let _ = writeln!(&mut parse_snap, "(no ast)"); } else { parse_snap.push_str(&parse.2); }

//     let mut sema_snap = String::new();
//     let _ = writeln!(&mut sema_snap, "id: {}", id);
//     let _ = writeln!(&mut sema_snap, "--- sema ---");
//     let _ = writeln!(&mut sema_snap, "ok: {}", sema.0);
//     let _ = writeln!(&mut sema_snap, "panicked: {}", sema.2);
//     if sema.2 && !sema.3.is_empty() { let _ = writeln!(&mut sema_snap, "panic: {}", sema.3); }
//     let _ = writeln!(&mut sema_snap, "errors: {}", sema.1);
//     let _ = writeln!(&mut sema_snap, "--- asg ---");
//     if sema.3.is_empty() { let _ = writeln!(&mut sema_snap, "(no asg)"); } else { sema_snap.push_str(&sema.3); }

//     (lex_snap, parse_snap, sema_snap)
// }

// Ok: no diags and no panic; Diag: diags >= 1 and no panic; Panic: stage panicked; Todo/Skip as defined
fn apply_expect(stage: Option<Expect>, diag_count: usize, panicked: bool) -> Result<(), String> {
    match stage {
        Some(Expect::Skip) | None => Ok(()),
        Some(Expect::Panic) => {
            if panicked {
                Ok(())
            } else {
                Err("expected panic, but stage completed".into())
            }
        }
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

fn rel_id(path: &Path) -> String {
    let root_abs = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(ROOT);
    path.strip_prefix(&root_abs)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

fn check(path: &Path) -> datatest_stable::Result<()> {
    let id = rel_id(path);
    let src = fs::read_to_string(path)?;
    let exp = parse_expectations(&src);

    // Stages
    let lex = run_lex(&src);
    let parse = run_parser_from_path(path);
    let sema = run_sema_from_path(path);

    // Expectations
    if let Err(msg) = apply_expect(exp.lex, lex.1, false) {
        return Err(format!("lex: {}", msg).into());
    }
    if let Err(msg) = apply_expect(exp.parse, parse.1, parse.3) {
        return Err(format!("parse: {}", msg).into());
    }
    if let Err(msg) = apply_expect(exp.sema, sema.1, sema.3) {
        return Err(format!("sema: {}", msg).into());
    }

    let (lex_snap, parse_snap, sema_snap) = snapshot_for(&id, &src, &exp, &lex, &parse, &sema);

    insta::assert_snapshot!(format!("{id}-lex"), lex_snap);
    insta::assert_snapshot!(format!("{id}-parse"), parse_snap);
    insta::assert_snapshot!(format!("{id}-sema"), sema_snap);

    Ok(())
}

// One test per .qasm under tests/snippets (recursive). Requires harness=false in Cargo.toml.

#[cfg(not(target_os = "windows"))]
datatest_stable::harness! {
    { test = check, root = ROOT, pattern = r".*\.qasm$" },
}
#[cfg(target_os = "windows")]
#[test]
fn snapshots_skipped_on_windows() {
    eprintln!("Skipping snippet snapshots on Windows");
}
