// Copyright contributors to the openqasm-parser project
// SPDX-License-Identifier: Apache-2.0

//! This was copied from an external crate. It would be better to use the
//! external crate. But it has a bug-- a hard coded path that requires the
//! source to be at this path.
//!
//! OQ3 uses the same sourcegen routines that rust-analyer does. This reduces
//! boiler plate somewhat.
//!
//! Things like feature documentation or assist tests are implemented by
//! processing rust-analyzer's own source code and generating the appropriate
//! output. See `sourcegen_` tests in various crates.
//!
//! This crate contains utilities to make this kind of source-gen easy.

use std::{
    fmt, fs, mem,
    path::{Path, PathBuf},
};

use xshell::{cmd, Shell};

// I think list_rust_files and list_files are only used to support
// extracting tests from comments.

#[allow(unused)]
pub fn list_rust_files(dir: &Path) -> Vec<PathBuf> {
    let mut res = list_files(dir);
    res.retain(|it| {
        it.file_name()
            .unwrap_or_default()
            .to_str()
            .unwrap_or_default()
            .ends_with(".rs")
    });
    res
}

#[allow(unused)]
pub fn list_files(dir: &Path) -> Vec<PathBuf> {
    let mut res = Vec::new();
    let mut work = vec![dir.to_path_buf()];
    while let Some(dir) = work.pop() {
        for entry in dir.read_dir().unwrap() {
            let entry = entry.unwrap();
            let file_type = entry.file_type().unwrap();
            let path = entry.path();
            let is_hidden = path
                .file_name()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default()
                .starts_with('.');
            if !is_hidden {
                if file_type.is_dir() {
                    work.push(path);
                } else if file_type.is_file() {
                    res.push(path);
                }
            }
        }
    }
    res
}

#[derive(Clone)]
pub struct CommentBlock {
    pub id: String,
    pub line: usize,
    pub contents: Vec<String>,
    is_doc: bool,
}

impl CommentBlock {
    #[allow(unused)]
    pub fn extract(tag: &str, text: &str) -> Vec<CommentBlock> {
        assert!(tag.starts_with(char::is_uppercase));

        let tag = format!("{tag}:");
        let mut blocks = CommentBlock::extract_untagged(text);
        blocks.retain_mut(|block| {
            let first = block.contents.remove(0);
            let Some(id) = first.strip_prefix(&tag) else {
                return false;
            };

            if block.is_doc {
                panic!("Use plain (non-doc) comments with tags like {tag}:\n    {first}");
            }

            block.id = id.trim().to_string();
            true
        });
        blocks
    }

    #[allow(unused)]
    pub fn extract_untagged(text: &str) -> Vec<CommentBlock> {
        let mut res = Vec::new();

        let lines = text.lines().map(str::trim_start);

        let dummy_block = CommentBlock {
            id: String::new(),
            line: 0,
            contents: Vec::new(),
            is_doc: false,
        };
        let mut block = dummy_block.clone();
        for (line_num, line) in lines.enumerate() {
            match line.strip_prefix("//") {
                Some(mut contents) => {
                    if let Some('/' | '!') = contents.chars().next() {
                        contents = &contents[1..];
                        block.is_doc = true;
                    }
                    if let Some(' ') = contents.chars().next() {
                        contents = &contents[1..];
                    }
                    block.contents.push(contents.to_string());
                }
                None => {
                    if !block.contents.is_empty() {
                        let block = mem::replace(&mut block, dummy_block.clone());
                        res.push(block);
                    }
                    block.line = line_num + 2;
                }
            }
        }
        if !block.contents.is_empty() {
            res.push(block);
        }
        res
    }
}

#[derive(Debug)]
pub struct Location {
    pub file: PathBuf,
    pub line: usize,
}

// FIXME: URL for r-a is hardcoded here.
// My guess is:
// This is meant to display the location in the code of a test failure.
// The tests are extracted and written and run elsewhere, so the location in
// source is recorded so that the dev can find them.
impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let path = self
            .file
            .strip_prefix(project_root())
            .unwrap()
            .display()
            .to_string();
        let path = path.replace('\\', "/");
        let name = self.file.file_name().unwrap();
        write!(
            f,
            "https://github.com/rust-lang/rust-analyzer/blob/master/{}#L{}[{}]",
            path,
            self.line,
            name.to_str().unwrap()
        )
    }
}

#[allow(unused)]
fn ensure_rustfmt(sh: &Shell) {
    let version = cmd!(sh, "rustup run stable rustfmt --version")
        .read()
        .unwrap_or_default();
    if !version.contains("stable") {
        panic!(
            "Failed to run rustfmt from toolchain 'stable'. \
                 Please run `rustup component add rustfmt --toolchain stable` to install it.",
        );
    }
}

#[allow(unused)]
pub fn reformat(text: String) -> String {
    let sh = Shell::new().unwrap();
    ensure_rustfmt(&sh);
    let rustfmt_toml = project_root().join("rustfmt.toml");
    let mut stdout = cmd!(
        sh,
        "rustup run stable rustfmt --config-path {rustfmt_toml} --config fn_single_line=true"
    )
    .stdin(text)
    .read()
    .unwrap();
    if !stdout.ends_with('\n') {
        stdout.push('\n');
    }
    stdout
}

#[allow(unused)]
pub fn add_preamble(generator: &'static str, mut text: String) -> String {
    let preamble = format!("//! Generated by `{generator}`, do not edit by hand.\n\n");
    text.insert_str(0, &preamble);
    text
}

/// Checks that the `file` has the specified `contents`. If that is not the
/// case, updates the file and then fails the test.
#[allow(unused)]
pub fn ensure_file_contents(file: &Path, contents: &str) {
    if let Ok(old_contents) = fs::read_to_string(file) {
        if normalize_newlines(&old_contents) == normalize_newlines(contents) {
            // File is already up to date.
            return;
        }
    }

    let display_path = file.strip_prefix(project_root()).unwrap_or(file);
    eprintln!(
        "\n\x1b[31;1merror\x1b[0m: {} was not up-to-date, updating\n",
        display_path.display()
    );
    if std::env::var("CI").is_ok() {
        eprintln!("    NOTE: run `cargo test` locally and commit the updated files\n");
    }
    if let Some(parent) = file.parent() {
        let _ = fs::create_dir_all(parent);
    }
    fs::write(file, contents).unwrap();
    panic!("some file was not up to date and has been updated, simply re-run the tests");
}

#[allow(unused)]
fn normalize_newlines(s: &str) -> String {
    s.replace("\r\n", "\n")
}

pub fn project_root() -> PathBuf {
    let dir = env!("CARGO_MANIFEST_DIR");
    let res = PathBuf::from(dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_owned();
    assert!(res.join("dummy_triagebot.toml").exists()); // in rust-analyzer just "triagebot.toml".
    res
}
