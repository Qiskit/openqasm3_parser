use oq3_syntax::{ast, AstNode, SourceFile};

#[test]
fn single_statement_if_branches_are_distinct() {
    let parse = SourceFile::parse("if (true) x q; else z q;");
    assert!(parse.errors().is_empty());

    let if_stmt = match parse.tree().statements().next() {
        Some(ast::Stmt::IfStmt(if_stmt)) => if_stmt,
        _ => panic!("expected an if statement"),
    };

    let then_branch = if_stmt.then_branch_stmt().unwrap();
    let else_branch = if_stmt.else_branch_stmt().unwrap();

    // Regression test: the else accessor previously returned the then branch.
    assert_eq!(then_branch.syntax().text().to_string(), "x q;");
    assert_eq!(else_branch.syntax().text().to_string(), "z q;");
}
