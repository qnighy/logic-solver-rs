use insta::assert_snapshot;
use std::process::Command;

#[test]
fn test_latex1() {
    assert_snapshot!(run("A"));
}

#[test]
fn test_latex2() {
    assert_snapshot!(run("A → A"));
}

#[test]
fn test_latex3() {
    assert_snapshot!(run("A → B → A"));
}

#[test]
fn test_latex4() {
    assert_snapshot!(run("(A → B → A) ∧ (A ∨ B → B ∨ A)"));
}

#[test]
fn test_latex5() {
    assert_snapshot!(run("(A → B → A) ∧ (A ∨ B → B)"));
}

#[test]
fn test_latex6() {
    assert_snapshot!(run("⇒"));
}

fn run(s: &str) -> String {
    let output = Command::new("cargo")
        .args(&["run", "--", "-e", s, "--latex"])
        .output()
        .unwrap();
    assert!(output.status.success());
    String::from_utf8(output.stdout).unwrap()
}
