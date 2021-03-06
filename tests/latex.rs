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

#[test]
fn test_latex7() {
    assert_snapshot!(run(
        "(A → A → B) → (B → B → C) → (C → C → D) → (D → D → E) → (E → E → F) → A → F"
    ));
}

#[test]
fn test_latex8() {
    assert_snapshot!(run("¬¬(A ∨ ¬A)"));
}

#[test]
fn test_latex9() {
    assert_snapshot!(run("(A ⇔ ¬A) → ⊥"));
}

#[test]
fn test_latex10() {
    assert_snapshot!(run("A ∨ ¬ A"));
}

#[test]
fn test_latex11() {
    assert_snapshot!(run("¬¬A → A"));
}

#[test]
fn test_latex12() {
    assert_snapshot!(run("(A → B) ∨ (B → A)"));
}

#[test]
fn test_latex13() {
    assert_snapshot!(run("A → B"));
}

#[test]
fn test_latex14() {
    assert_snapshot!(run("(A → B) → (¬A ∨ B)"));
}

#[test]
fn test_latex15() {
    assert_snapshot!(run("((A → B) → A) → A"));
}

#[test]
fn test_latex16() {
    assert_snapshot!(run("A -> A /\\ A /\\ A /\\ A /\\ A /\\ A"));
}

fn run(s: &str) -> String {
    let output = Command::new("cargo")
        .args(&["run", "--", "-e", s, "--latex"])
        .output()
        .unwrap();
    assert!(output.status.success());
    String::from_utf8(output.stdout).unwrap()
}
