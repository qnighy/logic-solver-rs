// TODO: some of the functions should eventually be rearranged as public APIs.

use crate::ipc::solve;
use crate::parsing::parse_prop;
use crate::prop::Env;
use crate::prop::{IdGen, Prop};

const PROPS: &[(&str, bool)] = &[
    ("A", false),
    ("A → A", true),
    ("A → A → A", true),
    ("(A → A) → A", false),
    ("A → B", false),
    ("A → B → A", true),
    ("B → A → A", true),
    ("((A → B) → A) → A", false),
    ("A → B → A ∧ B", true),
    ("A → A ∧ A", true),
    ("(A → A) ∧ A", false),
    ("A ∧ B → B ∧ A", true),
    ("A ∧ B ∧ C → B ∧ C ∧ A", true),
    ("(A ∧ B) ∧ C → A ∧ (B ∧ C)", true),
    ("(A ∧ B) ∧ C → A ∧ B ∧ C", true),
    ("A ∧ B ∧ C → B", true),
    ("(A → B) ∧ (A → C) ∧ (B → C → D) → A → D", true),
    ("⊤", true),
    ("⊤ → A", false),
    ("A ∨ (A → A)", true),
    ("A ∨ A → A", true),
    ("A ∨ B → B", false),
    ("A → A ∨ B", true),
    ("(A → B) ∨ (B → A)", false),
    ("A ∨ B ∨ C → A ∨ B", false),
    ("⊥", false),
    ("⊥ → A", true),
    ("A ∨ (A → ⊥)", false),
    ("A ∨ ⊥ → A", true),
    ("⊤ → ⊥", false),
    ("⊥ → ⊤", true),
    ("(A → A → ⊥) ∧ ((A → ⊥) → A) → ⊥", true),
    ("((A ∨ (A → ⊥)) → ⊥) → ⊥", true),
    ("((((A → B) → A) → A) → ⊥) → ⊥", true),
    ("(A ∨ (A → ⊥)) → ((A → ⊥) → ⊥) → A", true),
    ("(A ∧ B) ∨ C → (A ∨ C) ∧ (B ∨ C)", true),
    ("(A ∨ C) ∧ (B ∨ C) → (A ∧ B) ∨ C", true),
];

#[test]
fn test_provability() {
    for &(prop_str, provable_expect) in PROPS {
        let mut idgen = IdGen::new();
        let mut env = Env::new();
        let ast = parse_prop(prop_str).unwrap();
        let prop = Prop::from_ast(&mut idgen, &mut env, &ast);
        let provable = solve(&prop).is_some();
        assert_eq!(provable, provable_expect, "{}", prop_str);
    }
}
