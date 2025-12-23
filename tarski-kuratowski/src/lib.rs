use lib_fol::{Wff, WffBuilder, Formula, FormulaId};

fn to_nnf(
  builder: &mut WffBuilder,
  wff: &Wff,
  id: FormulaId,
  negated: bool,
) -> FormulaId {
  match wff.get_formula(id) {
    Some(Formula::RelationApplication(rel_id)) => {
      let base = builder.push_formula(Formula::RelationApplication(*rel_id));
      if negated {
        builder.push_formula(Formula::Negation(base))
      } else {
        base
      }
    }
    Some(Formula::Identity(left, right)) => {
      let base = builder.push_formula(Formula::Identity(*left, *right));
      if negated {
        builder.push_formula(Formula::Negation(base))
      } else {
        base
      }
    }
    Some(Formula::Negation(inner)) => {
      to_nnf(builder, wff, *inner, !negated)
    }
    Some(Formula::Disjunction(left, right)) => {
      if negated {
        let left = to_nnf(builder, wff, *left, true);
        let right = to_nnf(builder, wff, *right, true);
        builder.push_formula(Formula::Conjunction(left, right))
      } else {
        let left = to_nnf(builder, wff, *left, false);
        let right = to_nnf(builder, wff, *right, false);
        builder.push_formula(Formula::Disjunction(left, right))
      }
    }
    Some(Formula::Conjunction(left, right)) => {
      if negated {
        let left = to_nnf(builder, wff, *left, true);
        let right = to_nnf(builder, wff, *right, true);
        builder.push_formula(Formula::Disjunction(left, right))
      } else {
        let left = to_nnf(builder, wff, *left, false);
        let right = to_nnf(builder, wff, *right, false);
        builder.push_formula(Formula::Conjunction(left, right))
      }
    }
    Some(Formula::Conditional(left, right)) => {
      if negated {
        let left = to_nnf(builder, wff, *left, false);
        let right = to_nnf(builder, wff, *right, true);
        builder.push_formula(Formula::Conjunction(left, right))
      } else {
        let left = to_nnf(builder, wff, *left, true);
        let right = to_nnf(builder, wff, *right, false);
        builder.push_formula(Formula::Disjunction(left, right))
      }
    }
    Some(Formula::Biconditional(left, right)) => {
      if negated {
        let left_t = to_nnf(builder, wff, *left, false);
        let right_f = to_nnf(builder, wff, *right, true);
        let conj1 = builder.push_formula(Formula::Conjunction(left_t, right_f));

        let left_f = to_nnf(builder, wff, *left, true);
        let right_t = to_nnf(builder, wff, *right, false);
        let conj2 = builder.push_formula(Formula::Conjunction(left_f, right_t));

        builder.push_formula(Formula::Disjunction(conj1, conj2))
      } else {
        let left_f = to_nnf(builder, wff, *left, true);
        let right_t = to_nnf(builder, wff, *right, false);
        let disj1 = builder.push_formula(Formula::Disjunction(left_f, right_t));

        let left_t = to_nnf(builder, wff, *left, false);
        let right_f = to_nnf(builder, wff, *right, true);
        let disj2 = builder.push_formula(Formula::Disjunction(right_f, left_t));

        builder.push_formula(Formula::Conjunction(disj1, disj2))
      }
    }
    Some(Formula::Existential(var, inner)) => {
      if negated {
        let inner = to_nnf(builder, wff, *inner, true);
        builder.push_formula(Formula::Universal(*var, inner))
      } else {
        let inner = to_nnf(builder, wff, *inner, false);
        builder.push_formula(Formula::Existential(*var, inner))
      }
    }
    Some(Formula::Universal(var, inner)) => {
      if negated {
        let inner = to_nnf(builder, wff, *inner, true);
        builder.push_formula(Formula::Existential(*var, inner))
      } else {
        let inner = to_nnf(builder, wff, *inner, false);
        builder.push_formula(Formula::Universal(*var, inner))
      }
    }
    None => unreachable!(),
  }
}

fn prenex(
  builder: &mut WffBuilder,
  wff: &Wff,
  id: FormulaId,
  quantifiers: &mut Vec<(bool, u32)>,
) -> FormulaId {
  match wff.get_formula(id) {
    Some(Formula::RelationApplication(rel_id)) => {
      builder.push_formula(Formula::RelationApplication(*rel_id))
    }
    Some(Formula::Identity(left, right)) => {
      builder.push_formula(Formula::Identity(*left, *right))
    }
    Some(Formula::Negation(inner)) => {
      let inner = prenex(builder, wff, *inner, quantifiers);
      builder.push_formula(Formula::Negation(inner))
    }
    Some(Formula::Disjunction(left, right)) => {
      let left = prenex(builder, wff, *left, quantifiers);
      let right = prenex(builder, wff, *right, quantifiers);
      builder.push_formula(Formula::Disjunction(left, right))
    }
    Some(Formula::Conjunction(left, right)) => {
      let left = prenex(builder, wff, *left, quantifiers);
      let right = prenex(builder, wff, *right, quantifiers);
      builder.push_formula(Formula::Conjunction(left, right))
    }
    Some(Formula::Existential(var, inner)) => {
      quantifiers.push((false, *var));
      prenex(builder, wff, *inner, quantifiers)
    }
    Some(Formula::Universal(var, inner)) => {
      quantifiers.push((true, *var));
      prenex(builder, wff, *inner, quantifiers)
    }
    Some(Formula::Conditional(_, _) | Formula::Biconditional(_, _)) => {
      // should be eliminated in nnf pass...
      unreachable!()
    }
    None => unreachable!(),
  }
}

fn wrap_quantifiers(builder: &mut WffBuilder, matrix: FormulaId, quantifiers: Vec<(bool, u32)>) -> FormulaId {
  quantifiers.into_iter().rev().fold(matrix, |inner, (is_universal, var)| {
    if is_universal {
      builder.push_formula(Formula::Universal(var, inner))
    } else {
      builder.push_formula(Formula::Existential(var, inner))
    }
  })
}

fn make_pnf(wff: &Wff) -> Wff {
  let mut builder = WffBuilder::new();
  let root = to_nnf(&mut builder, wff, wff.root(), false);
  let nnf = builder.build(root).unwrap();

  let mut builder = WffBuilder::new();
  let mut quantifiers = Vec::with_capacity(64);
  let matrix = prenex(&mut builder, &nnf, nnf.root(), &mut quantifiers);
  let root = wrap_quantifiers(&mut builder, matrix, quantifiers);
  builder.build(root).unwrap()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Class {
  Sigma(usize),
  Pi(usize),
  Delta(usize),
}

pub fn tarski_kuratowski(wff: &Wff) -> Class {
  let pnf = make_pnf(wff);
  let mut alternations = 0;
  let mut current_id = pnf.root();

  #[derive(Debug)]
  enum OuterQuantifier {
    Universal,
    Existential,
  }

  let outer_quantifier = match pnf.get_formula(current_id) {
    Some(Formula::Universal(_, _)) => Some(OuterQuantifier::Universal),
    Some(Formula::Existential(_, _)) => Some(OuterQuantifier::Existential),
    _ => None,
  };

  loop {
    match pnf.get_formula(current_id) {
      Some(Formula::Universal(_, inner)) => {
        alternations += 1;
        current_id = *inner;
      }
      Some(Formula::Existential(_, inner)) => {
        alternations += 1;
        current_id = *inner;
      }
      _ => break,
    }
  }

  match outer_quantifier {
    Some(OuterQuantifier::Universal) => Class::Pi(alternations),
    Some(OuterQuantifier::Existential) => Class::Sigma(alternations),
    None => Class::Delta(0),
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  // crappy demo for testing
  fn build_test_formula(size: usize) -> Wff {
    let mut builder = WffBuilder::new();

    let mut current = builder.push_formula(Formula::RelationApplication(0));

    for i in 0..size {
      let next = builder.push_formula(Formula::RelationApplication((i % 10) as u32));
      current = match i % 4 {
        0 => builder.push_formula(Formula::Conjunction(current, next)),
        1 => builder.push_formula(Formula::Disjunction(current, next)),
        2 => builder.push_formula(Formula::Negation(current)),
        _ => {
          let var = (i % 5) as u32;
          builder.push_formula(Formula::Existential(var, current))
        }
      };
    }

    builder.build(current).unwrap()
  }

  fn matrix_is_quantifier_free(wff: &Wff, id: FormulaId) -> bool {
    match wff.get_formula(id) {
      Some(Formula::Universal(_, _)) | Some(Formula::Existential(_, _)) => false,
      Some(Formula::Negation(inner)) => matrix_is_quantifier_free(wff, *inner),
      Some(Formula::Disjunction(l, r)) | Some(Formula::Conjunction(l, r)) => {
        matrix_is_quantifier_free(wff, *l) && matrix_is_quantifier_free(wff, *r)
      }
      Some(Formula::RelationApplication(_)) | Some(Formula::Identity(_, _)) => true,
      _ => true,
    }
  }

  fn skip_quantifiers(wff: &Wff, mut id: FormulaId) -> FormulaId {
    loop {
      match wff.get_formula(id) {
        Some(Formula::Universal(_, inner)) | Some(Formula::Existential(_, inner)) => {
          id = *inner;
        }
        _ => return id,
      }
    }
  }

  #[test]
  fn test_basic_pnf() {
    let wff = build_test_formula(10);
    println!("{}", &wff);
    let pnf = make_pnf(&wff);
    println!("{}", &pnf);
    let matrix = skip_quantifiers(&pnf, pnf.root());
    assert!(matrix_is_quantifier_free(&pnf, matrix));
  }

  #[test]
  fn test_tarski_kuratowski() {
    let wff = build_test_formula(10);
    let class = tarski_kuratowski(&wff);
    assert!(matches!(class, Class::Sigma(_) | Class::Pi(_)));
  }

  #[test]
  fn test_nested_quantifier_in_conjunction() {
    let mut builder = WffBuilder::new();
    let p = builder.push_formula(Formula::RelationApplication(0));
    let q = builder.push_formula(Formula::RelationApplication(1));
    let r = builder.push_formula(Formula::RelationApplication(2));
    let exists_r = builder.push_formula(Formula::Existential(0, r));
    let q_and_exists_r = builder.push_formula(Formula::Conjunction(q, exists_r));
    let root = builder.push_formula(Formula::Conjunction(p, q_and_exists_r));
    let wff = builder.build(root).unwrap();

    let pnf = make_pnf(&wff);

    assert!(matches!(pnf.get_formula(pnf.root()), Some(Formula::Existential(0, _))));

    let matrix = skip_quantifiers(&pnf, pnf.root());
    assert!(matrix_is_quantifier_free(&pnf, matrix));
  }

  #[test]
  fn test_nested_quantifier_in_disjunction() {
    let mut builder = WffBuilder::new();
    let p = builder.push_formula(Formula::RelationApplication(0));
    let q = builder.push_formula(Formula::RelationApplication(1));
    let r = builder.push_formula(Formula::RelationApplication(2));
    let exists_r = builder.push_formula(Formula::Existential(0, r));
    let q_and_exists_r = builder.push_formula(Formula::Conjunction(q, exists_r));
    let root = builder.push_formula(Formula::Disjunction(p, q_and_exists_r));
    let wff = builder.build(root).unwrap();

    let pnf = make_pnf(&wff);

    assert!(matches!(pnf.get_formula(pnf.root()), Some(Formula::Existential(0, _))));

    let matrix = skip_quantifiers(&pnf, pnf.root());
    assert!(matrix_is_quantifier_free(&pnf, matrix));
  }

  #[test]
  fn test_double_nesting() {
    let mut builder = WffBuilder::new();
    let p = builder.push_formula(Formula::RelationApplication(0));
    let q = builder.push_formula(Formula::RelationApplication(1));
    let exists_p = builder.push_formula(Formula::Existential(0, p));
    let exists_q = builder.push_formula(Formula::Existential(1, q));
    let root = builder.push_formula(Formula::Disjunction(exists_p, exists_q));
    let wff = builder.build(root).unwrap();

    let pnf = make_pnf(&wff);

    let mut count = 0;
    let mut id = pnf.root();
    loop {
      match pnf.get_formula(id) {
        Some(Formula::Existential(_, inner)) => {
          count += 1;
          id = *inner;
        }
        _ => break,
      }
    }
    assert_eq!(count, 2);

    let matrix = skip_quantifiers(&pnf, pnf.root());
    assert!(matrix_is_quantifier_free(&pnf, matrix));
  }

  #[test]
  fn test_mixed_quantifiers() {
    let mut builder = WffBuilder::new();
    let p = builder.push_formula(Formula::RelationApplication(0));
    let q = builder.push_formula(Formula::RelationApplication(1));
    let forall_p = builder.push_formula(Formula::Universal(0, p));
    let exists_q = builder.push_formula(Formula::Existential(1, q));
    let root = builder.push_formula(Formula::Conjunction(forall_p, exists_q));
    let wff = builder.build(root).unwrap();

    let pnf = make_pnf(&wff);
    assert!(matches!(pnf.get_formula(pnf.root()), Some(Formula::Universal(0, _))));

    let matrix = skip_quantifiers(&pnf, pnf.root());
    assert!(matrix_is_quantifier_free(&pnf, matrix));
  }
}