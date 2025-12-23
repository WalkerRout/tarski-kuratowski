use std::fmt;
use std::convert::Infallible;

pub type VarId = u32;

pub type FunId = u32;

pub type TermId = u32;

#[derive(Debug, Clone)]
pub enum Term {
  Variable(VarId),
  FunctionApplication(FunId, Vec<TermId>),
}

pub type RelId = u32;

pub type FormulaId = u32;

#[derive(Debug, Clone)]
pub enum Formula {
  RelationApplication(RelId),
  Identity(TermId, TermId),
  Negation(FormulaId),
  Disjunction(FormulaId, FormulaId),
  Conjunction(FormulaId, FormulaId),
  Conditional(FormulaId, FormulaId),
  Biconditional(FormulaId, FormulaId),
  // fresh VarId
  Existential(VarId, FormulaId),
  Universal(VarId, FormulaId),
}

#[derive(Debug, Clone)]
struct FormulaArena {
  terms: Vec<Term>,
  formulas: Vec<Formula>,
}

impl FormulaArena {
  fn get_term(&self, id: TermId) -> Option<&Term> {
    self.terms.get(id as usize)
  }

  fn get_formula(&self, id: FormulaId) -> Option<&Formula> {
    self.formulas.get(id as usize)
  }
}

#[derive(Debug, Clone)]
pub struct Wff {
  root: FormulaId,
  arena: FormulaArena,
}

impl Wff {
  pub fn root(&self) -> FormulaId {
    self.root
  }

  pub fn get_formula(&self, id: FormulaId) -> Option<&Formula> {
    self.arena.get_formula(id)
  }

  pub fn get_term(&self, id: TermId) -> Option<&Term> {
    self.arena.get_term(id)
  }
}

fn fmt_term(wff: &Wff, f: &mut fmt::Formatter<'_>, id: TermId) -> fmt::Result {
  match wff.arena.get_term(id) {
    Some(Term::Variable(var)) => write!(f, "x{}", var),
    Some(Term::FunctionApplication(fun, args)) => {
      write!(f, "f{}(", fun)?;
      for (i, arg) in args.iter().enumerate() {
        if i > 0 {
          write!(f, ", ")?;
        }
        fmt_term(wff, f, *arg)?;
      }
      write!(f, ")")
    }
    None => write!(f, "?term{}?", id),
  }
}

fn fmt_formula(wff: &Wff, f: &mut fmt::Formatter<'_>, id: FormulaId) -> fmt::Result {
  match wff.arena.get_formula(id) {
    Some(Formula::RelationApplication(rel)) => {
      write!(f, "P{}", rel)
    }
    Some(Formula::Identity(left, right)) => {
      fmt_term(wff, f, *left)?;
      write!(f, " = ")?;
      fmt_term(wff, f, *right)
    }
    Some(Formula::Negation(inner)) => {
      write!(f, "¬")?;
      fmt_formula_parens(wff, f, *inner)
    }
    Some(Formula::Disjunction(left, right)) => {
      write!(f, "(")?;
      fmt_formula(wff, f, *left)?;
      write!(f, " ∨ ")?;
      fmt_formula(wff, f, *right)?;
      write!(f, ")")
    }
    Some(Formula::Conjunction(left, right)) => {
      write!(f, "(")?;
      fmt_formula(wff, f, *left)?;
      write!(f, " ∧ ")?;
      fmt_formula(wff, f, *right)?;
      write!(f, ")")
    }
    Some(Formula::Conditional(left, right)) => {
      write!(f, "(")?;
      fmt_formula(wff, f, *left)?;
      write!(f, " → ")?;
      fmt_formula(wff, f, *right)?;
      write!(f, ")")
    }
    Some(Formula::Biconditional(left, right)) => {
      write!(f, "(")?;
      fmt_formula(wff, f, *left)?;
      write!(f, " ↔ ")?;
      fmt_formula(wff, f, *right)?;
      write!(f, ")")
    }
    Some(Formula::Existential(var, inner)) => {
      write!(f, "∃x{}. ", var)?;
      fmt_formula(wff, f, *inner)
    }
    Some(Formula::Universal(var, inner)) => {
      write!(f, "∀x{}. ", var)?;
      fmt_formula(wff, f, *inner)
    }
    None => write!(f, "?formula{}?", id),
  }
}

/// Format with parentheses for atoms/negations that need grouping
fn fmt_formula_parens(wff: &Wff, f: &mut fmt::Formatter<'_>, id: FormulaId) -> fmt::Result {
  match wff.arena.get_formula(id) {
    Some(Formula::RelationApplication(_)) | Some(Formula::Identity(_, _)) => {
      fmt_formula(wff, f, id)
    }
    Some(Formula::Negation(inner)) => {
      write!(f, "¬")?;
      fmt_formula_parens(wff, f, *inner)
    }
    _ => {
      write!(f, "(")?;
      fmt_formula(wff, f, id)?;
      write!(f, ")")
    }
  }
}

impl fmt::Display for Wff {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt_formula(self, f, self.root)
  }
}

#[derive(Debug, Clone)]
pub struct WffBuilder {
  terms: Vec<Term>,
  formulas: Vec<Formula>,
}

impl WffBuilder {
  pub fn new() -> Self {
    Self {
      terms: Vec::with_capacity(64),
      formulas: Vec::with_capacity(64),
    }
  }
  
  pub fn push_term(&mut self, term: Term) -> TermId {
    let id = self.terms.len() as u32;
    self.terms.push(term);
    id
  }
  
  pub fn push_formula(&mut self, formula: Formula) -> FormulaId {
    let id = self.formulas.len() as u32;
    self.formulas.push(formula);
    id
  }
  
  pub fn build(self, root: FormulaId) -> Result<Wff, Infallible> {
    assert!(root < self.formulas.len() as u32);
    let arena = FormulaArena {
      terms: self.terms,
      formulas: self.formulas,
    };
    Ok(Wff { root, arena })
  }
}