use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion};

use lib_fol::{Wff, WffBuilder, Formula};
use lib_tarski_kuratowski::tarski_kuratowski;

// crappy demo formula builder for benchmarking
// - should definitely be replaced with something better, but it works for a rough
//   overview of runtimes
fn build_benchmark_formula(size: usize) -> Wff {
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

fn benchmark_pnf(c: &mut Criterion) {
  c.bench_function("tarski_kuratowski_100", |b| {
    let wff = build_benchmark_formula(100);
    b.iter(|| {
      tarski_kuratowski(black_box(&wff))
    })
  });

  c.bench_function("tarski_kuratowski_500", |b| {
    let wff = build_benchmark_formula(500);
    b.iter(|| {
      tarski_kuratowski(black_box(&wff))
    })
  });

  c.bench_function("tarski_kuratowski_1000", |b| {
    let wff = build_benchmark_formula(1000);
    b.iter(|| {
      tarski_kuratowski(black_box(&wff))
    })
  });
}

criterion_group!(benches, benchmark_pnf);
criterion_main!(benches);