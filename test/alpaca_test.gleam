import alpaca
import gleam/int
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

fn random_int() {
  int.random(100)
}

fn cached_int() {
  use <- alpaca.persist()
  random_int()
}

pub fn simple_test() {
  let n = cached_int()
  let m = cached_int()

  n |> should.equal(m)
}
