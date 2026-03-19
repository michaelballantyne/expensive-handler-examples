// What goes wrong: sieve with deep handlers, no reification
//
// This is the direct translation of the Racket sieve pattern:
// catch the first yield as a prime, wrap the continuation in
// filter-gen, and recursively sieve. It type-checks in Racket
// (where all yields share a single prompt tag) but fails in
// Koka due to infinite effect row expansion.
//
// The error:
//   inferred effect: <yield|_e>
//   expected effect: <yield,yield,yield|_e>
//
// Each ctl yield handler removes one yield from the row and
// the handler body adds one back (the re-yield). With recursive
// run-sieve calling filter-gen calling run-sieve, the type
// checker tries to solve e = <yield|e>, which has no finite
// solution in Koka's row-typed effect system.

effect ctl yield(v : int) : ()

fun collect(action)
  with handler
    return(_) Nil
    ctl yield(v) Cons(v, resume(()))
  action()

fun naturals(mn : int, mx : int) : <div,yield> ()
  if mn < mx then
    yield(mn)
    naturals(mn + 1, mx)

fun filter-gen(gen, pred)
  fn()
    with ctl yield(v)
      if pred(v) then yield(v)
      resume(())
    gen()

// Sieve: deep handler catches first yield as prime,
// wraps continuation in filter-gen, recurses.
// DOES NOT COMPILE — infinite type.
fun run-sieve(gen)
  with ctl yield(p)
    yield(p)
    run-sieve(filter-gen(fn() resume(()), fn(x) x % p != 0))
  gen()

pub fun main()
  val result = collect(fn() run-sieve(fn() naturals(2, 30)))
  println(result.show)
  println("expected: [2,3,5,7,11,13,17,19,23,29]")
