// Compute primes with a sieve based on generators, which
// are implemented as computations that perform a yield effect.
//
// A counter effect instruments how many remainder computations
// the sieve performs. Incrementing the counter requires crossing
// many yield handlers installed by the nested calls to filter-gen.
//
// Koka only has deep handlers (even raw ctl reinstalls). To split
// a stream into head + tail for sieve, we reify via a concrete
// step type (deep handler -> data), then convert back to yield-based
// generators for filter-gen (which composes naturally with deep handlers).
//
// The polymorphic ystate<e,a> pattern (as in pstate from unix.kk)
// causes infinite types when mixed with yield handlers — the open
// effect row variable unifies with a row containing yield, creating
// <yield,yield,...>. A concrete step type avoids this.

effect ctl yield(v : int) : ()
effect ctl ctr-inc() : int

// counter effect via ref cell
fun with-counter(n : int, action)
  var ctr := n - 1
  with fun ctr-inc()
    ctr := ctr + 1
    ctr
  action()

fun collect(action)
  with handler
    return(_) Nil
    ctl yield(v) Cons(v, resume(()))
  action()

fun naturals(mn : int, mx : int) : <div,yield> ()
  if mn < mx then
    yield(mn)
    naturals(mn + 1, mx)

// Nested filter-gens compose correctly with deep handlers.
fun filter-gen(gen, pred)
  fn()
    with ctl yield(v)
      if pred(v) then yield(v)
      resume(())
    gen()

// Concrete step type for reifying the yield stream.
// Deep handler converts yields to data; rest() resumes to next step.
type step
  Done
  Yielded(value : int, rest : () -> <div,ctr-inc> step)

fun to-step(gen : () -> <div,yield,ctr-inc> ()) : <div,ctr-inc> step
  with ctl yield(v)
    Yielded(v, fn() resume(()))
  gen()
  Done

fun from-step(s : step) : <div,yield,ctr-inc> ()
  match s
    Done -> ()
    Yielded(v, rest) ->
      yield(v)
      from-step(rest())

// Sieve: split via step (to get the prime), filter the rest
// via yield-based filter-gen, then recurse.
fun run-sieve(s : step) : <div,yield,ctr-inc> ()
  match s
    Done -> ()
    Yielded(p, rest) ->
      yield(p)
      val filtered = filter-gen(fn() from-step(rest()), fn(x) { val _ = ctr-inc(); x % p != 0 })
      run-sieve(to-step(filtered))

pub fun main()
  val n = 20000
  with-counter(0)
    val primes = collect fn()
      run-sieve(to-step(fn() naturals(2, n)))
    println(primes.length.show ++ " primes found, " ++ ctr-inc().show ++ " remainder computations")
