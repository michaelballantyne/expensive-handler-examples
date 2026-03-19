(* Compute primes with a sieve based on generators, which
   are implemented as computations that perform a yield effect.

   A counter effect instruments how many remainder computations
   the sieve performs. Incrementing the counter requires crossing
   many yield handlers installed by the nested calls to filter_gen.

   All handlers use Effect.Shallow, matching the Racket version's
   explicit recursive re-installation of handlers.

   A generator is represented as a (unit, unit) Effect.Shallow.continuation
   rather than a thunk, since handler continuations k are already
   continuations — this avoids reify/fiber roundtrips. *)

(* Effects *)
type _ Effect.t +=
  | Yield : int -> unit Effect.t
  | Ctr_inc : int Effect.t

let yield v = Effect.perform (Yield v)
let ctr_inc () = Effect.perform Ctr_inc

type gen = (unit, unit) Effect.Shallow.continuation

(* Counter effect handler — state threaded through recursive re-installation,
   matching the Racket: (with-counter (+ n 1) (lambda () (k n))) *)
let with_counter init thunk =
  let rec handler : type b. int -> (b, b) Effect.Shallow.handler = fun n ->
    { Effect.Shallow.retc = Fun.id
    ; exnc = raise
    ; effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Ctr_inc ->
          Some (fun (k : (a, _) Effect.Shallow.continuation) ->
            Effect.Shallow.continue_with k n (handler (n + 1)))
        | _ -> None
    }
  in
  Effect.Shallow.continue_with (Effect.Shallow.fiber thunk) () (handler init)

(* collect : gen -> int list *)
let collect (g : gen) =
  let rec handler : (unit, int list) Effect.Shallow.handler =
    { Effect.Shallow.retc = (fun () -> [])
    ; exnc = raise
    ; effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Yield v ->
          Some (fun (k : (a, _) Effect.Shallow.continuation) ->
            v :: Effect.Shallow.continue_with k () handler)
        | _ -> None
    }
  in
  Effect.Shallow.continue_with g () handler

(* filter_gen : gen -> (int -> bool) -> gen *)
let filter_gen (g : gen) pred : gen =
  Effect.Shallow.fiber (fun () ->
    let rec handler : (unit, unit) Effect.Shallow.handler =
      { Effect.Shallow.retc = Fun.id
      ; exnc = raise
      ; effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Yield v ->
            Some (fun (k : (a, _) Effect.Shallow.continuation) ->
              if pred v then yield v;
              Effect.Shallow.continue_with k () handler)
          | _ -> None
      }
    in
    Effect.Shallow.continue_with g () handler)

(* naturals : int -> int -> gen *)
let naturals min max : gen =
  Effect.Shallow.fiber (fun () ->
    let rec loop i =
      if i < max then begin
        yield i;
        loop (i + 1)
      end
    in
    loop min)

(* run_sieve : gen -> unit  —  directly performs yields, no wrapping *)
let rec run_sieve (g : gen) =
  Effect.Shallow.continue_with g ()
    { Effect.Shallow.retc = Fun.id
    ; exnc = raise
    ; effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Yield p ->
          Some (fun (k : (a, _) Effect.Shallow.continuation) ->
            yield p;
            run_sieve (filter_gen k
                        (fun x -> ignore (ctr_inc ()); x mod p <> 0)))
        | _ -> None
    }

(* sieve : gen -> gen *)
let sieve (g : gen) : gen =
  Effect.Shallow.fiber (fun () -> run_sieve g)

let () =
  let n = 6000 in
  with_counter 0 (fun () ->
    let t0 = Sys.time () in
    let primes = collect (sieve (naturals 2 n)) in
    let t1 = Sys.time () in
    Printf.printf "cpu time: %f\n" (t1 -. t0);
    Printf.printf "%d primes found, %d remainder computations\n"
      (List.length primes) (ctr_inc ()))
