(* Compute primes with a sieve based on generators, which
   are implemented as computations that perform a yield effect.

   A counter effect instruments how many remainder computations
   the sieve performs. Incrementing the counter requires crossing
   many yield handlers installed by the nested calls to filter_gen.

   All handlers use Effect.Shallow, matching the Racket version's
   explicit recursive re-installation of handlers. *)

(* Effects *)
type _ Effect.t +=
  | Yield : int -> unit Effect.t
  | Ctr_inc : int Effect.t

let yield v = Effect.perform (Yield v)
let ctr_inc () = Effect.perform Ctr_inc

(* Convert a shallow continuation back into a generator thunk
   by re-performing all effects it produces. *)
let reify (k : (unit, unit) Effect.Shallow.continuation) : unit -> unit =
  fun () ->
    let rec fwd : (unit, unit) Effect.Shallow.handler =
      { Effect.Shallow.retc = Fun.id
      ; exnc = raise
      ; effc = fun (type a) (eff : a Effect.t) ->
          Some (fun (k : (a, _) Effect.Shallow.continuation) ->
            let v = Effect.perform eff in
            Effect.Shallow.continue_with k v fwd)
      }
    in
    Effect.Shallow.continue_with k () fwd

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

(* A generator is a (unit -> unit) that may yield values when called. *)

(* collect : generator -> int list *)
let collect gen =
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
  Effect.Shallow.continue_with (Effect.Shallow.fiber gen) () handler

(* filter_gen : generator -> (int -> bool) -> generator *)
let filter_gen gen pred =
  fun () ->
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
    Effect.Shallow.continue_with (Effect.Shallow.fiber gen) () handler

(* naturals : int -> int -> generator *)
let rec naturals min max =
  fun () ->
    if min < max then begin
      yield min;
      (naturals (min + 1) max) ()
    end

(* sieve : generator -> generator *)
let rec sieve gen =
  fun () ->
    Effect.Shallow.continue_with (Effect.Shallow.fiber gen) ()
      { Effect.Shallow.retc = Fun.id
      ; exnc = raise
      ; effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Yield p ->
            Some (fun (k : (a, _) Effect.Shallow.continuation) ->
              yield p;
              (sieve (filter_gen (reify k)
                        (fun x -> ignore (ctr_inc ()); x mod p <> 0))) ())
          | _ -> None
      }

let () =
  let n = 4000 in
  with_counter 0 (fun () ->
    let t0 = Sys.time () in
    let primes = collect (sieve (naturals 2 n)) in
    let t1 = Sys.time () in
    Printf.printf "cpu time: %f\n" (t1 -. t0);
    Printf.printf "%d primes found, %d remainder computations\n"
      (List.length primes) (ctr_inc ()))
