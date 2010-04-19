(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml, chap1_2.sml, chap2_2.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 3  Modularity, Objects, and State *)
(* 3.5  Streams *)
(* 3.5.1  Streams Are Delayed Lists *)

(* prime: => included in chap1_2.sml *)

fun sumPrimes (a, b) =
    let
      fun iter (count, accum) =
          if count > b then
            accum
          else if prime count then
            iter (count + 1, accum + count)
          else
            iter (count + 1, accum)
    in
      iter (a, 0)
    end;

sumPrimes (1, 100);

(* accumulate: => included in chap2_2.sml *)
(* enumerateInterval: => included in chap2_2.sml *)

fun sumPrimes' (a, b) =
    accumulate (op +)
               0
               (filter prime (enumerateInterval (a, b)));

sumPrimes' (1, 100);

(*
 * List.nth (filter prime (enumerateInterval (10000, 1000000)), 1)
 *)

signature STREAM =
sig
  type 'a t
  val null : 'a t
  val cons : 'a * (unit -> 'a t) -> 'a t
  val car : 'a t -> 'a
  val cdr : 'a t -> 'a t
  val isNull : 'a t -> bool
end;

functor StreamOpsFn (Stream : STREAM) =
struct
  open Stream

  fun nth (stream, n) =
      if n = 0 then car stream
      else nth (cdr stream, n - 1)

  fun map proc stream =
      if isNull stream then null
      else cons (proc (car stream),
              fn () => (U.log "map: proc called";
                        map proc (cdr stream)))

  fun map2 proc (stream1, stream2) =
      if isNull stream1 orelse isNull stream2 then null
      else cons (proc (car stream1, car stream2),
              fn () => (U.log "map: proc called";
                        map2 proc (cdr stream1, cdr stream2)))

  fun forEach proc stream =
      if isNull stream then ()
      else (proc (car stream);
            forEach proc (cdr stream))

  fun forEach2 proc (stream1, stream2) =
      if isNull stream1 orelse isNull stream2 then ()
      else (proc (car stream1, car stream2);
            forEach2 proc (cdr stream1, cdr stream2))

  fun filter predicate stream =
      if isNull stream then null
      else if predicate (car stream) then
        cons (car stream,
           fn () =>
              (U.log "filter: proc called";
               filter predicate (cdr stream)))
      else
        filter predicate (cdr stream)

  fun display aToString stream =
      forEach (fn a => print (aToString a ^ "\n")) stream

  fun enumerateInterval (low, high) =
      if low > high then null
      else cons (low,
              fn () =>
                 (U.log "enumerateInterval: proc called";
                  enumerateInterval (low + 1, high)))

  fun integersStartingFrom n =
      cons (n, fn () =>
                  (U.log ("integersStartingFrom: proc called: n=" ^
                          Int.toString n);
                   integersStartingFrom (n + 1)));

  val integers = integersStartingFrom 1;
end;

structure Stream' :> STREAM =
struct
  datatype 'a t = Nil | Cons of 'a * (unit -> 'a t)

  val null = Nil

  fun memoProc (proc : unit -> 'a t) : unit -> 'a t =
      let
        val sref = ref NONE
      in
        (fn () =>
            case !sref of
              SOME s =>
              (U.log "memoProc: cache returned"; s)
            | NONE =>
              let
                val s = proc ()
              in
                sref := (SOME s);
                U.log "memoProc: cache stored";
                s
              end)
      end

  fun cons (a, p) = Cons (a, (memoProc p))

  fun car (Cons (a, _)) = a
    | car Nil = raise Empty

  fun cdr (Cons (_, susp)) = susp ()
    | cdr Nil = raise Empty

  fun isNull (Cons _) = false
    | isNull Nil = true
end;

(* prime: => included in chap1_2.sml *)

structure SO = StreamOpsFn (Stream');
SO.nth (SO.filter prime (SO.enumerateInterval (10000, 1000000)), 1);

structure Stream'' :> STREAM =
struct
  structure S = SMLofNJ.Susp

  datatype 'a t = Nil | Cons of 'a * 'a t S.susp

  val null = Nil

  fun cons (a, p) = Cons (a, S.delay p)

  fun car (Cons (a, _)) = a
    | car Nil = raise Empty

  fun cdr (Cons (_, susp)) = S.force susp
    | cdr Nil = raise Empty

  fun isNull (Cons _) = false
    | isNull Nil = true
end;

(* prime: => included in chap1_2.sml *)

structure SO = StreamOpsFn (Stream'');
SO.nth (SO.filter prime (SO.enumerateInterval (10000, 1000000)), 1);

(* 3.5.2  Infinite Streams *)

val noSevens = SO.filter (fn x => not (I.divisible (x, 7))) SO.integers;

SO.nth (noSevens, 100);

fun fibgen (a, b) =
    SO.cons (a, fn () =>
                   (U.log ("fibgen: proc called: a=" ^
                           Int.toString a);
                    fibgen (b, a + b)));

val fibs = fibgen (0, 1);

SO.nth (fibs, 10);

fun sieve stream = (* sieve of Eratosthenes *)
    SO.cons (SO.car stream,
          fn () =>
             (U.log ("sieve: proc called: x=" ^
                     Int.toString (SO.car stream));
              sieve (SO.filter (fn x =>
                                   not (I.divisible (x, SO.car stream)))
                               (SO.cdr stream))));

val primes = sieve (SO.integersStartingFrom 2);

SO.nth (primes, 50);

(* Defining streams implicitly => chap3_5_lazy.sml *)
