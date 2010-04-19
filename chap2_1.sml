(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 2  Building Abstractions with Data *)
(* 2.1  Introduction to Data Abstraction *)
(* 2.1.1  Example: Arithmetic Operations for Rational Numbers *)

signature RAT = sig
  type t
  val makeRat : int * int -> t
  val numer : t -> int
  val denom : t -> int
end;

functor RatOpsFn (Rat : RAT) = struct
  open Rat

  fun addRat (x, y) =
      makeRat ((numer x) * (denom y) + (numer y) * (denom x),
               (denom x) * (denom y))

  fun subRat (x, y) =
      makeRat ((numer x) * (denom y) - (numer y) * (denom x),
               (denom x) * (denom y))

  fun mulRat (x, y) =
      makeRat ((numer x) * (numer y),
               (denom x) * (denom y))

  fun divRat (x, y) =
      makeRat ((numer x) * (denom y),
               (denom x) * (numer y))

  fun equalRat (x, y) =
      ((numer x) * (denom y)) = ((denom x) * (numer y))

  fun toString x =
      Int.toString (numer x) ^ "/" ^ Int.toString (denom x)
end;

structure Rat' :> RAT = struct
  type t = int * int

  fun makeRat (_, 0) = raise Div
    | makeRat (n, d) = (n, d)

  fun numer ((n, _):t) = n

  fun denom ((_, d):t) = d
end;

structure RO = RatOpsFn (Rat');
val oneHalf = RO.makeRat (1, 2);
val oneThird = RO.makeRat (1, 3);
RO.toString oneHalf;
RO.toString (RO.addRat (oneHalf, oneThird));
RO.toString (RO.mulRat (oneHalf, oneThird));
RO.toString (RO.addRat (oneThird, oneThird));

structure Rat'' :> RAT = struct
  type t = int * int

  fun makeRat (_, 0) = raise Div
    | makeRat (n, d) = let val g = I.gcd (n, d)
                       in (n div g, d div g) end

  fun numer ((n, _):t) = n

  fun denom ((_, d):t) = d
end;

structure RO = RatOpsFn (Rat'');
val oneHalf = RO.makeRat (1, 2);
val oneThird = RO.makeRat (1, 3);
RO.toString oneHalf;
RO.toString (RO.addRat (oneHalf, oneThird));
RO.toString (RO.mulRat (oneHalf, oneThird));
RO.toString (RO.addRat (oneThird, oneThird));

(* 2.1.2  Abstraction Barriers *)

structure Rat''' :> RAT = struct
  type t = int * int

  fun makeRat (_, 0) = raise Div
    | makeRat (n, d) = (n, d)

  fun numer ((n, d):t) = let val g = I.gcd (n, d)
                        in n div g end

  fun denom ((n, d):t) = let val g = I.gcd (n, d)
                         in d div g end
end;

structure RO = RatOpsFn (Rat''');
val oneHalf = RO.makeRat (1, 2);
val oneThird = RO.makeRat (1, 3);
RO.toString oneHalf;
RO.toString (RO.addRat (oneHalf, oneThird));
RO.toString (RO.mulRat (oneHalf, oneThird));
RO.toString (RO.addRat (oneThird, oneThird));

(* 2.1.3  What Is Meant by Data? *)

signature PAIR = sig
  type 'a pair
  val cons : 'a * 'a -> 'a pair
  val car : 'a pair -> 'a
  val cdr : 'a pair -> 'a
end;

structure FunAsPair :> PAIR  = struct
  datatype msg = Car | Cdr
  type 'a pair = msg -> 'a

  fun cons (x, y) : 'a pair =
      let
        fun dispatch Car = x
          | dispatch Cdr = y
      in
        dispatch
      end

  fun car (z:'a pair) = z Car

  fun cdr (z:'a pair) = z Cdr

end;

structure Pair = FunAsPair;
val x = Pair.cons (1, 2);
Pair.car x;
Pair.cdr x;

structure TupleAsPair :> PAIR  = struct
  type 'a pair = 'a * 'a

  fun cons (x, y) : 'a pair = (x, y)

  fun car (x, _) = x

  fun cdr (_, y) = y

end;

structure Pair = TupleAsPair;
val x = Pair.cons (1, 2);
Pair.car x;
Pair.cdr x;

(* 2.1.4  Extended Exercise: Interval Arithmetic *)

functor IntervalFn (Pair : PAIR) = struct
  open Pair

  fun makeInterval (x:real, y:real) : real pair =
      if x > y then raise Fail "illegal order"
      else cons (x, y);

  fun lowerBound (interval:real pair) = car interval

  fun upperBound (interval:real pair) = cdr interval

  fun addInterval (x, y) =
      makeInterval ((lowerBound x) + (lowerBound y),
                    (upperBound x) + (upperBound y))

  fun mulInterval (x, y) =
      let
        val p1 = (lowerBound x) * (lowerBound y)
        and p2 = (lowerBound x) * (upperBound y)
        and p3 = (upperBound x) * (lowerBound y)
        and p4 = (upperBound x) * (upperBound y)
        fun min (x1,x2,x3,x4) = Real.min(Real.min(Real.min(x1,x2),x3),x4)
        and max (x1,x2,x3,x4) = Real.max(Real.max(Real.max(x1,x2),x3),x4)
      in
        makeInterval (min (p1, p2, p3, p4), max (p1, p2, p3, p4))
      end

  fun divInterval (x, y) =
      mulInterval (x, makeInterval (1.0 / (upperBound y),
                                    1.0 / (lowerBound y)))

  fun toString x =
      "[" ^
      Real.toString (lowerBound x) ^
      ", " ^
      Real.toString (upperBound x) ^
      "]"

end;

structure Interval = IntervalFn (FunAsPair);
val x = Interval.makeInterval (1.0, 2.0);
val y = Interval.makeInterval (2.0, 3.0);
Interval.toString x;
Interval.toString y;
Interval.toString (Interval.addInterval (x, y));
Interval.toString (Interval.mulInterval (x, y));
Interval.toString (Interval.divInterval (x, y));


structure Interval = IntervalFn (TupleAsPair);
(* 6.8-ohm 10% *)
val x = Interval.makeInterval (6.12, 7.48);
(* 4.7-ohm 5% *)
val y = Interval.makeInterval (4.465, 4.935);
Interval.toString x;
Interval.toString y;
Interval.toString (Interval.addInterval (x, y));
Interval.toString (Interval.mulInterval (x, y));
Interval.toString (Interval.divInterval (x, y));

fun makeCenterWidth (c, w) =
    Interval.makeInterval (c - w, c + w);

fun center i =
    ((Interval.lowerBound i) + (Interval.upperBound i)) / 2.0;

fun width i =
    ((Interval.upperBound i) - (Interval.lowerBound i)) / 2.0;

val z = makeCenterWidth (3.5, 0.15);
Interval.toString z;
center z;
width z;

fun par1 (r1, r2) =
    Interval.divInterval (Interval.mulInterval (r1, r2),
                          Interval.addInterval (r1, r2));

fun par2 (r1, r2) =
    let
      val one = Interval.makeInterval (1.0, 1.0)
      val invR1 = Interval.divInterval (one, r1)
      and invR2 = Interval.divInterval (one, r2)
    in
      Interval.divInterval (one,
                            Interval.addInterval (invR1, invR2))
    end;

Interval.toString (par1 (x, y));
Interval.toString (par2 (x, y));
