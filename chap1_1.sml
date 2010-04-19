(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 1  Building Abstractions with Procedures *)
(* 1.1  The Elements of Programming *)
(* 1.1.1  Expressions *)

486;

137 + 349;

1000 - 334;

99 * 5;

10 div 5;

2.7 + 10.0;

21 + 35 + 12 + 7;

25 * 4 * 12;

(3 * 5) + (10 - 6);

(3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6);

(* 1.1.2  Naming and the Environment *)

val size = 2;

size;

5 * size;

val pi = 3.14159;

val radius = 10.0;

pi * radius * radius;

fun circumference () = 2.0 * pi * radius;

circumference();

(* 1.1.3  Evaluating Combinations *)

(2 + (4 * 6)) * (3 + 5 + 7);

(* 1.1.4  Compound Procedures *)

(*
 * fun square x = x * x;
 * => included in util.sml
 *)

I.square 21;

I.square (2 + 5);

I.square (I.square 3);

fun sumOfSquares (x, y) = I.square x + I.square y;

sumOfSquares (3, 4);

fun f a = sumOfSquares (a + 1, a * 2);

f 5;

(* 1.1.5  The Substitution Model for Procedure Application *)

(* 1.1.6  Conditional Expressions and Predicates *)

(*
 * fun abs x = if x < 0 then ~x else x;
 * => included in util.sml
 *)

fun ge (x, y) = (x > y) orelse (x = y);

fun ge (x, y) = not (x < y);

(* 1.1.7  Example: Square Roots by Newton's Method *)

(*
 * fun average (x, y) = (x + y) / 2.0;
 * => included in util.sml
 *)

fun improve (guess, x) =
    R.average (guess, x / guess);

fun goodEnough (guess, x) =
    R.abs (R.square guess - x) < 0.001;

fun sqrtIter (guess, x) =
    if goodEnough (guess, x) then guess
    else sqrtIter (improve (guess, x), x);

fun sqrt x = sqrtIter (1.0, x);

sqrt 9.0;

sqrt (100.0 + 37.0);

sqrt (sqrt 2.0 + sqrt 3.0);

R.square (sqrt 1000.0);

(* 1.1.8  Procedures as Black-Box Abstractions *)

fun sqrt x =
    let
      fun goodEnough (guess, x) =
          R.abs (R.square guess - x) < 0.001
      and improve (guess, x) =
          R.average (guess, x / guess)
      fun sqrtIter (guess, x) =
          if goodEnough (guess, x) then guess
          else sqrtIter (improve (guess, x), x)
    in
      sqrtIter (1.0, x)
    end;

sqrt 9.0;

fun sqrt x =
    let
      fun goodEnough guess =
          R.abs (R.square guess - x) < 0.001
      and improve guess =
          R.average (guess, x / guess)
      fun sqrtIter guess =
          if goodEnough guess then guess
          else sqrtIter (improve guess)
    in
      sqrtIter 1.0
    end;

sqrt 9.0;

