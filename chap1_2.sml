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
(* 1.2  Procedures and the Processes They Generate *)
(* 1.2.1  Linear Recursion and Iteration *)

fun factorial n =
    if n = 1 then 1
    else n * factorial (n - 1);

factorial 6;

local
  fun factIter (product, counter, maxCount) =
      if counter > maxCount then product
      else factIter (counter * product, counter + 1, maxCount)
in
fun factorial' n = factIter (1, 1, n)
end;

factorial' 6;

(* 1.2.2  Tree Recursion *)

fun fib n =
    if n = 0 then 0
    else if n = 1 then 1
    else fib (n - 1) + fib (n - 2);

fib 10;

local
  fun fibIter (a, b, count) =
      if count = 0 then b
      else fibIter (a + b, a, count - 1)
in
fun fib' n = fibIter (1, 0, n)
end;

fib' 10;

local
  fun firstDenomination kindsOfCoins =
      case kindsOfCoins of
        1 => 1
      | 2 => 5
      | 3 => 10
      | 4 => 25
      | 5 => 50
      | coin => raise Fail ("Unknown kinds of coins: " ^
                            Int.toString coin)

  fun cc (amount, kindsOfCoins) =
      if amount = 0 then 1
      else if (amount < 0) orelse (kindsOfCoins = 0) then 0
      else cc (amount, kindsOfCoins - 1) +
           cc (amount - firstDenomination kindsOfCoins, kindsOfCoins)
in
fun countChange amount = cc (amount, 5)
end;

(* 1.2.3  Orders of Growth *)

(* 1.2.4  Exponentiation *)

fun expt (b, n) =
    if n = 0 then 1
    else b * expt (b, n - 1);

expt (2, 4);

local
  fun exptIter (b, counter, product) =
      if counter = 0 then product
      else exptIter (b, counter - 1, b * product)
in
fun expt' (b, n) = exptIter (b, n, 1)
end;

expt' (2, 4);

(*
 * fun even n = n mod 2 = 0;
 * => included in util.sml
 *)

fun fastExpt (b, n) =
    if n = 0 then 1
    else if I.even n then I.square (fastExpt (b, n div 2))
    else b * fastExpt (b, n - 1);

(* 1.2.5  Greatest Common Divisors *)

(*
 * fun gcd (a, b) =
 *     if b = 0 then a
 *     else gcd (b, a mod b);
 * => included in util.sml
 *)

(* 1.2.6  Example: Testing for Primality *)

local
  fun divides (a, b) = (b mod a) = 0

  fun findDivisor (n, testDivisor) =
      if I.square testDivisor > n then n
      else if divides (testDivisor, n) then testDivisor
      else findDivisor (n, testDivisor + 1)

  fun smallestDivisor n = findDivisor (n, 2)
in
fun prime n = n = (smallestDivisor n)
end;

local
  fun expmod (base, exp, m) =
      if exp = 0 then
        1
      else if I.even exp then
        I.square (expmod (base, exp div 2, m)) mod m
      else
        base * expmod (base, exp - 1, m) mod m

  fun fermatTest n =
      let
        fun tryIt a = expmod (a, n, n) = a
      in
        tryIt (1 + U.random (n - 1))
      end
in
fun fastPrime (n, times) =
    if times = 0 then true
    else if fermatTest n then fastPrime (n, times - 1)
    else false
end;
