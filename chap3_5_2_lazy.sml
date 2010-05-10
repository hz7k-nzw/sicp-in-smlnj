(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml, chap3_1.sml;
 *
 * Notice: the following code requires the lazy evaluation,
 * which is a non-standard feature of ML provided by SML/NJ.
 * The lazy evaluation features must be enabled by executing
 * the following at the top level:
 *  Control.lazysml := true;
 *
 * For more information, please see the following:
 *  Chapter 15 (Lazy Data Structures)
 *  of
 *  Programming in Standard ML
 *  (WORKING DRAFT OF AUGUST 20, 2009.)
 *  Robert Harper
 *  Carnegie Mellon University
 *  Spring Semester, 2005
 *  -> available online: http://www.cs.cmu.edu/~rwh/smlbook/
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 3  Modularity, Objects, and State *)
(* 3.5  Streams *)
(* 3.5.2  Infinite Streams *)
(* Defining streams implicitly *)

structure ImplicitStream =
struct
  open Lazy;

  (*
   * For simplicity, base case is omitted; only the
   * infinite streams are considered.
   *)
  datatype lazy 'a t = Cons of 'a * 'a t

  fun car (Cons (h, _)) = h

  fun cdr (Cons (_, t)) = t

  fun lazy cdr' (Cons (_, t)) = t

  fun nth (stream, n) =
      if n = 0 then car stream
      else nth (cdr stream, n - 1)

  fun map f =
      let
        fun lazy loop (Cons (x, s)) = Cons (f x, loop s)
      in
        loop
      end

  fun map2 f =
      let
        fun lazy loop (Cons (x1, s1), Cons (x2, s2)) =
            Cons (f (x1, x2), loop (s1, s2))
      in
        loop
      end

  fun filter pred =
      let
        fun lazy loop (Cons (x, s)) =
            if pred x then Cons (x, loop s)
            else loop s
      in
        loop
      end

  fun display (title, aToString, sep) s n =
      let
        fun iter (s, k) =
            if k = n then
              ()
            else
              (print (aToString (car s) ^ sep);
               iter (cdr s, k+1))
      in
        print title;
        iter (s, 0);
        print "...\n"
      end

  fun lazy integersStartingFrom n =
      Cons (n, integersStartingFrom (n + 1))

  fun add (s1:int t, s2:int t) = map2 (op + ) (s1, s2)
  fun mul (s1:int t, s2:int t) = map2 (op * ) (s1, s2)
  fun scale (s, factor:int) = map (fn x => factor * x) s

  fun addR (s1:real t, s2:real t) = map2 (op + ) (s1, s2)
  fun mulR (s1:real t, s2:real t) = map2 (op * ) (s1, s2)
  fun scaleR (s, factor:real) = map (fn x => factor * x) s

  val rec lazy ones = Cons (1, ones)
  val rec lazy integers = Cons (1, add (ones, integers))

  val rec lazy onesR = Cons (1.0, onesR)
  val rec lazy integersR = Cons (1.0, addR (onesR, integersR))
end;

structure IS = ImplicitStream;

(* behavior of cdr and cdr' *)
val rec lazy s1 = (print "... "; IS.Cons (1, s1));
val x1 = IS.cdr s1; (* prints "... " *)
val y1 = IS.cdr s1; (* silent *)
val rec lazy s2 = (print "... "; IS.Cons (1, s2));
val x2 = IS.cdr' s2; (* silent *)
val y2 = IS.cdr s2; (* prints "... " *)

(* ones and integers *)
let
  fun show title = IS.display (title ^ ": ", Int.toString, ", ")
in
  show "ones" IS.ones 10;
  show "integers" IS.integers 10;
  print "done\n"
end;

(* ones and integers (for real) *)
let
  fun showR title = IS.display (title ^ ": ", Real.toString, ", ")
in
  showR "ones" IS.onesR 5;
  showR "integers" IS.integersR 5;
  print "done\n"
end;

(* fib, double, and primes *)

(*
 * val rec lazy fibs =
 *     IS.Cons (0, IS.Cons (1, IS.add (IS.cdr fibs, fibs)))
 * => the fibs does not work (it hangs!)
 *)
val rec lazy fibs = IS.Cons (0, fibs')
and lazy fibs' = IS.Cons (1, IS.add (IS.cdr fibs, fibs));
(*
 * val rec lazy fibs =
 *     IS.Cons (0, IS.Cons (1, IS.add (IS.cdr' fibs, fibs)));
 * => the fibs also works
 *)

val rec lazy double =
    IS.Cons (1, IS.scale (double, 2));

val rec lazy primes =
    let
      fun prime n =
          let
            fun iter ps =
                if I.square (IS.car ps) > n then true
                else if (n mod (IS.car ps)) = 0 then false
                else iter (IS.cdr ps)
          in
            iter primes
          end
    in
      IS.Cons (2, IS.filter prime (IS.integersStartingFrom 3))
    end;

let
  fun show title = IS.display (title ^ ": ", Int.toString, ", ")
in
  show "fibs" fibs 10;
  show "double" double 10;
  show "primes" primes 10;
  print "done\n"
end;

(* Exercise 3.53 *)
val rec lazy double' =
    IS.Cons (1, IS.add (double', double'));

(* Exercise 3.54 *)
val rec lazy factorials =
    IS.Cons (1, IS.mul (factorials, IS.cdr IS.integers));

(* Exercise 3.55 *)
fun partialSums s =
    let
      fun sum s n =
          if n = 0 then 0
          else (IS.car s) + sum (IS.cdr s) (n-1)
    in
      IS.map (sum s) IS.integers
    end;

fun partialSumsR s =
      let
        fun sum s n =
            if n = 0 then 0.0
            else (IS.car s) + sum (IS.cdr s) (n-1)
      in
        IS.map (sum s) IS.integers
      end;

(*
 * Exercise 3.55: another solution borrowed from
 * http://eli.thegreenplace.net/2007/11/05/sicp-sections-351-352/
 *)
fun lazy partialSums' s =
    IS.Cons (IS.car s, IS.add (IS.cdr s, partialSums' s));

fun lazy partialSumsR' s =
    IS.Cons (IS.car s, IS.addR (IS.cdr s, partialSumsR' s));

(* Exercise 3.56 *)
fun lazy merge (s1:int IS.t, s2:int IS.t) =
    let
      val x1 = IS.car s1
      val x2 = IS.car s2
    in
      if x1 < x2 then
        IS.Cons (x1, merge (IS.cdr s1, s2))
      else if x1 > x2 then
        IS.Cons (x2, merge (s1, IS.cdr s2))
      else
        IS.Cons (x1, merge (IS.cdr s1, IS.cdr s2))
    end;

(* Exercise 3.56 (for real) *)
fun lazy mergeR (s1:real IS.t, s2:real IS.t) =
    let
      val x1 = IS.car s1
      val x2 = IS.car s2
    in
      if x1 < x2 then
        IS.Cons (x1, mergeR (IS.cdr s1, s2))
      else if x1 > x2 then
        IS.Cons (x2, mergeR (s1, IS.cdr s2))
      else
        IS.Cons (x1, mergeR (IS.cdr s1, IS.cdr s2))
    end;

val S = IS.Cons (1, merge (IS.scale (IS.integers, 2),
                           merge (IS.scale (IS.integers, 3),
                                  IS.scale (IS.integers, 5))));

(* Exercise 3.58 *)
fun lazy expand (num, den, radix) =
    IS.Cons ((num * radix) div den,
             expand ((num * radix) mod den, den, radix));

let
  fun show title = IS.display (title ^ ": ", Int.toString, ", ")
in
  show "double'" double' 10;
  show "factorials" factorials 10;
  show "S" S 10;
  show "expand(1,7,10)" (expand (1,7,10)) 10;
  show "expand(3,8,10)" (expand (3,8,10)) 10;
  print "done\n"
end;

(* Exercize 3.59 *)
fun integrateSeries s =
    IS.map2 (fn (n, a) => a / n) (IS.integersR, s);

val rec lazy expSeries =
    IS.Cons (1.0, integrateSeries expSeries);

val rec lazy cosSeries =
    IS.Cons (1.0, IS.scaleR (integrateSeries sinSeries, ~1.0))
and lazy sinSeries =
    IS.Cons (0.0, integrateSeries cosSeries);

(*
 * Exercise 3.59: another solution borrowed from
 * http://eli.thegreenplace.net/2007/11/05/sicp-sections-351-352/
 *)
fun integrateSeries' s =
    let
      fun lazy aux (s, n) =
          IS.Cons (IS.car s / n, aux (IS.cdr s, n + 1.0))
    in
      aux (s, 1.0)
    end;

val rec lazy expSeries' =
    IS.Cons (1.0, integrateSeries' expSeries');

val rec lazy cosSeries' =
    IS.Cons (1.0, IS.scaleR (integrateSeries' sinSeries', ~1.0))
and lazy sinSeries' =
    IS.Cons (0.0, integrateSeries' cosSeries');

(* Exercise 3.60 *)
fun lazy mulSeries (s1, s2) =
    IS.Cons ((IS.car s1) * (IS.car s2),
             IS.addR (IS.scaleR (IS.cdr s2, IS.car s1),
                      mulSeries (IS.cdr s1, s2)));

val squareOfCos = mulSeries (cosSeries, cosSeries);
val squareOfSin = mulSeries (sinSeries, sinSeries);

(* Exercise 3.61 *)
fun lazy invertUnitSeries s =
    IS.Cons (1.0, IS.scaleR (mulSeries (IS.cdr s,
                                        invertUnitSeries s),
                             ~1.0));
fun invertSeries s =
    let
      val c = IS.car s (* constarnt term *)
    in
      if Real.== (c, 0.0) then
        raise Fail "constant term of the denominator is zero"
      else
        IS.scaleR (invertUnitSeries (IS.scaleR (s, 1.0 / c)), 1.0 / c)
    end;

val A = IS.scaleR (cosSeries, 2.0);
val B = invertSeries A;
val C = mulSeries (A, B);

(* Exercise 3.62 *)
fun divSeries (sn, sd) = mulSeries (sn, invertSeries sd);

val tanSeries = divSeries (sinSeries, cosSeries);

let
  fun showR title = IS.display (title ^ ": ", Real.toString, ", ")
in
  showR "exp" expSeries 5;
  showR "cos" cosSeries 5;
  showR "sin" sinSeries 5;
  showR "exp'" expSeries' 5;
  showR "cos'" cosSeries' 5;
  showR "sin'" sinSeries' 5;
  showR "cos^2" squareOfCos 5;
  showR "sin^2" squareOfSin 5;
  showR "cos^2 + sin^2" (IS.addR (squareOfCos, squareOfSin)) 5;
  showR "A=2*cos" A 5; (* A: cosX * 2 *)
  showR "B=(2*cos)^-1" B 5; (* B: inverse of A *)
  showR "C=A*B" C 5; (* C: A * B -> must be unit! *)
  showR "tan" tanSeries 5;
  print "done\n"
end;

(* 3.5.3  Exploiting the Stream Paradigm *)

fun sqrtImprove (guess, x) =
    R.average (guess, x / guess)

fun sqrtStream x =
    let
      val rec lazy guesses =
          IS.Cons (1.0,
                   IS.map (fn guess => sqrtImprove (guess, x))
                          guesses)
    in
      guesses
    end;
(*
 * (* Exercise 3.63 *)
 * fun lazy sqrtStream' x =
 *     IS.Cons (1.0,
 *              IS.map (fn guess => sqrtImprove (guess, x))
 *                     (sqrtStream' x));
 *)

(* piSummand n: 1/n - 1/(n+2) + 1/(n+4) - 1/(n+8) + ... *)
fun lazy piSummands n =
    IS.Cons (1.0 / n, IS.map ~ (piSummands (n + 2.0)));

val piStream = IS.scaleR (partialSumsR (piSummands 1.0), 4.0);

fun lazy eulerTransform s =
    let
      val s0 = IS.nth (s, 0) (* S[n-1] *)
      val s1 = IS.nth (s, 1) (* S[n] *)
      val s2 = IS.nth (s, 2) (* S[n+1] *)
    in
      IS.Cons (s2 - (R.square (s2 - s1) / (s0 - (2.0 * s1) + s2)),
               eulerTransform (IS.cdr s))
    end;

fun lazy makeTableau transform s =
    IS.Cons (s, makeTableau transform (transform s));

fun acceleratedSequence transform s =
    IS.map IS.car (makeTableau transform s);

(* Exercise 3.64 *)
fun streamLimit (s, tolerance) =
    let
      val v1 = IS.car s
      val s' = IS.cdr s
      val v2 = IS.car s'
    in
      if R.abs (v1 - v2) < tolerance then
        v2
      else
        streamLimit (IS.cdr s', tolerance)
    end;

fun sqrt (x, tolerance) =
    streamLimit (sqrtStream x, tolerance);

(* ln2Summand n: 1/n - 1/(n+1) + 1/(n+2) - 1/(n+3) + ... *)
fun lazy ln2Summands n =
    IS.Cons (1.0 / n, IS.map ~ (ln2Summands (n + 1.0)));

val ln2Stream = partialSumsR (ln2Summands 1.0);

let
  fun showR title = IS.display (title ^ ":\n", Real.toString, "\n")
in
  showR "sqrt(2.0)" (sqrtStream 2.0) 8;
(*  showR (sqrtStream' "sqrt'(2.0)" 2.0) 8; *)
  print ("sqrt(2,0.0001): " ^ Real.toString (sqrt (2.0, 0.0001)) ^ "\n");
  showR "pi" piStream 8;
  showR "pi'" (eulerTransform piStream) 8;
  showR "pi''" (acceleratedSequence eulerTransform piStream) 8;
  showR "ln2" ln2Stream 8;
  showR "ln2'" (eulerTransform ln2Stream) 8;
  showR "ln2''" (acceleratedSequence eulerTransform ln2Stream) 8;
  print "done\n"
end;

fun lazy interleave (s1, s2) =
    IS.Cons (IS.car s1, interleave (s2, IS.cdr s1));

fun lazy pairs (s1, s2) =
    IS.Cons ((IS.car s1, IS.car s2),
             interleave (IS.map (fn x2 => (IS.car s1, x2))
                                (IS.cdr s2),
                         pairs (IS.cdr s1, IS.cdr s2)));

val integerPairs = pairs (IS.integers, IS.integers);

(* Exercise 3.67 *)
fun lazy allPairs (s1, s2) =
    IS.Cons ((IS.car s1, IS.car s2),
             interleave (IS.map (fn x2 => (IS.car s1, x2))
                                (IS.cdr s2),
                         interleave (IS.map (fn x1 => (x1, IS.car s2))
                                            (IS.cdr s1),
                                     allPairs (IS.cdr s1, IS.cdr s2))));

val integerAllPairs = allPairs (IS.integers, IS.integers);

(* Exercise 3.69 *)
fun lazy triples (s1, s2, s3) =
    let
      fun lazy myPairs (s2, s3) =
          IS.Cons ((IS.car s1, IS.car s2, IS.car s3),
                   interleave (IS.map (fn x3 => (IS.car s1, IS.car s2, x3))
                                      (IS.cdr s3),
                               myPairs (IS.cdr s2, IS.cdr s3)))
    in
      interleave (myPairs (s2, s3),
                  triples (IS.cdr s1, IS.cdr s2, IS.cdr s3))
    end;

val integerTriples = triples (IS.integers, IS.integers, IS.integers);

val pythagoreanTriples = IS.filter (fn (i,j,k) => i*i + j*j = k*k)
                                   integerTriples;

(* Exercise 3.70 *)
fun lazy mergeWeighted (weight: ''a -> int) (s1: ''a IS.t,
                                             s2: ''a IS.t) =
    let
      val x1 = IS.car s1
      val x2 = IS.car s2
      val w1 = weight x1
      val w2 = weight x2
    in
      if w1 < w2 then
        IS.Cons (x1, mergeWeighted weight (IS.cdr s1, s2))
      else if w1 > w2 then
        IS.Cons (x2, mergeWeighted weight (s1, IS.cdr s2))
      else
        if x1 = x2 then
          IS.Cons (x1, mergeWeighted weight (IS.cdr s1, IS.cdr s2))
        else
          IS.Cons (x1, mergeWeighted weight (IS.cdr s1, s2))
    end;

fun lazy weightedPairs weight (s1, s2) =
    IS.Cons ((IS.car s1, IS.car s2),
             mergeWeighted weight
                           (IS.map (fn x2 => (IS.car s1, x2))
                                   (IS.cdr s2),
                            weightedPairs weight (IS.cdr s1, IS.cdr s2)));

val weightedPairsA =
    weightedPairs (fn (i,j) => i + j)
                  (IS.integers, IS.integers);

val weightedPairsB =
    let
      val S = IS.filter (fn i => not (I.divisible (i, 2) orelse
                                      I.divisible (i, 3) orelse
                                      I.divisible (i, 5)))
                        IS.integers;
    in
      weightedPairs (fn (i,j) => 2*i + 3*j + 5*i*j)
                    (S, S)
    end;

(* Exercise 3.71 *)
val ramanujan =
    let
      val weight = (fn (i,j) => i*i*i + j*j*j)
      val S = weightedPairs weight (IS.integers, IS.integers)
      fun lazy find s =
          let
            val p1 = IS.car s
            val w1 = weight p1
            val s' = IS.cdr s
            val p2 = IS.car s'
            val w2 = weight p2
          in
            if w1 = w2 then
              IS.Cons ((w1, p1, p2), find s')
            else
              find s'
          end
    in
      find S
    end;

(* Exercise 3.72 *)
val sumOfTwoSquaresInTreeDifferentWays =
    let
      val weight = (fn (i,j) => i*i + j*j)
      val S = weightedPairs weight (IS.integers, IS.integers)
      fun lazy find s =
          let
            val p1 = IS.car s
            val w1 = weight p1
            val s' = IS.cdr s
            val p2 = IS.car s'
            val w2 = weight p2
            val s'' = IS.cdr s'
            val p3 = IS.car s''
            val w3 = weight p3
          in
            if w1 = w2 andalso w2 = w3 then
              IS.Cons ((w1, p1, p2, p3), find s')
            else
              find s'
          end
    in
      find S
    end;

let
  fun show2 title =
      IS.display (title ^ ": ",
                  (fn (x,y)
                      => "(" ^ Int.toString x ^
                         "," ^ Int.toString y ^
                         ")"),
                  ", ")

  fun show3 title =
      IS.display (title ^ ": ",
                  (fn (x,y,z)
                      => "(" ^ Int.toString x ^
                         "," ^ Int.toString y ^
                         "," ^ Int.toString z ^
                         ")"),
                  ", ")

  fun showW2 title =
      IS.display (title ^ ":\n",
                  (fn (n,(i1,j1),(i2,j2))
                      => "[" ^ Int.toString n ^
                         ", (" ^ Int.toString i1 ^
                         "," ^ Int.toString j1 ^
                         "), (" ^ Int.toString i2 ^
                         "," ^ Int.toString j2 ^
                         ")]"),
                  "\n")

  fun showW3 title =
      IS.display (title ^ ":\n",
                  (fn (n,(i1,j1),(i2,j2),(i3,j3))
                      => "[" ^ Int.toString n ^
                         ", (" ^ Int.toString i1 ^
                         "," ^ Int.toString j1 ^
                         "), (" ^ Int.toString i2 ^
                         "," ^ Int.toString j2 ^
                         "), (" ^ Int.toString i3 ^
                         "," ^ Int.toString j3 ^
                         ")]"),
                  "\n")
in
  show2 "intPairs" integerPairs 20;
  show2 "intAllPairs" integerAllPairs 20;
  show3 "intTriples" integerTriples 20;
  show3 "pythagoreanTriples" pythagoreanTriples 4;
  show2 "weightedPairsA" weightedPairsA 20;
  show2 "weightedPairsB" weightedPairsB 20;
  showW2 "ramanujan" ramanujan 6;
  showW3 "sumOfTwoSquaresInTreeDifferentWays"
         sumOfTwoSquaresInTreeDifferentWays 6;
  print "done\n"
end;

fun integral (integrand, initialValue, dt) =
    let
      val rec lazy int =
          IS.Cons (initialValue,
                   IS.addR (IS.scaleR (integrand, dt), int))
    in
      int
    end;

(* Exercise 3.73 *)
fun rc (R, C, dt) =
    (fn (i, v0) =>
        IS.addR (IS.scaleR (i, R),
                 integral (IS.scaleR (i, 1.0 / C), v0, dt)));

(* Exercise 3.74 *)
fun makeSenseData (a, f, dt) =
    let
      val f' = 2.0 * Math.pi * f
      fun lazy seq t =
          IS.Cons (a * Math.sin (f' * t), seq (t + dt))
    in
      seq 0.0
    end;

val senseData = makeSenseData (1.0, 1.0, 0.2);

fun signChangeDetector (x:real, y:real) =
    (* Assume that the sign of a 0 input is positive. *)
    if x >= 0.0 andalso y < 0.0 then ~1
    else if x < 0.0 andalso y >= 0.0 then 1
    else 0;

fun lazy makeZeroCrossings (input, lastValue) =
    IS.Cons (signChangeDetector (IS.car input, lastValue),
             makeZeroCrossings (IS.cdr input, IS.car input));

val zeroCrossings1 = makeZeroCrossings (senseData, 0.0);

val zeroCrossings2 =
    let
      val lazy senseData' = IS.Cons (0.0, senseData)
    in
      IS.map2 signChangeDetector (senseData, senseData')
    end;

(* Exercise 3.75 *)
fun lazy makeZeroCrossingsWithSmoothing (input, lastValue, lastAvpt) =
    let
      val value = IS.car input
      val avpt = (value + lastValue) / 2.0
    in
      IS.Cons (signChangeDetector (avpt, lastAvpt),
               makeZeroCrossingsWithSmoothing (IS.cdr input, value, avpt))
    end;

val zeroCrossings3 =
    makeZeroCrossingsWithSmoothing (senseData, 0.0, 0.0);

(* Exercise 3.76 *)
(*
 * fun makeZeroCrossings' input =
 *     let
 *       val lazy input' = IS.Cons (0.0, input)
 *     in
 *       IS.map2 signChangeDetector (input, input')
 *     end;
 * 
 * fun smooth input =
 *     let
 *       val lazy input' = IS.Cons (0.0, input)
 *     in
 *       IS.map2 (fn (v1,v2) => (v1 + v2) / 2.0) (input, input')
 *     end;
 * 
 * val zeroCrossings4 = (makeZeroCrossings' o smooth) senseData;
 *)

fun mapInput proc input =
    let
      val lazy input' = IS.Cons (0.0, input)
    in
      IS.map2 proc (input, input')
    end;

val zeroCrossings4 =
    ((mapInput signChangeDetector) o
     (mapInput (fn (v1,v2) => (v1 + v2) / 2.0))) senseData;

let
  fun show title = IS.display (title ^ ": ", Int.toString, ", ")
  fun showR title = IS.display (title ^ ": ", Real.toString, ", ")
in
  showR "rc" (rc (5.0, 1.0, 0.5) (IS.onesR, 0.0)) 20;
  showR "senseData" senseData 20;
  show "zeroCrossings1" zeroCrossings1 20;
  show "zeroCrossings2" zeroCrossings2 20;
  show "zeroCrossings3" zeroCrossings3 20;
  show "zeroCrossings4" zeroCrossings4 20;
  print "done\n"
end;

(* 3.5.4  Streams and Delayed Evaluation *)

fun solve (f, y0, dt) =
    let
      val rec lazy y = integral (dy, y0, dt)
      and lazy dy = IS.map f y
    in
      y
    end;
(*
 * let
 *   fun showR title = IS.display (title ^ ":\n", Real.toString, "\n")
 * in
 *   showR "integral" (integral (IS.onesR, 0.0, 0.05)) 100;
 *   showR "dy = y with y0 = 1 -> y = exp(x)"
 *         (solve (fn x => x, 1.0, 0.05)) 100;
 *   print "done\n"
 * end;
 *)
IS.nth (solve (fn x => x, 1.0, 0.001), 1000);

(* Exercise 3.77 *)
(*
 * fun lazy integral' (integrand, initialValue:real, dt) =
 *     let
 *       val _ = print "."
 *       val lazy x = 
 *     in
 *     IS.Cons (initialValue,
 *              integral' (IS.cdr integrand,
 *                         dt * (IS.car integrand) + initialValue,
 *                         dt))
 *     end;
 * => when integral' is used in solve' (defined below)
 *    it does not work (it hangs!)
 *)
fun lazy integral' (integrand, initialValue:real, dt) =
    let
      val lazy rest =
          integral' (IS.cdr integrand,
                     dt * (IS.car integrand) + initialValue,
                     dt)
    in
      IS.Cons (initialValue, rest)
    end;

fun solve' (f, y0, dt) =
    let
      val rec lazy y = integral' (dy, y0, dt)
      and lazy dy = IS.map f y
    in
      y
    end;
(*
 * let
 *   fun showR title = IS.display (title ^ ":\n", Real.toString, "\n")
 * in
 *   showR "integral'" (integral' (IS.onesR, 0.0, 0.05)) 100;
 *   showR "dy = y with y0 = 1 -> y = exp(x)"
 *         (solve' (fn x => x, 1.0, 0.05)) 100;
 *   print "done\n"
 * end;
 *)
IS.nth (solve' (fn x => x, 1.0, 0.001), 1000);

(* Exercise 3.78 *)
fun solve2ndLinear (a, b, y0, dy0, dt) =
    let
      val rec lazy y = integral (dy, y0, dt)
      and lazy dy = integral (ddy, dy0, dt)
      and lazy ddy = IS.addR (IS.scaleR (dy, a), IS.scaleR (y, b))
    in
      y
    end;

(* Exercise 3.79 *)
fun solve2nd (f, y0, dy0, dt) =
    let
      val rec lazy y = integral (dy, y0, dt)
      and lazy dy = integral (ddy, dy0, dt)
      and lazy ddy = IS.map2 f (dy, y)
    in
      y
    end;

(* Exercise 3.80 *)
fun rlc (R, L, C, dt) =
    (fn (vC0, iL0) =>
        let
          val rec lazy vC = integral (dvC, vC0, dt)
          and lazy dvC = IS.scaleR (iL, ~1.0 / C)
          and lazy iL = integral (diL, iL0, dt)
          and lazy diL = IS.addR (IS.scaleR (vC, 1.0 / L),
                                  IS.scaleR (iL, ~ R / L))
        in
          (vC, iL)
        end);

let
  fun showR title = IS.display (title ^ ":\n", Real.toString, "\n")
  fun show2R title =
      IS.display (title ^ ":\n",
                  (fn (x,y)
                      => "(" ^ Real.toString x ^
                         "," ^ Real.toString y ^
                         ")"),
                  "\n")
in
  showR "ddy + y = 0 with y0 = 0, dy0 = 1 -> y = sin(x)"
        (solve2ndLinear (0.0, ~1.0, 0.0, 1.0, 0.05)) 100;
  show2R "rlc (vC,iL)"
         (IS.map2 (fn p => p) (rlc (1.0,1.0,0.2,0.1) (10.0,0.0))) 100;
  print "done\n"
end;

(* 3.5.5  Modularity of Functional Programs and Modularity of Objects *)

(* randomInit, randUpdate: => included in chap3_1.sml *)

val rec lazy randomNumbers =
    IS.Cons (randomInit, IS.map randUpdate randomNumbers);

fun lazy mapSuccessivePairs f s =
    IS.Cons (f (IS.car s, IS.car (IS.cdr s)),
             mapSuccessivePairs f (IS.cdr (IS.cdr s)));

val cesaroStream =
    mapSuccessivePairs (fn (r1, r2) => (I.gcd (r1, r2) = 1))
                       randomNumbers;

fun genMonteCarloStream (experimentStream, passed, failed) =
    let
      fun lazy next (passed, failed) =
          IS.Cons ((real passed) / (real (passed + failed)),
                   genMonteCarloStream (IS.cdr experimentStream,
                                        passed, failed))
    in
      if IS.car experimentStream then
        next (passed + 1, failed)
      else
        next (passed, failed + 1)
    end;

val piStream' =
    IS.map (fn p => Math.sqrt (6.0 / p))
           (genMonteCarloStream (cesaroStream, 0, 0));

(* Exercise 3.81 *)
datatype randomReq = Generate | Reset of int;

(* randomInit, randUpdate: => included in chap3_1.sml *)

fun genRandomNumbers requests =
    let
      fun controller (Generate, rand) = randUpdate rand
        | controller (Reset seed,  _) = seed
      val rec lazy randomNumbers =
          IS.Cons (randomInit,
                   IS.map2 controller (requests, randomNumbers))
    in
      randomNumbers
    end;

val rec lazy req1 =
    IS.Cons (Generate, req1);

val rec lazy req2 =
    IS.Cons (Generate,
             IS.Cons (Generate,
                      IS.Cons (Reset randomInit, req2)));

(* Exercise 3.82 *)
(* randomInRange: => included in chap3_1.sml *)

fun genEstimateIntegralStream p (x1, x2, y1, y2) =
    let
      val w = x2 - x1
      val h = y2 - y1
      val S = real (w * h)
      fun lazy experiment () =
          let
            val point = (randomInRange (x1, x2),
                         randomInRange (y1, y2))
          in
            IS.Cons (p point, experiment ())
          end
    in
      IS.map (fn p => S * p)
             (genMonteCarloStream (experiment (), 0, 0))
    end;

(* regionTest: => included in chap3_1.sml *)

val estimateIntegralStream =
    genEstimateIntegralStream regionTest (2,8,4,10);

let
  fun show title = IS.display (title ^ ": ", Int.toString, ", ")
  fun showR title = IS.display (title ^ ":\n", Real.toString, "\n")
in
  showR "pi'" piStream' 30;
  show "rand" randomNumbers 30;
  show "rand(req1)" (genRandomNumbers req1) 30;
  show "rand(req2)" (genRandomNumbers req2) 30;
  showR "integral" estimateIntegralStream 30;
  print "done\n"
end;
