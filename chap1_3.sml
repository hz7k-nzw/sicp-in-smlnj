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
(* 1.3  Formulating Abstractions with Higher-Order Procedures *)
(* 1.3.1  Procedures as Arguments *)

(*
 * fun cube x = x * x * x;
 * => included in util.sml
 *)

fun sumIntegers (a, b) =
    if a > b then 0
    else a + sumIntegers (a + 1, b);

sumIntegers (1, 10);

fun sumCubes (a, b) =
    if a > b then 0
    else I.cube a + sumCubes (a + 1, b);

sumCubes (1, 10);

fun piSum (a, b) =
    if a > b then 0.0
    else (1.0 / a * (a + 2.0)) + piSum (a + 4.0, b);

8.0 * piSum (1.0, 1000.0);

fun sum term next (a, b) =
    if a > b then 0
    else term a + sum term next (next a, b);

fun sum' term next (a:real, b:real) =
    if a > b then 0.0
    else term a + sum' term next (next a, b);

fun inc n = n + 1;

fun sumCubes (a, b) = sum I.cube inc (a, b);

sumCubes (1, 10);

(*
 * fun identity x = x;
 * => included in util.sml
 *)

fun sumIntegers (a, b) = sum U.identity inc (a, b);

(*
 * val sumIntegers = sum U.identity inc;
 *)

sumIntegers (1, 10);

fun piSum (a, b) =
    let
      fun piTerm x = 1.0 / (x * (x + 2.0))
      and piNext x = x + 4.0
    in
      sum' piTerm piNext (a, b)
    end;

8.0 * piSum (1.0, 1000.0);

fun integral (f, a, b, dx) =
    let
      fun addDx x = x + dx
    in
      sum' f addDx (a + (dx / 2.0), b) * dx
    end;

integral (R.cube, 0.0, 1.0, 0.01);

integral (R.cube, 0.0, 1.0, 0.001);

(* 1.3.2  Constructing Procedures Using Lambda *)

fun piSum (a, b) =
    sum' (fn x => 1.0 / (x * (x + 2.0))) (fn x => x + 4.0) (a, b);

8.0 * piSum (1.0, 1000.0);

fun integral (f, a, b, dx) =
    sum' f (fn x => x + dx) (a + (dx / 2.0), b) * dx;

integral (R.cube, 0.0, 1.0, 0.001);

fun plus4 x = x + 4;

val plus4 = fn x => x + 4;

(fn (x, y, z) => x + y + (I.square z)) (1, 2, 3);

fun f (x, y) =
    let
      fun f_helper (a, b) = (x * (I.square a)) + (y * b) + (a * b)
    in
      f_helper (1 + (x * y), 1 - y)
    end;

fun f (x, y) =
    (fn (a, b) => (x * (I.square a)) + (y * b) + (a * b))
        (1 + (x * y), 1 - y);

fun f (x, y) =
    let
      val a = 1 + (x * y)
      val b = 1 - y
    in
      (x * (I.square a)) + (y * b) + (a * b)
    end;

val x = 5;
let val x = 3 in x + (x * 10) end + x;

val x = 2;
let val x = 3 and y = x + 2 in x * y end;

(* 1.3.3  Procedures as General Methods *)

fun closeEnough (x, y) = R.abs (x - y) < 0.001;

fun search f (negPoint, posPoint) =
    let
      val midpoint = R.average (negPoint, posPoint)
    in
      if closeEnough (negPoint, posPoint) then midpoint
      else
        let
          val testValue = f midpoint
        in
          if R.positive testValue then
            search f (negPoint, midpoint)
          else if R.negative testValue then
            search f (midpoint, posPoint)
          else
            midpoint
        end
    end;

fun halfIntervalMethod f (a, b) =
    let
      val aValue = f a and bValue = f b
    in
      if R.negative aValue andalso R.positive bValue then
        search f (a, b)
      else if R.negative bValue andalso R.positive aValue then
        search f (b, a)
      else
        raise Fail "Not opposite sign"
    end;

halfIntervalMethod Math.sin (2.0, 4.0);

halfIntervalMethod (fn x => (x * x * x) - (2.0 * x) - 3.0)
                   (1.0, 2.0);

val tolerance = 0.00001;

fun fixedPoint f firstGuess =
    let
      fun closeEnough (v1, v2) =
          R.abs (v1 - v2) < tolerance
      fun try guess =
          let
            val next = f guess
          in
            if closeEnough (guess, next) then next
            else try next
          end
    in
      try firstGuess
    end;

fixedPoint Math.cos 1.0;

fixedPoint (fn y => Math.sin y + Math.cos y) 1.0;

(*
 * fun sqrt x = fixedPoint (fn y => x / y) 1.0;
 *)

fun sqrt x =
    fixedPoint (fn y => R.average (y, x / y)) 1.0;

(* 1.3.4  Procedures as Returned Values *)

fun averageDamp f = fn x => R.average (x, f x);

averageDamp R.square 10.0;

fun sqrt x =
    fixedPoint (averageDamp (fn y => x / y)) 1.0;

fun cubeRoot x =
    fixedPoint (averageDamp (fn y => x / (R.square y))) 1.0;

local
  val dx = 0.00001
in
fun deriv g = (fn x => (g (x + dx) - g x) / dx);
end;

deriv R.cube 5.0;

fun newtonTransform g =
    (fn x => x - ((g x) / (deriv g x)));

fun newtonsMethod g guess =
    fixedPoint (newtonTransform g) guess;

fun sqrt x =
    newtonsMethod (fn y => (R.square y) - x) 1.0;

sqrt 2.0;

fun fixedPointOfTransform g transform guess =
    fixedPoint (transform g) guess;

fun sqrt x =
    fixedPointOfTransform (fn y => x / y) averageDamp 1.0;

sqrt 2.0;

fun sqrt x =
    fixedPointOfTransform (fn y => (R.square y) - x)
                          newtonTransform 1.0;

sqrt 2.0;
