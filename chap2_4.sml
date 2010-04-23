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
(* 2.4  Multiple Representations for Abstract Data *)
(* 2.4.1  Representations for Complex Numbers *)

signature COMPLEX =
sig
  type t
  val makeFromRealImag : real * real -> t
  val makeFromMagAng : real * real -> t
  val realPart : t -> real
  val imagPart : t -> real
  val magnitude : t -> real
  val angle : t -> real
end;

functor ComplexOpsFn (C : COMPLEX) =
struct
  open C

  val ZERO = makeFromRealImag (0.0,0.0); (* 0 *)
  val ONE = makeFromRealImag (1.0,0.0); (* 1 *)
  val I = makeFromRealImag (0.0,1.0); (* i *)

  fun addComplex (z1, z2) =
      makeFromRealImag ((realPart z1) + (realPart z2),
                        (imagPart z1) + (imagPart z2))

  fun subComplex (z1, z2) =
      makeFromRealImag ((realPart z1) - (realPart z2),
                        (imagPart z1) - (imagPart z2))

  fun mulComplex (z1, z2) =
      makeFromMagAng ((magnitude z1) * (magnitude z2),
                      (angle z1) + (angle z2))

  fun divComplex (z1, z2) =
      makeFromMagAng ((magnitude z1) / (magnitude z2),
                      (angle z1) - (angle z2))

  fun toConjugate z =
      makeFromRealImag ((realPart z), ~1.0 * (imagPart z));

  fun toString z = "(" ^ Real.toString (realPart z) ^
                   " " ^ Real.toString (imagPart z) ^ ")"
end;

(* Rectangular representation *)
structure Complex' : COMPLEX = struct
  type t = real * real

  fun makeFromRealImag (x, y) = (x, y)

  fun makeFromMagAng (r, a) = (r * (Math.cos a), r * (Math.sin a))

  fun realPart (x, _) = x

  fun imagPart (_, y) = y

  fun magnitude (x, y) =
      Math.sqrt ((R.square x) + (R.square y))

  fun angle (x, y) = Math.atan2 (y, x)
end;

structure CO = ComplexOpsFn (Complex');
CO.toString (CO.mulComplex (CO.I, CO.I));
CO.toString (CO.toConjugate CO.ONE);
CO.toString (CO.toConjugate CO.I);

(* Polar representation *)
structure Complex'' : COMPLEX = struct
  type t = real * real

  fun makeFromRealImag (x, y) =
      (Math.sqrt ((R.square x) + (R.square y)), Math.atan2 (y, x))

  fun makeFromMagAng (r, a) = (r, a)

  fun realPart (r, a) = r * (Math.cos a)

  fun imagPart (r, a) = r * (Math.sin a)

  fun magnitude (r, _) = r

  fun angle (_, a) = a
end;

structure CO = ComplexOpsFn (Complex'');
CO.toString (CO.mulComplex (CO.I, CO.I));
CO.toString (CO.toConjugate CO.ONE);
CO.toString (CO.toConjugate CO.I);

(* 2.4.2  Tagged data *)

(* Generic operations with explicit dispatch *)
structure Complex''' :> COMPLEX = struct
  datatype t = Rectangular of real * real
             | Polar of real * real

  structure R = Complex'
  structure P = Complex''

  val makeFromRealImag = Rectangular

  val makeFromMagAng = Polar

  fun realPart (Rectangular (x, y)) = R.realPart (x, y)
    | realPart (Polar (r, a)) = P.realPart (r, a)

  fun imagPart (Rectangular (x, y)) = R.imagPart (x, y)
    | imagPart (Polar (r, a)) = P.imagPart (r, a)

  fun magnitude (Rectangular (x, y)) = R.magnitude (x, y)
    | magnitude (Polar (r, a)) = P.magnitude (r, a)

  fun angle (Rectangular (x, y)) = R.angle (x, y)
    | angle (Polar (r, a)) = P.angle (r, a)
end;

structure CO = ComplexOpsFn (Complex''');
CO.toString (CO.mulComplex (CO.I, CO.I));
CO.toString (CO.toConjugate CO.ONE);
CO.toString (CO.toConjugate CO.I);

(* 2.4.3  Data-Directed Programming and Additivity *)

(* Data-directed style => skip *)

(* Message passing style *)
structure Complex'''' :> COMPLEX = struct
  datatype msg = RealPart
               | ImagPart
               | Magnitude
               | Angle

  type t = msg -> real

  structure R = Complex'
  structure P = Complex''

  fun makeFromRealImag (x, y) =
   fn RealPart => R.realPart (x, y)
    | ImagPart => R.imagPart (x, y)
    | Magnitude => R.magnitude (x, y)
    | Angle => R.angle (x, y)

  fun makeFromMagAng (r, a) =
   fn RealPart => P.realPart (r, a)
    | ImagPart => P.imagPart (r, a)
    | Magnitude => P.magnitude (r, a)
    | Angle => P.angle (r, a)

  fun realPart z = z RealPart

  fun imagPart z = z ImagPart

  fun magnitude z = z Magnitude

  fun angle z = z Angle
end;

structure CO = ComplexOpsFn (Complex'''');
CO.toString (CO.mulComplex (CO.I, CO.I));
CO.toString (CO.toConjugate CO.ONE);
CO.toString (CO.toConjugate CO.I);

