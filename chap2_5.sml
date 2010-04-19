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
(* 2.5  Systems with Generic Operations *)
(* 2.5.1  Generic Arithmetic Operations *)

(* 2.5.2  Combining Data of Different Types *)

(* 2.5.3  Example: Symbolic Algebra *)

signature ARITH =
sig
  type t

  val ZERO : t
  val ONE : t

  val add : t * t -> t
  val sub : t * t -> t
  val mul : t * t -> t
  val quo : t * t -> t (* a/b = a*(1/b) *)
  val eq : t * t -> bool
  val lt : t * t -> bool
  val gt : t * t -> bool
  val le : t * t -> bool
  val ge : t * t -> bool
  val neg : t -> t
  val inv : t -> t
  val normalize : t -> t
  val normalizeRatio : t * t -> t * t
  val fromInt : int -> t
  val fromReal : real -> t
  val toInt : t -> int
  val toReal : t -> real
  val toString : t -> string
end;

signature ARITH_WITH_EUCLID =
sig
  include ARITH
  val divAndRem : t * t -> t * t (* a = b*q + r *)
  val gcd : t * t -> t (* calc gcd by Euclidean algorithm *)
  val normalizeGcd : t -> t
end;

structure IntArith : ARITH_WITH_EUCLID =
struct
  type t = int

  val ZERO = 0 : t
  val ONE = 1 : t

  val add = op + : t * t -> t
  val sub = op - : t * t -> t
  val mul = op * : t * t -> t
  fun quo _ = raise Fail "not supported: quo"
  val eq = op = : t * t -> bool
  val lt = op < : t * t -> bool
  val gt = op > : t * t -> bool
  val le = op <= : t * t -> bool
  val ge = op >= : t * t -> bool
  val neg = op ~ : t -> t
  fun inv _ = raise Fail "not supported: inv"

  fun normalize (a:t) = a

  and normalizeRatio (n, d) =
      let
        val (n', d') =
            if lt (ZERO, d) then (n, d)
            else (neg n, neg d)
        val g = (normalizeGcd o gcd) (n', d')
      in
        (Int.quot (n', g), Int.quot (d', g))
      end

  and fromInt (x:int) = x

  (*and fromReal (x:real) = Real.trunc x*)
  and fromReal (_:real) = raise Fail "not supported: fromReal"

  and toInt (x:t) = x

  and toReal (x:t) = real x

  and toString (x:t) = Int.toString x

  and divAndRem (a, b) =
      (Int.quot (a, b), Int.rem (a, b))

  and gcd (a, b) =
      if eq (b, ZERO) then a
      else gcd (b, Int.rem (a, b))

  and normalizeGcd (x:t) = Int.abs x
end;

structure RealArith : ARITH =
struct
  type t = real

  val ZERO = 0.0 : t
  val ONE = 1.0 : t

  val add = op + : t * t -> t
  val sub = op - : t * t -> t
  val mul = op * : t * t -> t
  val quo = op / : t * t -> t
  (*fun eq (a:t, b:t) = a <= b andalso b <= a*)
  val eq = Real.== : t * t -> bool
  val lt = op < : t * t -> bool
  val gt = op > : t * t -> bool
  val le = op <= : t * t -> bool
  val ge = op >= : t * t -> bool
  val neg = op ~ : t -> t
  val inv = fn x:t => quo (ONE, x)

  fun normalize (a:t) = a

  and normalizeRatio (n:t, d:t) =
      if lt (ZERO, d) then (n, d)
      else (neg n, neg d)

  and fromInt (x:int) = real x

  and fromReal (x:real) = x

  (*and toInt (x:t) = Real.trunc x*)
  and toInt (_:t) = raise Fail "not supported: toInt"

  and toReal (x:t) = x

  and toString (x:t) = Real.toString x
end;

(*
 * N : int must be a positive integer.
 * If N is a prime number, then this functor returns a finite field.
 *)
functor IntModArithFn (val N :int) : ARITH =
struct
  type t = int

  val ZERO = 0 : t
  val ONE = 1 : t

  fun add (a:t, b:t) = normalize (a + b)
  and sub (a:t, b:t) = normalize (a - b)
  and mul (a:t, b:t) = normalize (a * b)
  and quo (a:t, b:t) = mul (a, inv b)
  and eq (a:t, b:t) = normalize (a - b) = 0
  and lt (_:t, _:t) = raise Fail "not supported: lt"
  and gt (_:t, _:t) = raise Fail "not supported: gt"
  and le (_:t, _:t) = raise Fail "not supported: le"
  and ge (_:t, _:t) = raise Fail "not supported: ge"
  and neg (a:t) = normalize (~a)
  (*and inv (_:t) = raise Fail "not supported: inv"*)
  and inv (a:t) =
      let
        val a' = normalize a
        fun findInv i =
            if N <= i then raise Fail "inv not found"
            else if mul (a', i) = 1 then i
            else findInv (i + 1)
      in
        if a' = 0 then raise Div
        else findInv 1
      end

  and normalize (a:t) =
      let
        val v = Int.rem (a, N)
      in
        if v < 0 then N + v else v
      end

  and normalizeRatio (n:t, d:t) = (normalize n, normalize d)

  and fromInt (a:int) = normalize a

  and fromReal (_:real) = raise Fail "not supported: fromReal"

  and toInt (a:t) = normalize a

  and toReal (_:t) = raise Fail "not supported: toReal"

  and toString (a:t) = Int.toString (normalize a)
                       (*^ "(mod" ^ Int.toString N ^ ")"*)
end;

(*
 * A : ARITH must be a commutative ring.
 * If A is an integral domain, then this functor returns a field.
 *)
functor QuotientArithFn (structure A : ARITH) : ARITH =
struct
  type t = A.t * A.t

  (* ZERO: 0/1 *)
  val ZERO = (A.ZERO, A.ONE) : t
  (* ONE: 1/1 *)
  and ONE = (A.ONE, A.ONE) : t

  (* add: a/b + c/d = (ad + cb)/bd *)
  fun add ((a, b), (c, d)) =
      normalize (A.add (A.mul (a, d), A.mul (c, b)), A.mul (b, d))

  (* sub: a/b - c/d = (ad - cb)/bd *)
  and sub ((a, b), (c, d)) =
      normalize (A.sub (A.mul (a, d), A.mul (c, b)), A.mul (b, d))

  (* mul: a/b * c/d = ac/bd *)
  and mul ((a, b), (c, d)) =
      normalize (A.mul (a, c), A.mul (b, d))

  (* quo: a/b / c/d = ad/bc *)
  and quo ((a, b), (c, d)) =
      normalize (A.mul (a, d), A.mul (b, c))

  and eq (x, y) =
      let
        val (a, b) = normalize x
        val (c, d) = normalize y
      in
        A.eq (A.mul (a, d), A.mul (b, c))
      end

  and lt (x, y) =
      let
        val (a, b) = normalize x
        val (c, d) = normalize y
      in
        A.lt (A.mul (a, d), A.mul (b, c))
      end

  and gt (x, y) =
      let
        val (a, b) = normalize x
        val (c, d) = normalize y
      in
        A.gt (A.mul (a, d), A.mul (b, c))
      end

  and le (x, y) =
      let
        val (a, b) = normalize x
        val (c, d) = normalize y
      in
        A.le (A.mul (a, d), A.mul (b, c))
      end

  and ge (x, y) =
      let
        val (a, b) = normalize x
        val (c, d) = normalize y
      in
        A.ge (A.mul (a, d), A.mul (b, c))
      end

  and neg (a, b) = normalize (A.neg a, b)

  and inv (a, b) = normalize (b, a)

  and normalize (a, b) =
      if A.eq (b, A.ZERO) then raise Div
      else A.normalizeRatio (a, b)

  and normalizeRatio (n:t, d:t) =
      if lt (ZERO, d) then (normalize n, normalize d)
      else (neg n, neg d)

  and fromInt (a:int) = (A.fromInt a, A.ONE)

  and fromReal (a:real) = (A.fromReal a, A.ONE)

  (* and toInt (a, b) =
      Int.quot (A.toInt a, A.toInt b) *)
  and toInt (a, b) = raise Fail "not supported: toInt"

  and toReal (a, b) = A.toReal a / A.toReal b

  and toString (a, b) = "(" ^ A.toString a ^ "/" ^ A.toString b ^ ")"
end;

functor GaussArithFn (structure A : ARITH) : ARITH_WITH_EUCLID =
struct
  type t = A.t * A.t

  (* ZERO: 0 + i0 *)
  val ZERO = (A.ZERO, A.ZERO) : t
  (* ONE: 1 + i0 *)
  val ONE = (A.ONE, A.ZERO) : t

  (* (a + ib) + (c + id) = (a + c) + i(b + d) *)
  fun add ((a, b), (c, d)) =
      normalize (A.add (a, c), A.add (b, d))

  (* (a + ib) - (c + id) = (a - c) + i(b - d) *)
  and sub ((a, b), (c, d)) =
      normalize (A.sub (a, c), A.sub (b, d))

  (* (a + ib) * (c + id) = (ac - bd) + i(ad + bc) *)
  and mul ((a, b), (c, d)) =
      normalize (A.sub (A.mul (a, c), A.mul (b, d)),
                 A.add (A.mul (a, d), A.mul (b, c)))

  (* (a + ib) / (c + id) = {(ac + bd)/(cc + dd)} + i{(bc - ad)/(cc + dd)} *)
  and quo ((a, b), (c, d)) =
      let
        val denom = A.add (A.mul (c, c), A.mul (d, d))
      in
        normalize (A.quo (A.add (A.mul (a, c), A.mul (b, d)), denom),
                   A.quo (A.sub (A.mul (b, c), A.mul (a, d)), denom))
      end

  and eq ((a, b), (c, d)) = A.eq (a, c) andalso A.eq (b, d)

  and lt (_:t, _:t) = raise Fail "not supported: lt"
  and gt (_:t, _:t) = raise Fail "not supported: gt"
  and le (_:t, _:t) = raise Fail "not supported: le"
  and ge (_:t, _:t) = raise Fail "not supported: ge"

  and neg (a, b) = (A.neg a, A.neg b)

  and inv (z:t) = quo (ONE, z)

  and normalize (a, b) = (A.normalize a, A.normalize b)

  and normalizeRatio (n, d) =
      let
        val (n', d') = (normalize n, normalize d)
        val g = (normalizeGcd o gcd) (n', d')
        val (n'', _) = divAndRem (n', g)
        val (d'', _) = divAndRem (d', g)
      in
        (n'', d'')
      end

  and fromInt (a:int) = (A.fromInt a, A.ZERO)

  and fromReal (a:real) = (A.fromReal a, A.ZERO)

  and toInt (_:t) = raise Fail "not supported: toInt"

  and toReal (_:t) = raise Fail "not supported: toReal"

  and toString (a, b) = "(" ^ A.toString a ^
                         "+i" ^ A.toString b ^ ")"

  and divAndRem (z1 as (a, b), z2 as (c, d)) =
      if eq (z2, ZERO) then raise Div
      else if eq (z1, ZERO) then (ZERO, ZERO)
      else
        let
          (* 1. calc z1/z2 = (nume1/denom) + i(nume2/denom). *)
          val denom = A.toReal (A.add (A.mul (c, c), A.mul (d, d)))
          val nume1 = A.toReal (A.add (A.mul (a, c), A.mul (b, d)))
          val nume2 = A.toReal (A.sub (A.mul (b, c), A.mul (a, d)))
          (* 2. get integer m and n nearest to nume[12]/denom. *)
          val m = A.fromInt (Real.round (nume1 / denom))
          val n = A.fromInt (Real.round (nume2 / denom))
          (* 3. make complex number q = m + in. *)
          val q = normalize (m, n)
          (* 4. calc complex number r = z1 - q*z2. *)
          val r = sub (z1, mul (z2, q))
        in
          (q, r)
        end

  and gcd (a:t, b:t) =
      if eq (b, ZERO) then a
      else let val (_,r) = divAndRem (a, b)
           in gcd (b, r) end

  and normalizeGcd (a:t) = a
end;

functor PolyArithFn (val V : string; structure A : ARITH)
        : ARITH_WITH_EUCLID =
struct
  type t = (A.t * int) list

  (* ZERO: [] *)
  val ZERO = [] : t
  (* ONE: [(1, 0)] *)
  val ONE = [(A.ONE, 0)] : t

  fun add (nil, p2) = p2
    | add (p1, nil) = p1
    | add (p1 as ((t1 as (c1, o1))::ts1),
           p2 as ((t2 as (c2, o2))::ts2)) =
      if o2 < o1 then adjoinTerm (t1, add (ts1, p2))
      else if o1 < o2 then adjoinTerm (t2, add (p1, ts2))
      else adjoinTerm ((A.add (c1, c2), o1), add (ts1, ts2))

  and sub (p1, p2) = add (p1, neg p2)

  and mul (nil, _) = ZERO
    | mul (t1::ts1, p2) =
      add (mulTermByAllTerms (t1, p2), mul (ts1, p2))

  and quo (_, _) = raise Fail "not supported: quo"

  and eq (nil, nil) = true
    | eq (nil, _) = false
    | eq (_, nil) = false
    | eq (t1::ts1, t2::ts2) =
      eqTerm (t1, t2) andalso eq (ts1, ts2)

  and lt (_, _) = raise Fail "not supported: lt"
  and gt (_, _) = raise Fail "not supported: gt"
  and le (_, _) = raise Fail "not supported: le"
  and ge (_, _) = raise Fail "not supported: ge"

  and neg nil = ZERO
    | neg (t::ts) = negTerm t :: neg ts

  and inv p = quo (ONE, p)

  and normalize p =
      List.filter (fn (coeff, _) => not (A.eq (coeff, A.ZERO))) p

  and normalizeRatio (n, d) =
      let
        val (n', d') = (normalize n, normalize d)
        val g = (normalizeGcd o gcd) (n', d')
        val (n'', _) = divAndRem (n', g)
        val (d'', _) = divAndRem (d', g)
      in
        (n'', d'')
      end

  and fromInt (a:int) = [(A.fromInt a, 0)]

  and fromReal (a:real) = [(A.fromReal a, 0)]

  and toInt (_:t) = raise Fail "not supported: toInt"

  and toReal (_:t) = raise Fail "not supported: toReal"

  and toString p =
      let
        fun s1 nil = "[]"
          | s1 (t::ts) = "[" ^ termToString t ^ s2 ts
        and s2 nil = "]"
          | s2 (t::ts) = "+" ^ termToString t ^ s2 ts
      in
        s1 (normalize p)
      end

  and divAndRem (_, nil) = raise Div
    | divAndRem (nil, _) = (ZERO, ZERO)
    | divAndRem (p1 as ((c1, o1)::ts1),
                 p2 as ((c2, o2)::ts2)) =
      if o1 < o2 then
        (ZERO, p1)
      else
        let
          val term = (A.quo (c1, c2), o1 - o2)
          val diff = sub (p1, normalize (mulTermByAllTerms (term, p2)))
          val (q, r) = divAndRem (diff, p2)
        in
          (adjoinTerm (term, q), r)
        end

  and gcd (a:t, b:t) =
      if eq (b, ZERO) then a
      else let val (_, r) = divAndRem (a, b)
           in gcd (b, r) end

  and normalizeGcd nil = ZERO
    | normalizeGcd (p as ((coeff, order)::ts)) =
      let val (q, _) = divAndRem (p, [(coeff, 0)])
      in q end

  and adjoinTerm (t1 as (c1, _), p2) =
      if A.eq (c1, A.ZERO) then p2 else t1 :: p2

  and mulTermByAllTerms (_, nil) = nil
    | mulTermByAllTerms (t1 as (c1, o1), (c2, o2)::ts2) =
      adjoinTerm ((A.mul (c1, c2), o1 + o2),
                  mulTermByAllTerms (t1, ts2))

  and negTerm (coeff, order) = (A.neg coeff, order)

  and eqTerm ((c1, o1:int), (c2, o2:int)) =
      A.eq (c1, c2) andalso o1 = o2

  and termToString (coeff, order) =
      A.toString coeff ^ "*" ^ V ^ "^" ^ Int.toString order
end;

structure Algebra = struct
  (* Z: Integer (Euclidean domain) *)
  structure Z = IntArith
  (* Z2: Z/2Z (finite field) *)
  structure Z2 = IntModArithFn (val N = 2)
  (* R: Real (field) *)
  structure R = RealArith
  (* Q: q(Z) (quotient field) *)
  structure Q = QuotientArithFn (structure A = Z)
  (* C: Complex (field) *)
  structure C = GaussArithFn (structure A = R)
  (* Zi: Gaussian integer (Euclidean domain) *)
  structure Zi = GaussArithFn (structure A = Z)
  (* Qi: Gaussian rational *)
  structure Qi = GaussArithFn (structure A = Q)
  (* Zx: Z[X] *)
  structure Zx = PolyArithFn (val V = "X"; structure A = Z)
  (* Z2x: Z2[X] (Euclidean domain) *)
  structure Z2x = PolyArithFn (val V = "X"; structure A = Z2)
  (* Qx: Q[X] (Euclidean domain) *)
  structure Qx = PolyArithFn (val V = "X"; structure A = Q)
  (* Rx: R[X] (Euclidean domain) *)
  structure Rx = PolyArithFn (val V = "X"; structure A = R)
  (* Q_Qx: q(Q[X]) (quotient field) *)
  structure Q_Qx = QuotientArithFn (structure A = Qx)
  (* Q_Rx: q(R[X]) (quotient field) *)
  structure Q_Rx = QuotientArithFn (structure A = Rx)

  fun makeInt (i:int) : Z.t = Z.normalize i

  fun makeMod2 (i:int) : Z2.t = Z2.normalize i

  fun makeReal (r:real) : R.t = R.normalize r

  fun makeRat (a:Z.t, b:Z.t) : Q.t = Q.normalize (a, b)

  fun makeComplex (a:R.t, b:R.t) : C.t = C.normalize (a, b)

  fun makeGaussianInt (a:Z.t, b:Z.t) : Zi.t = Zi.normalize (a, b)

  fun makeGaussianRat (a:Q.t, b:Q.t) : Qi.t = Qi.normalize (a, b)

  fun magnitude ((a, b):C.t) = Math.sqrt ((a * a) + (b * b))

  fun angle ((a, b):C.t) = Math.atan2 (b, a)

  fun intToRat (i:Z.t) : Q.t = (i, Z.ONE)

  fun intToReal (i:Z.t) : R.t = real i

  fun ratToReal ((a, b):Q.t) : R.t = (real a) / (real b)

  fun realToComplex (r:R.t) : C.t = (r, R.ZERO)

  fun intToGaussianInt (i:Z.t) : Zi.t = (i, Z.ZERO)

  fun ratToGaussianRat (q:Q.t) : Qi.t = (q, Q.ZERO)
end;

structure A = Algebra;

(*
 * Test for polynomials (Qx)
 *)

let
  (* Qx: X^5 - 1 *)
  val p1 = [((1,1),5),((~1,1),0)]
  (* Qx: X^2 - 1 *)
  val p2 = [((1,1),2),((~1,1),0)]
  (* Qx: p2 / p1 => expected: q = X^3 + X, r = X - 1 *)
  val (q, r) = A.Qx.divAndRem (p1, p2)
in
  print ("(q, r)=(" ^ A.Qx.toString q ^ ", " ^ A.Qx.toString r ^ ")\n")
end;

let
  (* Qx: X^4 - X^3 - 2*X^2 + 2*X *)
  val p1 = [((1,1),4),((~1,1),3),((~2,1),2),((2,1),1)]
  (* Qx: X^3 - X *)
  val p2 = [((1,1),3),((~1,1),1)]
  (* Qx: gcd (p1, p2) => expected: X^2 + X *)
  val gcd = (A.Qx.normalizeGcd o A.Qx.gcd) (p1, p2)
in
  print ("gcd=" ^ A.Qx.toString gcd ^ "\n")
end;

let
  (* Qx: X^2 + 2*X + 1 *)
  val p1 = [((1,1),2),((2,1),1),((1,1),0)]
  (* Qx: X^2 - 1 *)
  val p2 = [((1,1),2),((~1,1),0)]
  (* Qx: gcd (p1, p2) => expected: X + 1 *)
  val gcd = (A.Qx.normalizeGcd o A.Qx.gcd) (p1, p2)
in
  print ("gcd=" ^ A.Qx.toString gcd ^ "\n")
end;

let
  (* Qx: X^2 - 2*X + 1 *)
  val p1 = [((1,1),2),((~2,1),1),((1,1),0)]
  (* Qx: 11*X^2 + 7 *)
  val p2 = [((11,1),2),((7,1),0)]
  (* Qx: 13*X + 5 *)
  val p3 = [((13,1),1),((5,1),0)]
  (* Qx: p1 * p2 *)
  val q1 = A.Qx.mul (p1,p2)
  (* Qx: p1 * p3 *)
  val q2 = A.Qx.mul (p1,p3)
  (* Qx: gcd (q1, q2) => expected: p1 *)
  val gcd = (A.Qx.normalizeGcd o A.Qx.gcd) (q1, q2)
  (*: Qx: q1/q2 => expected: p2/p3 *)
  val rat = A.Q_Qx.normalize (q1, q2)
in
  print ("gcd=" ^ A.Qx.toString gcd ^ "\n");
  print ("q1/q2=" ^ A.Q_Qx.toString rat ^ "\n")
end;

(*
 * Test for polynomials (Z2x)
 *)

let
  (* Z2x: X^2 + 1 *)
  val p1 = [(1,2),(1,0)]
  (* Z2x: X + 1 *)
  val p2 = [(1,1),(1,0)]
  (* Z2x: gcd (p1, p2) => expected: X + 1 *)
  val gcd = (A.Z2x.normalizeGcd o A.Z2x.gcd) (p1, p2)
in
  print ("gcd=" ^ A.Z2x.toString gcd ^ "\n")
end;

(*
 * Test for complex numbers (Gaussian integers)
 *)

let
  (* Zi: 2 + i0 = (1 + i1)(1 + i~1) *)
  val z1 = (2, 0)
  (* Zi: 1 + i1 *)
  val z2 = (1, 1)
  (* Zi: gcd (p1, p2) => expected: 1 + i1 *)
  val gcd = (A.Zi.normalizeGcd o A.Zi.gcd) (z1, z2)
in
  print ("gcd=" ^ A.Zi.toString gcd ^ "\n")
end;

let
  (* Zi: -1 + i12 *)
  val z1 = (~1, 12)
  (* Zi: 4 + i7 *)
  val z2 = (4, 7)
  (* Zi: gcd (p1, p2) => expected: 2 + i1 *)
  val gcd = (A.Zi.normalizeGcd o A.Zi.gcd) (z1, z2)
in
  print ("gcd=" ^ A.Zi.toString gcd ^ "\n")
end;
