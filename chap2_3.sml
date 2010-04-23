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
(* 2.3  Symbolic Data *)
(* 2.3.1  Quotation *)

(*
 * fun memq (_, nil) = nil
 *   | memq (item, l as (h::t)) = if item = h then l
 *                                else memq (item, t);
 * memq ("apple", ["pear", "banana", "prune"]);
 * memq ("apple", ["x", "sauce", "y", "apple", "pear"]);
 *)

(* 2.3.2  Example: Symbolic Differentiation *)

signature EXP = sig
  type t
  val zero : t
  val one : t
  val isNumber : t -> bool
  val makeNumber : int -> t
  val isVariable : t -> bool
  val isSameVariable : t * t -> bool
  val makeVariable : string -> t
  val isSum : t -> bool
  val addend : t -> t
  val augend : t -> t
  val makeSum : t * t -> t
  val isProduct : t -> bool
  val multiplier : t -> t
  val multiplicand : t -> t
  val makeProduct : t * t -> t
  val toString : t -> string
end;

functor ExpOpsFn (E : EXP) = struct
  open E
  fun deriv (exp, var) =
      if isNumber exp then zero
      else if isVariable exp then
        if isSameVariable (exp, var) then one
        else zero
      else if isSum exp then
        makeSum (deriv (addend exp, var),
                 deriv (augend exp, var))
      else if isProduct exp then
        makeSum (makeProduct (multiplier exp,
                              deriv (multiplicand exp, var)),
                 makeProduct (deriv (multiplier exp, var),
                              multiplicand exp))
      else
        raise Fail "Unknown exp"
end;

structure Exp' :> EXP = struct
  datatype t = Num of int
             | Var of string
             | Sum of t * t
             | Product of t * t

  val zero = Num 0
  val one  = Num 1

  fun isNumber (Num _) = true
    | isNumber _ = false

  val makeNumber = Num

  fun isVariable (Var _) = true
    | isVariable _ = false

  fun isSameVariable (Var x, Var y) = x = y
    | isSameVariable (_, _) = false

  val makeVariable = Var

  fun isSum (Sum (_, _)) = true
    | isSum _ = false

  fun addend (Sum (x, _)) = x
    | addend _ = raise Match

  fun augend (Sum (_, y)) = y
    | augend _ = raise Match

  val makeSum = Sum

  fun isProduct (Product (_, _)) = true
    | isProduct _ = false

  fun multiplier (Product (x, _)) = x
    | multiplier _ = raise Match

  fun multiplicand (Product (_, y)) = y
    | multiplicand _ = raise Match

  val makeProduct = Product

  fun toString (Num r) = Int.toString r
    | toString (Var x) = x
    | toString (Sum (a1, a2)) =
      "[+ " ^ toString a1 ^ " " ^ toString a2 ^ "]"
    | toString (Product (m1, m2)) =
      "[* " ^ toString m1 ^ " " ^ toString m2 ^ "]"
end;

structure EO = ExpOpsFn (Exp');

val x = EO.makeVariable "x";
val y = EO.makeVariable "y";
val three = EO.makeNumber 3;

(* (deriv '[+ x 3] 'x) *)
EO.toString (EO.deriv (EO.makeSum (x, three), x));
(* (deriv '[* x y] 'x) *)
EO.toString (EO.deriv (EO.makeProduct (x, y), x));
(* (deriv '[* [* x y] [+ x 3]] 'x) *)
EO.toString (EO.deriv (EO.makeProduct (EO.makeProduct (x, y),
                                       EO.makeSum (x, three)), x));

structure Exp'' :> EXP = struct
  datatype t = Num of int
             | Var of string
             | Sum of t * t
             | Product of t * t

  val zero = Num 0
  val one  = Num 1

  fun isNumber (Num _) = true
    | isNumber _ = false

  val makeNumber = Num

  fun isVariable (Var _) = true
    | isVariable _ = false

  fun isSameVariable (Var x, Var y) = x = y
    | isSameVariable (_, _) = false

  val makeVariable = Var

  fun isSum (Sum (_, _)) = true
    | isSum _ = false

  fun addend (Sum (x, _)) = x
    | addend _ = raise Match

  fun augend (Sum (_, y)) = y
    | augend _ = raise Match

  fun makeSum (Num 0, a2) = a2
    | makeSum (a1, Num 0) = a1
    | makeSum (Num n1, Num n2) = Num (n1 + n2)
    | makeSum (a1, a2) = Sum (a1, a2)

  fun isProduct (Product (_, _)) = true
    | isProduct _ = false

  fun multiplier (Product (x, _)) = x
    | multiplier _ = raise Match

  fun multiplicand (Product (_, y)) = y
    | multiplicand _ = raise Match

  fun makeProduct (zero as Num 0, m2) = zero
    | makeProduct (m1, zero as Num 0) = zero
    | makeProduct (Num 1, m2) = m2
    | makeProduct (m1, Num 1) = m1
    | makeProduct (Num n1, Num n2) = Num (n1 * n2)
    | makeProduct (m1, m2) = Product (m1, m2)

  fun toString (Num r) = Int.toString r
    | toString (Var x) = x
    | toString (Sum (a1, a2)) =
      "[+ " ^ toString a1 ^ " " ^ toString a2 ^ "]"
    | toString (Product (m1, m2)) =
      "[* " ^ toString m1 ^ " " ^ toString m2 ^ "]"
end;

structure EO = ExpOpsFn (Exp'');

val x = EO.makeVariable "x";
val y = EO.makeVariable "y";
val three = EO.makeNumber 3;

(* (deriv '[+ x 3] 'x) *)
EO.toString (EO.deriv (EO.makeSum (x, three), x));
(* (deriv '[* x y] 'x) *)
EO.toString (EO.deriv (EO.makeProduct (x, y), x));
(* (deriv '[* [* x y] [+ x 3]] 'x) *)
EO.toString (EO.deriv (EO.makeProduct (EO.makeProduct (x, y),
                                       EO.makeSum (x, three)), x));

(* 2.3.3  Example: Representing Sets *)

signature SET =
sig
  type v
  type set
  val elementOf : v * set -> bool
  val adjoin : v * set -> set
  val intersection : set * set -> set
  val union : set * set -> set
end;

structure IntSetAsUnorderedList : SET = struct
  type v = int
  type set = v list

  fun elementOf (x:v, nil) = false
    | elementOf (x:v, h::t) = x = h orelse elementOf (x, t)

  fun adjoin (x, set) =
      if elementOf (x, set) then set
      else x :: set

  fun intersection (nil, set2) = nil
    | intersection (set1, nil) = nil
    | intersection (h::t, set2) =
      if elementOf (h, set2) then h :: intersection (t, set2)
      else intersection (t, set2);

  fun union (nil, set2) = set2
    | union (set1, nil) = set1
    | union (h::t, set2) =
      if elementOf (h, set2) then union (t, set2)
      else h :: union (t, set2);
end;

structure S = IntSetAsUnorderedList;

S.elementOf (1, [1,2,3,4,5]);
S.elementOf (6, [1,2,3,4,5]);
S.adjoin (1, [1,2,3,4,5]);
S.adjoin (6, [1,2,3,4,5]);
S.intersection ([1,2,3],[2,3,4,5,6,7]);
S.union ([1,2,3],[2,3,4,5,6,7]);

structure IntSetAsOrderedList : SET = struct
  type v = int
  type set = v list

  fun elementOf (x, nil) = false
    | elementOf (x, h::t) =
      if x < h then false
      else if x = h then true
      else elementOf (x, t)

  fun adjoin (x, nil) = [x]
    | adjoin (x, set as (h::t)) =
      if x < h then x :: set
      else if x = h then set
      else h :: adjoin (x, t)

  fun intersection (nil, set2) = nil
    | intersection (set1, nil) = nil
    | intersection (set1 as (h1::t1), set2 as (h2::t2)) =
      if h1 < h2 then intersection (t1, set2)
      else if h2 < h1 then intersection (set1, t2)
      else h1 :: intersection (t1, t2)

  fun union (nil, set2) = set2
    | union (set1, nil) = set1
    | union (set1 as (h1::t1), set2 as (h2::t2)) =
      if h1 < h2 then h1 :: union (t1, set2)
      else if h2 < h1 then h2 :: union (set1, t2)
      else h1 :: union (t1, t2)
end;

structure S = IntSetAsOrderedList;

S.elementOf (1, [1,2,3,4,5]);
S.elementOf (6, [1,2,3,4,5]);
S.adjoin (1, [1,2,3,4,5]);
S.adjoin (6, [1,2,3,4,5]);
S.intersection ([1,2,3],[2,3,4,5,6,7]);
S.union ([1,2,3],[2,3,4,5,6,7]);

structure BinTree =
struct
  datatype 'a t = Nil
                (* Node: entry * left * right *)
                | Node of 'a * 'a t * 'a t

  fun toList1 Nil = nil
    | toList1 (Node (e, lb, rb)) =
      (toList1 lb) @ (e :: toList1 rb)

  fun toList2 tree =
      let
        fun copyToList (Nil, resultList) = resultList
          | copyToList (Node (e, lb, rb), resultList) =
            copyToList (lb, e :: (copyToList (rb, resultList)))
      in
        copyToList (tree, nil)
      end

  fun fromList lst =
      let
        fun partialTree (elts, n) =
            if n = 0 then
              (Nil, elts)
            else
              let
                val leftSize = (n - 1) div 2
                val (leftTree, elts') = partialTree (elts, leftSize)
                val rightSize = n - (leftSize + 1)
                val thisEntry = hd elts'
                val (rightTree, elts'') = partialTree (tl elts', rightSize)
              in
                (Node (thisEntry, leftTree, rightTree), elts'')
              end

        val (tree, _) = partialTree (lst, length lst)
      in
        tree
      end
end;

(*
 * val bt = BinTree.fromList [1,3,5,7,9,11];
 * val l1 = BinTree.toList1 bt;
 * val l2 = BinTree.toList2 bt;
 *)

structure IntSetAsBinTree : SET = struct
  structure T = BinTree

  type v = int
  type set = v T.t

  fun elementOf (x, T.Nil) = false
    | elementOf (x, T.Node (e, lb, rb)) =
      if x < e then elementOf (x, lb)
      else if e < x then elementOf (x, rb)
      else true

  fun adjoin (x, T.Nil) = T.Node (x, T.Nil, T.Nil)
    | adjoin (x, set as T.Node (e, lb, rb)) =
      if x < e then T.Node (e, adjoin (x, lb), rb)
      else if e < x then T.Node (e, lb, adjoin (x, rb))
      else set

  fun intersection (set1, set2) =
      let
        fun f (nil, set2) = nil
          | f (set1, nil) = nil
          | f (set1 as (h1::t1), set2 as (h2::t2)) =
            if h1 < h2 then f (t1, set2)
            else if h2 < h1 then f (set1, t2)
            else h1 :: f (t1, t2)
        val l1 = T.toList2 set1
        and l2 = T.toList2 set2
      in
        T.fromList (f (l1, l2))
      end

  fun union (set1, set2) =
      let
        fun f (nil, set2) = set2
          | f (set1, nil) = set1
          | f (set1 as (h1::t1), set2 as (h2::t2)) =
            if h1 < h2 then h1 :: f (t1, set2)
            else if h2 < h1 then h2 :: f (set1, t2)
            else h1 :: f (t1, t2)
        val l1 = T.toList2 set1
        and l2 = T.toList2 set2
      in
        T.fromList (f (l1, l2))
      end
end;

structure S = IntSetAsBinTree;
structure T = BinTree;

S.elementOf (1, T.fromList [1,2,3,4,5]);
S.elementOf (6, T.fromList [1,2,3,4,5]);
T.toList2 (S.adjoin (1, T.fromList [1,2,3,4,5]));
T.toList2 (S.adjoin (6, T.fromList [1,2,3,4,5]));
T.toList2 (S.intersection (T.fromList [1,2,3],
                           T.fromList [2,3,4,5,6,7]));
T.toList2 (S.union (T.fromList [1,2,3],
                    T.fromList [2,3,4,5,6,7]));

fun lookupList key (givenKey:int, nil) = NONE
  | lookupList key (givenKey:int, h::t) =
    if givenKey = (key h) then SOME h
    else lookupList key (givenKey, t);

val db = [(1,"dog"),(2,"cat"),(3,"bird")];
lookupList #1 (1, db);
lookupList #1 (4, db);

fun lookupTree key (givenKey:int, T.Nil) = NONE
  | lookupTree key (givenKey:int, node as T.Node (e,lb,rb)) =
    let val k = key e
    in if givenKey < k then lookupTree key (givenKey, lb)
       else if k < givenKey then lookupTree key (givenKey, rb)
       else SOME e
    end;

val db = T.fromList [(1,"dog"),(2,"cat"),(3,"bird")];
lookupTree #1 (1, db);
lookupTree #1 (4, db);

(* 2.3.4  Example: Huffman Encoding Trees *)

structure HuffmanTree = struct
  datatype ''a t
    (* Leaf: symbol * weight *)
    = Leaf of ''a * int
    (* Node: symbol list * weight * left * right *)
    | Node of ''a list * int * ''a t * ''a t

  fun makeCodeTree (left, right) =
      Node ((symbols left) @ (symbols right),
            (weight left) + (weight right),
            left,
            right)

  and symbols (Leaf (sym,_)) = [sym]
    | symbols (Node (syms,_,_,_)) = syms

  and weight (Leaf (_,w)) = w
    | weight (Node (_,w,_,_)) = w

  fun decode (bits, tree) =
      let
        fun decode1 (nil, currentBranch) = nil
          | decode1 (h::t, currentBranch) =
            case chooseBranch (h, currentBranch)
             of (Leaf (s, _)) => s :: decode1 (t, tree)
              | (nextBranch as _) => decode1 (t, nextBranch)
      in
        decode1 (bits, tree)
      end

  and chooseBranch (0, Node (_,_,lb,_)) = lb
    | chooseBranch (1, Node (_,_,_,rb)) = rb
    | chooseBranch (_, Leaf (_,_)) = raise Match (* Node expected *)
    | chooseBranch (_, _) = raise Match (* Bad bit *)

  fun encode (nil, tree) = nil
    | encode (h::t, tree) =
      encodeSymbol (h, tree) @ encode (t, tree)

  and encodeSymbol (sym, tree) =
      let
        fun encodeSymbol1 (Leaf (_,_), bits) = bits
          | encodeSymbol1 (Node (_,_,lb,rb), bits) =
            if elementOf (sym, (symbols lb)) then
              encodeSymbol1 (lb, 0 :: bits)
            else if elementOf (sym, (symbols rb)) then
              encodeSymbol1 (rb, 1 :: bits)
            else
              raise Fail "Bad symbol"
        and elementOf (sym, nil) = false
          | elementOf (sym, h::t) =
            sym = h orelse elementOf (sym, t)
      in
        rev (encodeSymbol1 (tree, nil))
      end

  fun adjoinSet (x, nil) = [x]
    | adjoinSet (x, set as h::t) =
      if (weight x) < (weight h) then x :: set
      else h :: adjoinSet (x, t)

  fun makeLeafSet nil = []
    | makeLeafSet ((s, f)::t) =
      adjoinSet (Leaf (s, f), makeLeafSet t)

  fun generateTree pairs =
      successiveMerge (makeLeafSet pairs)

  and successiveMerge (h::nil) = h
    | successiveMerge (left::right::t) =
      successiveMerge (adjoinSet (makeCodeTree (left, right), t))
    | successiveMerge nil = raise Empty
end;

structure H = HuffmanTree;
(*
 * val t = H.makeCodeTree (H.Leaf (#"A",4),
 *                         H.makeCodeTree (H.Leaf (#"B",2),
 *                                         H.makeCodeTree (H.Leaf (#"D",1),
 *                                                         H.Leaf (#"C",1))));
 *)
val t = H.generateTree [(#"A",4),(#"B",2),(#"C",1),(#"D",1)];
val b = [0,1,1,0,0,1,0,1,0,1,1,1,0];
val m = H.decode (b, t);
val b' = H.encode (m, t);

val t = H.generateTree [("A",2),("NA",16),("BOOM",1),("SHA",3),
                        ("GET",2),("YIP",9),("JOB",2),("WAH",1)];
val m = ["GET","A","JOB",
         "SHA","NA","NA","NA","NA","NA","NA","NA","NA",
         "GET","A","JOB",
         "SHA","NA","NA","NA","NA","NA","NA","NA","NA",
         "WAH","YIP","YIP","YIP","YIP","YIP","YIP","YIP","YIP","YIP",
         "SHA","BOOM"];
val b = H.encode (m, t);
