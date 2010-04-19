(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml, chap1_2.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 2  Building Abstractions with Data *)
(* 2.2  Hierarchical Data and the Closure Property *)
(* 2.2.1  Representing Sequences *)

val oneThroughFour = 1 :: 2 :: 3 :: 4 :: nil;

hd oneThroughFour;

tl oneThroughFour;

hd (tl oneThroughFour);

10 :: oneThroughFour;

5 :: oneThroughFour;

fun listRef (items, n) =
    if n = 0 then hd items
    else listRef (tl items, n - 1);

val squares = [1, 4, 9, 16, 25];

listRef (squares, 3);

fun length' nil = 0
  | length' (_::t) = 1 + length' t;

val odds = [1, 3, 5, 7];

length' odds;

fun length' items =
    let
      fun lengthIter (nil,  count) = count
        | lengthIter (_::t, count) = lengthIter (t, count + 1)
    in
      lengthIter (items, 0)
    end;

squares @ odds;

odds @ squares;

fun append' (nil,  list2) = list2
  | append' (h::t, list2) = h :: append' (t, list2);

append' (squares, odds);

append' (odds, squares);

fun scaleList (nil,  factor) = nil
  | scaleList (h::t, factor) = factor * h :: scaleList (t, factor);

scaleList ([1, 2, 3, 4, 5], 10);

fun map' proc nil    = nil
  | map' proc (h::t) = (proc h) :: map proc t;

map' abs [~10.0, 2.5, ~11.6, 17.0];

map' (fn x => x * x) [1, 2, 3, 4];

fun scaleList (items, factor) = map' (fn x => x * factor) items;

scaleList ([1, 2, 3, 4, 5], 10);

fun forEach proc nil    = ()
  | forEach proc (h::t) = (proc h; forEach proc t);

forEach (fn x => print (Int.toString x ^ "\n")) [1,2,3,4];

(* 2.2.2  Hierarchical Structures *)

structure Tree = struct
  datatype tree = Nil
                | Leaf of int
                | Node of tree * tree

  fun len Nil           = 0
    | len (Node (_, y)) = 1 + len y
    | len _             = raise Match

  fun list nil    = Nil
    | list (h::t) = Node (h, list t)

  fun toString t =
      let
        fun s1 Nil           = "()"
          | s1 (Leaf i)      = Int.toString i
          | s1 (Node (x, y)) = "(" ^ s1 x ^ s2 y ^ ")"
        and s2 Nil           = ""
          | s2 (Leaf i)      = " . " ^ Int.toString i
          | s2 (Node (x, y)) = " " ^ s1 x ^ s2 y
      in
        s1 t
      end
end;

structure T = Tree;
(* () *)
T.toString (T.Nil);
(* 1 *)
T.toString (T.Leaf 1);
(* (1 . 2) *)
T.toString (T.Node (T.Leaf 1, T.Leaf 2));
(* (1 2 3) *)
T.toString (T.list [T.Leaf 1, T.Leaf 2, T.Leaf 3]);
(* ((1 . 2) 3) *)
T.toString (T.list [T.Node (T.Leaf 1, T.Leaf 2), T.Leaf 3]);

fun countLeaves T.Nil = 0
  | countLeaves (T.Leaf _) = 1
  | countLeaves (T.Node (l, r)) = (countLeaves l) + (countLeaves r);

(* ((1 2) 3 4) *)
val t = T.Node (T.list [T.Leaf 1, T.Leaf 2], T.list [T.Leaf 3, T.Leaf 4]);
T.toString t;
T.len t;
countLeaves t;

val t = T.list [t, t];
T.toString t;
T.len t;
countLeaves t;

fun scaleTree (T.Nil, factor) = T.Nil
  | scaleTree (T.Leaf i, factor) = T.Leaf (i * factor)
  | scaleTree (T.Node (l, r), factor) = T.Node (scaleTree (l, factor),
                                                scaleTree (r, factor));

(* (1 (2 (3 4) 5) (6 7)) *)
val t = T.list [T.Leaf 1,
                T.list [T.Leaf 2,
                        T.list [T.Leaf 3, T.Leaf 4],
                        T.Leaf 5],
                T.list [T.Leaf 6,
                        T.Leaf 7]];
T.toString t;
T.toString (scaleTree (t, 10));

(* 2.2.3  Sequences as Conventional Interfaces *)

fun sumOddSquares T.Nil = 0
  | sumOddSquares (T.Leaf i) = if I.odd i then I.square i else 0
  | sumOddSquares (T.Node (l, r)) = (sumOddSquares l) + (sumOddSquares r);

(* fib: => included in chap1_2.sml *)

fun evenFibs n =
    let
      fun next k =
          if k > n then nil
          else
            let
              val f = fib k
            in
              if I.even f then f :: next (k + 1)
              else next (k + 1)
            end
    in
      next 0
    end;

map I.square [1, 2, 3, 4, 5];

(* filter is same as List.filter *)
fun filter predicate nil = nil
  | filter predicate (h::t) =
    if predicate h then h :: filter predicate t
    else filter predicate t;

filter I.odd [1, 2, 3, 4, 5];

(* accumulate is same as List.foldr *)
fun accumulate operator initial nil = initial
  | accumulate operator initial (h::t) =
    operator (h, accumulate operator initial t);

accumulate (op + ) 0 [1, 2, 3, 4, 5];

accumulate (op * ) 1 [1, 2, 3, 4, 5];

accumulate (op ::) nil [1, 2, 3, 4, 5];

fun enumerateInterval (low, high) =
    if low > high then nil
    else low :: enumerateInterval (low + 1, high);

enumerateInterval (2, 7);

fun enumerateTree T.Nil = nil
  | enumerateTree (T.Leaf i) = [i]
  | enumerateTree (T.Node (l, r)) = (enumerateTree l) @ (enumerateTree r);

(* (1 (2 (3 4) 5)) *)
val t = T.list [T.Leaf 1,
                T.list[T.Leaf 2,
                       T.list [T.Leaf 3, T.Leaf 4],
                       T.Leaf 5]];
T.toString t;
enumerateTree t;

fun sumOddSquare tree =
    accumulate (op +)
               0
               (map I.square (filter I.odd (enumerateTree tree)));

T.toString t;
sumOddSquare t;

fun evenFibs n =
    accumulate (op ::)
               nil
               (filter I.even (map fib (enumerateInterval (0, n))));

evenFibs 10;

fun listFibSquares n =
    accumulate (op ::)
               nil
               (map I.square (map fib (enumerateInterval (0, n))));

listFibSquares 10;

fun productOfSquaresOfOddElements sequence =
    accumulate (op * )
               1
               (map I.square (filter I.odd sequence));

productOfSquaresOfOddElements [1, 2, 3, 4, 5];

(*
 * fun salaryOfHighestPaidProgrammer records =
 *     accumulate Int.max
 *                0
 *                (map sarary (filter programmer records));
 *)

(*
 * fun genOrderedPairs n =
 *     accumulate (op @)
 *                nil
 *                (map (fn i => (map (fn j => [i, j])
 *                                   (enumerateInterval (1, i - 1))))
 *                     (enumerateInterval (1, n)));
 * 
 * genOrderedPairs 6;
*)

fun flatmap proc seq =
    accumulate (op @) nil (map proc seq);

(* prime: => included in chap1_2.sml *)

fun primeSum (x, y) (* [x, y] *) = prime (x + y);

fun makePairSum (x, y) = (x, y, x+y); (* [x, y, x+y]; *)

fun primeSumPairs n =
    map makePairSum
        (filter primeSum
                (flatmap (fn i => map (fn j => (i,j))
                                      (enumerateInterval (1, i - 1)))
                         (enumerateInterval (1, n))));

primeSumPairs 6;

fun permutations (s:''a list) : ''a list list =
    let
      fun remove (item:''a, seq:''a list) =
          filter (fn x => not (x = item)) seq
    in
      if null s then [nil]
      else flatmap (fn x => (map (fn p => x :: p)
                                 (permutations (remove (x, s)))))
                   s
    end;

permutations ["a","b"];
permutations [1,2,3];

(* 2.2.4  Example: A Picture Language => chap2_2_4_smltk.sml *)
