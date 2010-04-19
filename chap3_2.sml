(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml, chap1_1.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 3  Modularity, Objects, and State *)
(* 3.2  The Environment Model of Evaluation *)
(* 3.2.1  The Rules for Evaluation *)

(*
 * fun square x = x * x;
 * 
 * square 5;
 * 
 * val square = fn x => x * x;
 * 
 * square 5;
 *)

(* 3.2.2  Applying Simple Procedures *)

(* sumOfSquares: => included in chap1_1.sml *)

fun f a = sumOfSquares (a + 1, a * 2);

f 5;

(* 3.2.3  Frames as the Repository of Local State *)

fun makeWithdraw' balance =
    let
      val balanceRef = ref balance
    in
      fn amount =>
         if !balanceRef >= amount then
           (balanceRef := !balanceRef - amount; SOME (!balanceRef))
         else
           (print "<<Insufficient funds>>\n"; NONE)
    end;

val W1 = makeWithdraw' 100;
W1 50;

val W2 = makeWithdraw' 100;

(* 3.2.4  Internal Definitions *)

(* sqrt: => included in chap1_1.sml *)

sqrt 2.0;
