(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 3  Modularity, Objects, and State *)
(* 3.1  Assignment and Local State *)
(* 3.1.1  Local State Variables *)

val balanceRef = ref 100;

fun withdraw amount =
    if !balanceRef >= amount then
      (balanceRef := !balanceRef - amount; SOME (!balanceRef))
    else
      (print "<<Insufficient funds>>\n"; NONE);

fun newWithdraw () =
    let
      val balanceRef = ref 100
    in
      fn amount =>
         if !balanceRef >= amount then
           (balanceRef := !balanceRef - amount; SOME (!balanceRef))
         else
           (print "<<Insufficient funds>>\n"; NONE)
    end;

fun makeWithdraw balance =
    let
      val balanceRef = ref balance
    in
      fn amount =>
         if !balanceRef >= amount then
           (balanceRef := !balanceRef - amount; SOME (!balanceRef))
         else
           (print "<<Insufficient funds>>\n"; NONE)
    end;

val w1 = makeWithdraw 100;
val w2 = makeWithdraw 100;
w1 50;
w2 70;
w2 40;
w1 40;

structure Account = struct
  datatype msg = Withdraw | Deposit

  fun make balance =
      let
        val balanceRef = ref balance

        fun withdraw amount =
            if !balanceRef >= amount then
              (balanceRef := !balanceRef - amount; SOME (!balanceRef))
            else
              (print "<<Insufficient funds>>\n"; NONE)
        fun deposit amount =
            (balanceRef := !balanceRef + amount; SOME (!balanceRef))

        fun dispatch Withdraw = withdraw
          | dispatch Deposit = deposit
      in
        dispatch
      end
end;

val acc = Account.make 100;
acc Account.Withdraw 50;
acc Account.Withdraw 60;
acc Account.Deposit 40;
acc Account.Withdraw 60;

(* 3.1.2  The Benefits of Introducing Assignment *)

fun randUpdate x =
    let
      (*
       * val a, b, and c are borrowed from
       * http://www.codepoetics.com/wiki/index.php?title=Topics:SICP_in_other_languages:Alice_ML:Chapter_3 *)
      val a = 27
      val b = 26
      val m = 127
    in
      Int.rem (a * x + b, m)
    end;

val randomInit = 1;

val rand =
    let
      val x = ref randomInit;
    in
      (fn () => (x := (randUpdate (!x)); !x))
    end;

fun estimatePi trials =
    Math.sqrt (6.0 / (monteCarlo (trials, cesaroTest)))

and cesaroTest () = I.gcd (rand (), rand ()) = 1

and monteCarlo (trials, experiment) =
    let
      fun iter (trialsRemaining, trialsPassed) =
          if trialsRemaining = 0 then
            (real trialsPassed) / (real trials)
          else if experiment () then
            iter (trialsRemaining - 1, trialsPassed + 1)
          else
            iter (trialsRemaining - 1, trialsPassed)
    in
      iter (trials, 0)
    end;

estimatePi 10000;

fun estimatePi' trials =
    Math.sqrt (6.0 / (randomGcdTest trials randomInit))

and randomGcdTest trials initialX =
    let
      fun iter (trialsRemaining, trialsPassed, x) =
          let
            val x1 = randUpdate x
            val x2 = randUpdate x1
          in
            if trialsRemaining = 0 then
              (real trialsPassed) / (real trials)
            else if I.gcd (x1, x2) = 1 then
              iter (trialsRemaining - 1, trialsPassed + 1, x2)
            else
              iter (trialsRemaining - 1, trialsPassed, x2)
          end
    in
      iter (trials, 0, initialX)
    end;

estimatePi' 10000;

fun randomInRange (low, high) =
    let
      val range = high - low
    in
      low + U.random range
    end;

fun estimateIntegral p (x1, x2, y1, y2) trials =
    let
      val w = x2 - x1
      val h = y2 - y1
      fun experiment () =
          let
            val point = (randomInRange (x1, x2),
                         randomInRange (y1, y2))
          in
            p point
          end
    in
      real (w * h) * (monteCarlo (trials, experiment))
    end

and regionTest (x, y) =
    I.square (x - 5) + I.square (y - 7) <= 9;

estimateIntegral regionTest (2,8,4,10) 10000;

structure MyRand = struct
  datatype msg = Generate | Reset
  datatype ret = Rand of int | Setter of (int -> unit)

  fun make randomInit =
      let
        val x = ref randomInit
        fun dispatch Generate =
            (x := (randUpdate (!x)); Rand (!x))
          | dispatch Reset =
            Setter (fn newValue => x := newValue)
      in
        dispatch
      end

  fun generate rand =
      case rand Generate
       of Rand value => value
        | _ => raise Match

  fun reset rand =
      case rand Reset
       of Setter f => f
        | _ => raise Match
end;

(* 3.1.3  The Costs of Introducing Assignment *)

fun makeSimplifiedWithdraw balance =
    let
      val balanceRef = ref balance
    in
      (fn amount =>
          (balanceRef := (!balanceRef) - amount; !balanceRef))
    end;

val W = makeSimplifiedWithdraw 25;
W 20;
W 10;

fun makeDecrementer balance =
 fn amount => balance - amount;

val D = makeDecrementer 25;
D 20;
D 10;

val peterAcc = Account.make 100;
val pualAcc = Account.make 100;

val peterAcc' = Account.make 100;
val pualAcc' = peterAcc';

fun factorial1 n =
    let
      fun iter (product, counter) =
          if counter > n then product
          else iter (counter * product, counter + 1)
    in
      iter (1, 1)
    end;

fun factorial2 n =
    let
      val product = ref 1
      val counter = ref 1
      fun iter () =
          if !counter > n then !product
          else ((product := (!counter * !product));
                (counter := (!counter + 1));
                iter ())
    in
      iter ()
    end;


