(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml, chap5_2.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 5  Computing with Register Machines *)
(* 5.1  Designing Register Machines *)

local
  structure Conf : REGISTER_MACHINE_CONF =
  struct
    structure Register = Register
    (*structure Stack = Stack*)
    structure Stack = Stack'
    type value = int
    val undef = 0
    val isTrue = fn n => not (n = 0)
    val printValue = fn (s, v) => TextIO.output (s, Int.toString v)
  end

  structure M = RegisterMachineFn (Conf)

  (* Register, Stack, RegisterMachineFn => included in chap5_2.sml *)

  val opeAdd = M.fromFn2 ("+", op + )
  val opeSub = M.fromFn2 ("-", op - )
  val opeMul = M.fromFn2 ("*", op * )
  val opeRem = M.fromFn2 ("rem", op mod)
  val opeEq =
      let
        fun f (n1:int,n2:int) = if n1 = n2 then 1 else 0
      in
        M.fromFn2 ("=", f)
      end
  val opeLt =
      let
        fun f (n1:int,n2:int) = if n1 < n2 then 1 else 0
      in
        M.fromFn2 ("<", f)
      end
  val opeRead =
      let
        fun f () =
            (print "input: ";
             case TextIO.scanStream (Int.scan StringCvt.DEC)
                                    TextIO.stdIn of
               SOME i => i
             | NONE => raise Fail "Cannot read int")
      in
        M.fromFn0 ("read", f)
      end
  val opePrint =
      let
        fun f n =
            (print ("output: " ^ Int.toString n ^ "\n"); n)
      in
        M.fromFn1 ("print", f)
      end
in
(* 5.1.1  A Language for Describing Register Machines *)

(* The GCD machine *)
fun calcGcd (a, b) =
    let
      (* registers *)
      val regs = ["a","b","t"]
      (* operators *)
      val ops = [opeRem, opeEq]
      (* controller-text *)
      val ctrls =
          [M.Label "test-b",
           M.Test ("=", [M.R "b", M.C 0]),
           M.Branch (M.L "gcd-done"),
           M.AssignOp ("t", "rem", [M.R "a", M.R "b"]),
           M.Assign ("a", M.R "b"),
           M.Assign ("b", M.R "t"),
           M.Goto (M.L "test-b"),
           M.Label "gcd-done"]
      (* machine model *)
      val m = M.makeMachine (regs, ops, ctrls)
    in
      M.setRegisterContents m "a" (M.Value a);
      M.setRegisterContents m "b" (M.Value b);
      M.start m;
      M.getRegisterContents m "a"
    end

(* The GCD machine that reads inputs and prints results *)
fun calcGcd' () =
    let
      (* registers *)
      val regs = ["a","b","t"]
      (* operators *)
      val ops = [opeRem, opeEq, opeRead, opePrint]
      (* controller-text *)
      val ctrls =
          [M.Label "gcd-loop",
           M.AssignOp ("a", "read", []),
           M.AssignOp ("b", "read", []),
           M.Label "test-b",
           M.Test ("=", [M.R "b", M.C 0]),
           M.Branch (M.L "gcd-done"),
           M.AssignOp ("t", "rem", [M.R "a", M.R "b"]),
           M.Assign ("a", M.R "b"),
           M.Assign ("b", M.R "t"),
           M.Goto (M.L "test-b"),
           M.Label "gcd-done",
           M.Perform ("print", [M.R "a"]),
           M.Goto (M.L "gcd-loop")]
      (* machine model *)
      val m = M.makeMachine (regs, ops, ctrls)
    in
      M.start m
    end

(* 5.1.2  Abstraction in Machine Design *)

(* The elabolated GCD machine *)
fun calcGcd'' (a, b) =
    let
      (* registers *)
      val regs = ["a","b","t"]
      (* operators *)
      val ops = [opeSub, opeEq, opeLt]
      (* controller-text *)
      val ctrls =
          [M.Label "test-b",
           M.Test ("=", [M.R "b", M.C 0]),
           M.Branch (M.L "gcd-done"),
           M.Assign ("t", M.R "a"),
           M.Label "rem-loop",
           M.Test ("<", [M.R "t", M.R "b"]),
           M.Branch (M.L "rem-done"),
           M.AssignOp ("t", "-", [M.R "t", M.R "b"]),
           M.Goto (M.L "rem-loop"),
           M.Label "rem-done",
           M.Assign ("a", M.R "b"),
           M.Assign ("b", M.R "t"),
           M.Goto (M.L "test-b"),
           M.Label "gcd-done"]
      (* machine model *)
      val m = M.makeMachine (regs, ops, ctrls)
    in
      M.setRegisterContents m "a" (M.Value a);
      M.setRegisterContents m "b" (M.Value b);
      M.start m;
      M.getRegisterContents m "a"
    end

(* 5.1.3  Subroutines *)

(* The GCD machine with subroutine *)
fun calcGcd''' () =
    let
      (* registers *)
      val regs = ["a","b","t","continue"]
      (* ops *)
      val ops = [opeRem, opeEq, opeRead, opePrint]
      (* controller-text *)
      val ctrls =
          [M.Label "main",
           (* call-gcd-1 *)
           M.Assign ("continue", M.L "after-gcd-1"),
           M.Goto (M.L "gcd"),
           M.Label "after-gcd-1",
           (* call-gcd-2 *)
           M.Assign ("continue", M.L "after-gcd-2"),
           M.Goto (M.L "gcd"),
           M.Label "after-gcd-2",
           M.Goto (M.L "main-done"),
           (* gcd-subroutine *)
           M.Label "gcd",
           M.AssignOp ("a", "read", []),
           M.AssignOp ("b", "read", []),
           M.Label "test-b",
           M.Test ("=", [M.R "b", M.C 0]),
           M.Branch (M.L "gcd-done"),
           M.AssignOp ("t", "rem", [M.R "a", M.R "b"]),
           M.Assign ("a", M.R "b"),
           M.Assign ("b", M.R "t"),
           M.Goto (M.L "test-b"),
           M.Label "gcd-done",
           M.Perform ("print", [M.R "a"]),
           M.Goto (M.R "continue"),
           (* gcd-subroutine done *)
           M.Label "main-done"]
      (* machine model *)
      val m = M.makeMachine (regs, ops, ctrls)
    in
      M.start m
    end

(* 5.1.4  Using a Stack to Implement Recursion *)

fun calcFact n =
    let
      (* registers *)
      val regs = ["n","val","continue"]
      (* operators *)
      val ops = [opeSub, opeMul, opeEq]
      (* controller-text *)
      val ctrls =
          [(* set up final return address *)
           M.Assign ("continue", M.L "fact-done"),
           M.Label "fact-loop",
           M.Test ("=", [M.R "n", M.C 1]),
           M.Branch (M.L "base-case"),
           (* Set up for the recursive call by saving n and continue.
            * Set up continue so that the computation will continue
            * at after-fact when the subroutine returns. *)
           M.Save "continue",
           M.Save "n",
           M.AssignOp ("n", "-", [M.R "n", M.C 1]),
           M.Assign ("continue", M.L "after-fact"),
           M.Goto (M.L "fact-loop"),
           M.Label "after-fact",
           M.Restore "n",
           M.Restore "continue",
           (* val now contains n (n - 1)! *)
           M.AssignOp ("val", "*", [M.R "n", M.R "val"]),
           (* return to caller *)
           M.Goto (M.R "continue"),
           M.Label "base-case",
           (* base case: 1! = 1 *)
           M.Assign ("val", M.C 1),
           (* return to caller *)
           M.Goto (M.R "continue"),
           M.Label "fact-done",
           M.Perform ("print-stack-stats", [])]
      (* machine model *)
      val m = M.makeMachine (regs, ops, ctrls)
    in
      M.setRegisterContents m "n" (M.Value n);
      M.start m;
      M.getRegisterContents m "val"
    end

fun calcFib n =
    let
      (* registers *)
      val regs = ["n","val","continue"]
      (* operators *)
      val ops = [opeAdd, opeSub, opeEq, opeLt]
      (* controller-text *)
      val ctrls =
          [(* set up final return address *)
           M.Assign ("continue", M.L "fib-done"),
           M.Label "fib-loop",
           M.Test ("<", [M.R "n", M.C 2]),
           M.Branch (M.L "immediate-answer"),
           (* set up to compute Fib(n - 1) *)
           M.Save "continue",
           M.Assign ("continue", M.L "afterfib-n-1"),
           (* save old value of n *)
           M.Save "n",
           (* clobber n to n - 1 *)
           M.AssignOp ("n", "-", [M.R "n", M.C 1]),
           (* perform recursive call *)
           M.Goto (M.L "fib-loop"),
           (* upon return, val contains Fib(n - 1) *)
           M.Label "afterfib-n-1",
           M.Restore "n",
           M.Restore "continue",
           (* set up to compute Fib(n - 2) *)
           M.AssignOp ("n", "-", [M.R "n", M.C 2]),
           M.Save "continue",
           M.Assign ("continue", M.L "afterfib-n-2"),
           (* save Fib(n - 1) *)
           M.Save "val",
           M.Goto (M.L "fib-loop"),
           (* upon return, val contains Fib(n - 2) *)
           M.Label "afterfib-n-2",
           M.Assign ("n", M.R "val"),
           (* val now contains Fib(n - 1) *)
           M.Restore "val",
           M.Restore "continue",
           (* Fib(n - 1) +  Fib(n - 2) *)
           M.AssignOp ("val", "+", [M.R "val", M.R "n"]),
           (* return to caller, answer is in val *)
           M.Goto (M.R "continue"),
           M.Label "immediate-answer",
           (* base case: Fib(n) = n *)
           M.Assign ("val", M.R "n"),
           M.Goto (M.R "continue"),
           M.Label "fib-done",
           M.Perform ("print-stack-stats", [])]
      (* machine model *)
      val m = M.makeMachine (regs, ops, ctrls)
    in
      M.setRegisterContents m "n" (M.Value n);
      M.start m;
      M.getRegisterContents m "val"
    end
end;

(*
 * calcGcd (206,40);
 * calcGcd' ();
 * calcGcd'' (206,40);
 * calcGcd''' ();
 * calcFact 5;
 * calcFib 10;
 *)

