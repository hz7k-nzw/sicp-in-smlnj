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
  (* Ope, Register, Stack, RegisterMachineFn => included in chap5_2.sml *)

  val opeAdd = Ope.fromFn2 ("+", op + )
  val opeSub = Ope.fromFn2 ("-", op - )
  val opeMul = Ope.fromFn2 ("*", op * )
  val opeRem = Ope.fromFn2 ("rem", op mod)
  val opeEq =
      let
        fun f (n1:int,n2:int) = if n1 = n2 then 1 else 0
      in
        Ope.fromFn2 ("=", f)
      end
  val opeLt =
      let
        fun f (n1:int,n2:int) = if n1 < n2 then 1 else 0
      in
        Ope.fromFn2 ("<", f)
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
        Ope.fromFn0 ("read", f)
      end
  val opePrint =
      let
        fun f n =
            (print ("output: " ^ Int.toString n ^ "\n"); n)
      in
        Ope.fromFn1 ("print", f)
      end

  structure Conf =
  struct
    structure Register = Register
    (*structure Stack = Stack*)
    structure Stack = Stack'
    type value = int
    val undef = 0
    val isTrue = fn n => not (n = 0)
  end

  structure M = RegisterMachineFn (Conf)

  open M
in
(* 5.1.1  A Language for Describing Register Machines *)

(* The GCD machine *)
fun calcGcd (a, b) =
    let
      val m = make ((* registers *)
                    ["a","b","t"],
                    (* ops *)
                    [opeRem, opeEq],
                    (* controller-text *)
                    [Label "test-b",
                     Test ("=", [R "b", C 0]),
                     Branch (L "gcd-done"),
                     AssignOp ("t", "rem", [R "a", R "b"]),
                     Assign ("a", R "b"),
                     Assign ("b", R "t"),
                     Goto (L "test-b"),
                     Label "gcd-done"])
    in
      setRegisterContents m "a" a;
      setRegisterContents m "b" b;
      start m;
      getRegisterContents m "a"
    end

(* The GCD machine that reads inputs and prints results *)
fun calcGcd' () =
    let
      val m = make ((* registers *)
                    ["a","b","t"],
                    (* ops *)
                    [opeRem, opeEq, opeRead, opePrint],
                    (* controller-text *)
                    [Label "gcd-loop",
                     AssignOp ("a", "read", []),
                     AssignOp ("b", "read", []),
                     Label "test-b",
                     Test ("=", [R "b", C 0]),
                     Branch (L "gcd-done"),
                     AssignOp ("t", "rem", [R "a", R "b"]),
                     Assign ("a", R "b"),
                     Assign ("b", R "t"),
                     Goto (L "test-b"),
                     Label "gcd-done",
                     Perform ("print", [R "a"]),
                     Goto (L "gcd-loop")])
    in
      start m
    end

(* 5.1.2  Abstraction in Machine Design *)

(* The elabolated GCD machine *)
fun calcGcd'' (a, b) =
    let
      val m = make ((* registers *)
                    ["a","b","t"],
                    (* ops *)
                    [opeSub, opeEq, opeLt],
                    (* controller-text *)
                    [Label "test-b",
                     Test ("=", [R "b", C 0]),
                     Branch (L "gcd-done"),
                     Assign ("t", R "a"),
                     Label "rem-loop",
                     Test ("<", [R "t", R "b"]),
                     Branch (L "rem-done"),
                     AssignOp ("t", "-", [R "t", R "b"]),
                     Goto (L "rem-loop"),
                     Label "rem-done",
                     Assign ("a", R "b"),
                     Assign ("b", R "t"),
                     Goto (L "test-b"),
                     Label "gcd-done"])
    in
      setRegisterContents m "a" a;
      setRegisterContents m "b" b;
      start m;
      getRegisterContents m "a"
    end

(* 5.1.3  Subroutines *)

(* The GCD machine with subroutine *)
fun calcGcd''' () =
    let
      val m = make ((* registers *)
                    ["a","b","t","continue"],
                    (* ops *)
                    [opeRem, opeEq, opeRead, opePrint],
                    (* controller-text *)
                    [Label "main",
                     (* call-gcd-1 *)
                     Assign ("continue", L "after-gcd-1"),
                     Goto (L "gcd"),
                     Label "after-gcd-1",
                     (* call-gcd-2 *)
                     Assign ("continue", L "after-gcd-2"),
                     Goto (L "gcd"),
                     Label "after-gcd-2",
                     Goto (L "main-done"),
                     (* gcd-subroutine *)
                     Label "gcd",
                     AssignOp ("a", "read", []),
                     AssignOp ("b", "read", []),
                     Label "test-b",
                     Test ("=", [R "b", C 0]),
                     Branch (L "gcd-done"),
                     AssignOp ("t", "rem", [R "a", R "b"]),
                     Assign ("a", R "b"),
                     Assign ("b", R "t"),
                     Goto (L "test-b"),
                     Label "gcd-done",
                     Perform ("print", [R "a"]),
                     Goto (R "continue"),
                     (* gcd-subroutine done *)
                     Label "main-done"
                   ])
    in
      start m
    end

(* 5.1.4  Using a Stack to Implement Recursion *)

fun calcFact n =
    let
      val m = make ((* registers *)
                    ["n","val","continue"],
                    (* ops *)
                    [opeSub, opeMul, opeEq],
                    (* controller-text *)
                    [(* set up final return address *)
                     Assign ("continue", L "fact-done"),
                     Label "fact-loop",
                     Test ("=", [R "n", C 1]),
                     Branch (L "base-case"),
                     (* Set up for the recursive call by saving n and continue.
                      * Set up continue so that the computation will continue
                      * at after-fact when the subroutine returns. *)
                     Save "continue",
                     Save "n",
                     AssignOp ("n", "-", [R "n", C 1]),
                     Assign ("continue", L "after-fact"),
                     Goto (L "fact-loop"),
                     Label "after-fact",
                     Restore "n",
                     Restore "continue",
                     (* val now contains n (n - 1)! *)
                     AssignOp ("val", "*", [R "n", R "val"]),
                     (* return to caller *)
                     Goto (R "continue"),
                     Label "base-case",
                     (* base case: 1! = 1 *)
                     Assign ("val", C 1),
                     (* return to caller *)
                     Goto (R "continue"),
                     Label "fact-done",
                     Perform ("print-stack-stats", [])])
    in
      setRegisterContents m "n" n;
      start m;
      getRegisterContents m "val"
    end

fun calcFib n =
    let
      val m = make ((* registers *)
                    ["n","val","continue"],
                    (* ops *)
                    [opeAdd, opeSub, opeEq, opeLt],
                    (* controller-text *)
                    [(* set up final return address *)
                     Assign ("continue", L "fib-done"),
                     Label "fib-loop",
                     Test ("<", [R "n", C 2]),
                     Branch (L "immediate-answer"),
                     (* set up to compute Fib(n - 1) *)
                     Save "continue",
                     Assign ("continue", L "afterfib-n-1"),
                     (* save old value of n *)
                     Save "n",
                     (* clobber n to n - 1 *)
                     AssignOp ("n", "-", [R "n", C 1]),
                     (* perform recursive call *)
                     Goto (L "fib-loop"),
                     (* upon return, val contains Fib(n - 1) *)
                     Label "afterfib-n-1",
                     Restore "n",
                     Restore "continue",
                     (* set up to compute Fib(n - 2) *)
                     AssignOp ("n", "-", [R "n", C 2]),
                     Save "continue",
                     Assign ("continue", L "afterfib-n-2"),
                     (* save Fib(n - 1) *)
                     Save "val",
                     Goto (L "fib-loop"),
                     (* upon return, val contains Fib(n - 2) *)
                     Label "afterfib-n-2",
                     Assign ("n", R "val"),
                     (* val now contains Fib(n - 1) *)
                     Restore "val",
                     Restore "continue",
                     (* Fib(n - 1) +  Fib(n - 2) *)
                     AssignOp ("val", "+", [R "val", R "n"]),
                     (* return to caller, answer is in val *)
                     Goto (R "continue"),
                     Label "immediate-answer",
                     (* base case: Fib(n) = n *)
                     Assign ("val", R "n"),
                     Goto (R "continue"),
                     Label "fib-done",
                     Perform ("print-stack-stats", [])])
    in
      setRegisterContents m "n" n;
      start m;
      getRegisterContents m "val"
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

