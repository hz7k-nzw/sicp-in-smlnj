(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml, chap4_1.sml, chap5_2.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 5  Computing with Register Machines *)
(* 5.4  The Explicit-Control Evaluator *)
(* 5.4.1  The Core of the Explicit-Control Evaluator *)
(* 5.4.2  Sequence Evaluation and Tail Recursion *)
(* 5.4.3  Conditionals, Assignments, and Definitions *)
(* 5.4.4  Running the Evaluator *)

functor ExplicitControlInterpreterFn (Runtime : LISP_RUNTIME)
        : INTERPRETER =
struct
  open Runtime

  structure Conf =
  struct
    structure Register = Register
    structure Stack = Stack'
    type value = Obj.t
    val undef = Obj.undef
    val isTrue = Obj.isTrue
  end

  structure M = RegisterMachineFn (Conf)

  open M

  (* constants *)
  val quit = Obj.sym ":q"
  val inputPrompt = Obj.str ";;; EC-Eval input:"
  val outputPrompt = Obj.str ";;; EC-Eval value:"

  (* registers *)
  val regs =
      ["exp", "env", "val", "continue", "proc", "argl", "unev"]

  (* operators *)
  val ops =
      [Ope.fromFn1 ("self-evaluating?",
                    Obj.bool o Syntax.isSelfEvaluating),
       Ope.fromFn1 ("variable?",
                    Obj.bool o Syntax.isVariable),
       Ope.fromFn1 ("quoted?",
                    Obj.bool o Syntax.isQuoted),
       Ope.fromFn1 ("assignment?",
                    Obj.bool o Syntax.isAssignment),
       Ope.fromFn1 ("definition?",
                    Obj.bool o Syntax.isDefinition),
       Ope.fromFn1 ("if?",
                    Obj.bool o Syntax.isIf),
       Ope.fromFn1 ("lambda?",
                    Obj.bool o Syntax.isLambda),
       Ope.fromFn1 ("begin?",
                    Obj.bool o Syntax.isBegin),
       Ope.fromFn1 ("application?",
                    Obj.bool o Syntax.isApplication),
       Ope.fromFn2 ("lookup-variable-value",
                    (fn (exp, env) => Obj.lookupEnv env exp)),
       Ope.fromFn1 ("text-of-quotation",
                    Syntax.textOfQuotation),
       Ope.fromFn1 ("lambda-parameters",
                    Syntax.lambdaParameters),
       Ope.fromFn1 ("lambda-body",
                    Syntax.lambdaBody),
       Ope.fromFn3 ("make-procedure",
                    Obj.expr),
       Ope.fromFn1 ("operands",
                    Syntax.operands),
       Ope.fromFn1 ("operator",
                    Syntax.operator),
       Ope.fromFn0 ("empty-arglist",
                    (fn () => Obj.null)),
       Ope.fromFn1 ("no-operands?",
                    Obj.bool o Obj.isNull),
       Ope.fromFn1 ("first-operand",
                    Obj.car),
       Ope.fromFn1 ("last-operand?",
                    Obj.bool o Obj.isNull o Obj.cdr),
       Ope.fromFn2 ("adjoin-arg",
                    (fn (arg, args) =>
                        Obj.append (args, Obj.fromList [arg]))),
       Ope.fromFn1 ("rest-operands",
                    Obj.cdr),
       Ope.fromFn1 ("primitive-procedure?",
                    Obj.bool o Obj.isSubr),
       Ope.fromFn1 ("compound-procedure?",
                    Obj.bool o Obj.isExpr),
       Ope.fromFn2 ("apply-primitive-procedure",
                    (fn (proc, args) =>
                        Obj.applySubr proc (Obj.toList args))),
       Ope.fromFn1 ("procedure-parameters",
                    Obj.exprParams),
       Ope.fromFn1 ("procedure-environment",
                    Obj.exprEnv),
       Ope.fromFn3 ("extend-environment",
                    (fn (vars, vals, env) =>
                        Obj.extendEnv env (Obj.toList vars,
                                           Obj.toList vals))),
       Ope.fromFn1 ("procedure-body",
                    Obj.exprBody),
       Ope.fromFn1 ("begin-actions",
                    Syntax.beginActions),
       Ope.fromFn1 ("first-exp",
                    Obj.car),
       Ope.fromFn1 ("last-exp?",
                    Obj.bool o Obj.isNull o Obj.cdr),
       Ope.fromFn1 ("rest-exps",
                    Obj.cdr),
       Ope.fromFn1 ("no-more-exps?",
                    Obj.bool o Obj.isNull),
       Ope.fromFn1 ("if-predicate",
                    Syntax.ifPredicate),
       Ope.fromFn1 ("true?",
                    Obj.bool o Obj.isTrue),
       Ope.fromFn1 ("if-alternative",
                    Syntax.ifAlternative),
       Ope.fromFn1 ("if-consequent",
                    Syntax.ifConsequent),
       Ope.fromFn1 ("assignment-variable",
                    Syntax.assignmentVariable),
       Ope.fromFn1 ("assignment-value",
                    Syntax.assignmentValue),
       Ope.fromFn3 ("set-variable-value!",
                    (fn (var, valu, env) =>
                        (Obj.setEnv env (var, valu); var))),
       Ope.fromFn1 ("definition-variable",
                    Syntax.definitionVariable),
       Ope.fromFn1 ("definition-value",
                    Syntax.definitionValue),
       Ope.fromFn3 ("define-variable!",
                    (fn (var, valu, env) =>
                        (Obj.defineEnv env (var, valu); var))),
       Ope.fromFn2 ("eq?",
                    Obj.bool o Obj.eq),
       Ope.fromFn2 ("equal?",
                    Obj.bool o Obj.equal),
       Ope.fromFn1 ("derived?",       (* for Exercise 5.23 *)
                    Obj.bool o Syntax.isDerived),
       Ope.fromFn1 ("expand-derived", (* for Exercise 5.23 *)
                    Syntax.expandDerived)]

  (* control-text *)
  val ctrls =
      [Label "main",
       Assign ("continue", L "main-done"),
       Goto (L "read-eval-print-loop"),
       Label "eval-dispatch",
       Test ("self-evaluating?", [R "exp"]),
       Branch (L "ev-self-eval"),
       Test ("variable?", [R "exp"]),
       Branch (L "ev-variable"),
       Test ("quoted?", [R "exp"]),
       Branch (L "ev-quoted"),
       Test ("assignment?", [R "exp"]),
       Branch (L "ev-assignment"),
       Test ("definition?", [R "exp"]),
       Branch (L "ev-definition"),
       Test ("if?", [R "exp"]),
       Branch (L "ev-if"),
       Test ("lambda?", [R "exp"]),
       Branch (L "ev-lambda"),
       Test ("begin?", [R "exp"]),
       Branch (L "ev-begin"),
       Test ("derived?", [R "exp"]), (* for Exercise 5.23 *)
       Branch (L "ev-derived"),      (* for Exercise 5.23 *)
       Test ("application?", [R "exp"]),
       Branch (L "ev-application"),
       Goto (L "unknown-expression-type"),
       (* Evaluating simple expressions *)
       Label "ev-self-eval",
       Assign ("val", R "exp"),
       Goto (R "continue"),
       Label "ev-variable",
       AssignOp ("val", "lookup-variable-value",
                 [R "exp", R "env"]),
       Goto (R "continue"),
       Label "ev-quoted",
       AssignOp ("val", "text-of-quotation", [R "exp"]),
       Goto (R "continue"),
       Label "ev-lambda",
       AssignOp ("unev", "lambda-parameters", [R "exp"]),
       AssignOp ("exp", "lambda-body", [R "exp"]),
       AssignOp ("val", "make-procedure",
                 [R "unev", R "exp", R "env"]),
       Goto (R "continue"),
       (* Evaluating procedure applications *)
       Label "ev-application",
       Save "continue",
       Save "env",
       AssignOp ("unev", "operands", [R "exp"]),
       Save "unev",
       AssignOp ("exp", "operator", [R "exp"]),
       Assign ("continue", L "ev-appl-did-operator"),
       Goto (L "eval-dispatch"),
       Label "ev-appl-did-operator",
       Restore "unev",                  (* the operands *)
       Restore "env",
       AssignOp ("argl", "empty-arglist", []),
       Assign ("proc", R "val"),        (* the operator *)
       Test ("no-operands?", [R "unev"]),
       Branch (L "apply-dispatch"),
       Save "proc",
       Label "ev-appl-operand-loop",
       Save "argl",
       AssignOp ("exp", "first-operand", [R "unev"]),
       Test ("last-operand?", [R "unev"]),
       Branch (L "ev-appl-last-arg"),
       Save "env",
       Save "unev",
       Assign ("continue", L "ev-appl-accumulate-arg"),
       Goto (L "eval-dispatch"),
       Label "ev-appl-accumulate-arg",
       Restore "unev",
       Restore "env",
       Restore "argl",
       AssignOp ("argl", "adjoin-arg", [R "val", R "argl"]),
       AssignOp ("unev", "rest-operands", [R "unev"]),
       Goto (L "ev-appl-operand-loop"),
       (*
        * from http://aggregate.org/LAR/p163-steele.pdf;
        * ... Wand has observed that a lexically scoped LISP evaluator
        * need never save the environment over the evaluation of the
        * last argument in a procedure call (see [Wand]); this is called
        * "evils tail-recursion".
        *)
       Label "ev-appl-last-arg",
       Assign ("continue", L "ev-appl-accum-last-arg"),
       Goto (L "eval-dispatch"),
       Label "ev-appl-accum-last-arg",
       Restore "argl",
       AssignOp ("argl", "adjoin-arg", [R "val", R "argl"]),
       Restore "proc",
       Goto (L "apply-dispatch"),
       (* Procedure application *)
       Label "apply-dispatch",
       Test ("primitive-procedure?", [R "proc"]),
       Branch (L "primitive-apply"),
       Test ("compound-procedure?", [R "proc"]),
       Branch (L "compound-apply"),
       Goto (L "unknown-procedure-type"),
       Label "primitive-apply",
       AssignOp ("val", "apply-primitive-procedure",
                 [R "proc", R "argl"]),
       Restore "continue",
       Goto (R "continue"),
       Label "compound-apply",
       AssignOp ("unev", "procedure-parameters", [R "proc"]),
       AssignOp ("env", "procedure-environment", [R "proc"]),
       AssignOp ("env", "extend-environment",
                 [R "unev", R "argl", R "env"]),
       AssignOp ("unev", "procedure-body", [R "proc"]),
       Goto (L "ev-sequence"),
       (* Sequence *)
       Label "ev-begin",
       AssignOp ("unev", "begin-actions", [R "exp"]),
       Save "continue",
       Goto (L "ev-sequence"),
       Label "ev-sequence",
       AssignOp ("exp", "first-exp", [R "unev"]),
       Test ("last-exp?", [R "unev"]),
       Branch (L "ev-sequence-last-exp"),
       Save "unev",
       Save "env",
       Assign ("continue", L "ev-sequence-continue"),
       Goto (L "eval-dispatch"),
       Label "ev-sequence-continue",
       Restore "env",
       Restore "unev",
       AssignOp ("unev", "rest-exps", [R "unev"]),
       Goto (L "ev-sequence"),
       (*
        * from "5.4.2 Sequence Evaluation and Tail Recursion" of SICP;
        * ... Rather than setting up continue to arrange for eval-dispatch
        * to return here and then restoring continue from the stack and
        * continuing at that entry point, we restore continue from the
        * stack before going to eval-dispatch, so that eval-dispatch will
        * continue at that entry point after evaluating the expression.
        *)
       Label "ev-sequence-last-exp",
       Restore "continue",
       Goto (L "eval-dispatch"),
       (* Conditionals *)
       Label "ev-if",
       Save "exp",                (* save expression for later *)
       Save "env",
       Save "continue",
       Assign ("continue", L "ev-if-decide"),
       AssignOp ("exp", "if-predicate", [R "exp"]),
       Goto (L "eval-dispatch"),  (* evaluate the predicate *)
       Label "ev-if-decide",
       Restore "continue",
       Restore "env",
       Restore "exp",
       Test ("true?", [R "val"]),
       Branch (L "ev-if-consequent"),
       Label "ev-if-alternative",
       AssignOp ("exp", "if-alternative", [R "exp"]),
       Goto (L "eval-dispatch"),
       Label "ev-if-consequent",
       AssignOp ("exp", "if-consequent", [R "exp"]),
       Goto (L "eval-dispatch"),
       (* Assignments *)
       Label "ev-assignment",
       AssignOp ("unev", "assignment-variable", [R "exp"]),
       Save "unev",               (* save variable for later *)
       AssignOp ("exp", "assignment-value", [R "exp"]),
       Save "env",
       Save "continue",
       Assign ("continue", L "ev-assignment-1"),
       Goto (L "eval-dispatch"),  (* evaluate the assignment value *)
       Label "ev-assignment-1",
       Restore "continue",
       Restore "env",
       Restore "unev",
       Perform ("set-variable-value!", [R "unev", R "val", R "env"]),
       Assign ("val", C Obj.undef),
       Goto (R "continue"),
       (* Definitions *)
       Label "ev-definition",
       AssignOp ("unev", "definition-variable", [R "exp"]),
       Save "unev",               (* save variable for later *)
       AssignOp ("exp", "definition-value", [R "exp"]),
       Save "env",
       Save "continue",
       Assign ("continue", L "ev-definition-1"),
       Goto (L "eval-dispatch"),  (* evaluate the definition value *)
       Label "ev-definition-1",
       Restore "continue",
       Restore "env",
       Restore "unev",
       Perform ("define-variable!", [R "unev", R "val", R "env"]),
       Assign ("val", C Obj.undef),
       Goto (R "continue"),
       (* Derived expressions: for Exercise 5.23 *)
       Label "ev-derived",
       AssignOp ("exp", "expand-derived", [R "exp"]),
       Goto (L "eval-dispatch"),
       (* Read-Eval-Print-Loop and error handlers *)
       Label "read-eval-print-loop",
       Perform ("initialize-stack", []),
       Perform ("prompt-for-input", [C inputPrompt]),
       AssignOp ("exp", "read", []),
       Test ("eq?", [R "exp", C Obj.eof]), (* eof? *)
       Branch (L "main-done"),
       Test ("eq?", [R "exp", C quit]), (* :q? *)
       Branch (L "main-done"),
       AssignOp ("env", "get-global-environment", []),
       Assign ("continue", L "print-result"),
       Goto (L "eval-dispatch"),
       Label "print-result",
       Perform ("print-stack-stats", []),
       Perform ("announce-output", [C outputPrompt]),
       Perform ("user-print", [R "val"]),
       Goto (L "read-eval-print-loop"),
       Label "unknown-expression-type",
       Assign ("val", C (Obj.str "Error: Unknown expression type")),
       Goto (L "signal-error"),
       Label "unknown-procedure-type",
       Restore "continue",    (* clean up stack (from apply-dispatch) *)
       Assign ("val", C (Obj.str "Error: Unknown procedure type")),
       Goto (L "signal-error"),
       Label "signal-error",
       Perform ("user-print", [R "val"]),
       Goto (L "read-eval-print-loop"),
       Label "main-done"]

  fun repl rt =
      let
        (* operators that depend on rt (lisp runtime) *)
        val ops' =
            [Ope.fromFn1 ("prompt-for-input",
                          (fn msg =>
                              (Printer.format (stdOut rt,
                                               Obj.toString msg, nil);
                               Printer.terpri (stdOut rt);
                               Obj.undef))),
             Ope.fromFn0 ("read",
                          (fn () => Reader.read (stdIn rt))),
             Ope.fromFn0 ("get-global-environment",
                          (fn () => env rt)),
             Ope.fromFn1 ("announce-output",
                          (fn msg =>
                              (Printer.format (stdOut rt,
                                               Obj.toString msg, nil);
                               Printer.terpri (stdOut rt);
                               Obj.undef))),
             Ope.fromFn1 ("user-print",
                          (fn obj =>
                              (Printer.print (stdOut rt, obj);
                               Printer.terpri (stdOut rt);
                               Obj.undef)))]

        (* machine model *)
        val m = M.make (regs, ops' @ ops, ctrls)
      in
        M.start m
      end

  fun go () =
      let
        val rt = makeRuntime ()
      in
        repl rt
        handle e => ignore (Printer.printException (stdErr rt, e))
      end
end;

local
  structure Lisp : LISP =
  struct
    structure Obj
      = LispObjectFn (structure Env = Env)
    structure Syntax
      = LispSyntaxFn (structure Obj = Obj)
    structure Reader
      = LispReaderFn (structure Obj = Obj and Syntax = Syntax)
    structure Printer
      = LispPrinterFn (structure Obj = Obj and Syntax = Syntax)
    structure Evaluator
      = LispEvaluatorFn (structure Obj = Obj and Syntax = Syntax)
  end
  structure Runtime = LispRuntimeFn (Lisp)
in
structure ECI = ExplicitControlInterpreterFn (Runtime)
end;

(*
 * ECI.go (); (* => activates top-level *)
 *)
