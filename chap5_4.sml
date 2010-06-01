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

signature LISP_REGISTER_MACHINE =
sig
  include LISP_RUNTIME

  structure M : REGISTER_MACHINE
  sharing type Obj.t = M.value

  val regs : string list
  val ops : (string * M.ope) list
  val ctrls : M.ctrl list
  val make : unit -> rt * M.mm
  val addOps : M.mm -> (string * M.ope) list -> unit
end;

structure LispRegisterMachine : LISP_REGISTER_MACHINE =
struct
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
  in
  structure LispRuntime = LispRuntimeFn (Lisp)
  end

  open LispRuntime

  local
    structure Conf : REGISTER_MACHINE_CONF =
    struct
      structure Register = Register
      structure Stack = Stack'
      type value = Obj.t
      val undef = Obj.undef
      val isTrue = Obj.isTrue
      val printValue =
       fn (s, v) => ignore (Printer.print (Obj.outputPort s, v))
    end
  in
  structure M = RegisterMachineFn (Conf)
  end

  (* constants *)
  val quit = Obj.sym ":q"
  val compileAndRun = Obj.sym ":c"
  val inputPrompt = Obj.str ";;; EC-Eval input:"
  val outputPrompt = Obj.str ";;; EC-Eval value:"

  (* registers *)
  val regs =
      ["exp", "env", "val", "continue", "proc", "argl", "unev"]

  (* operators *)
  local
    (* special operators for chap5_5.sml *)
    fun opeMakeCompProc name =
        (name, (fn [M.Insts i, M.Value v] => M.Value (Obj.cexpr (i, v))
                 | _ => raise Fail ("Unexpected arguments: "^name)))
    fun opeCompProcEntry name =
        (name, (fn [M.Value v] => M.Insts (Obj.cexprEntry v)
                 | _ => raise Fail ("Unexpected arguments: "^name)))
    fun opeList name =
        (name, (fn args =>
                   let
                     fun f nil = Obj.null
                       | f (M.Value v::args) = Obj.cons (v, f args)
                       | f _ = raise Fail ("Unexpected arguments: "^name)
                   in
                     M.Value (f args)
                   end))
  in
  val ops =
      [M.fromFn1 ("self-evaluating?",
                  Obj.bool o Syntax.isSelfEvaluating),
       M.fromFn1 ("variable?",
                  Obj.bool o Syntax.isVariable),
       M.fromFn1 ("quoted?",
                  Obj.bool o Syntax.isQuoted),
       M.fromFn1 ("assignment?",
                  Obj.bool o Syntax.isAssignment),
       M.fromFn1 ("definition?",
                  Obj.bool o Syntax.isDefinition),
       M.fromFn1 ("if?",
                  Obj.bool o Syntax.isIf),
       M.fromFn1 ("lambda?",
                  Obj.bool o Syntax.isLambda),
       M.fromFn1 ("begin?",
                  Obj.bool o Syntax.isBegin),
       M.fromFn1 ("application?",
                  Obj.bool o Syntax.isApplication),
       M.fromFn2 ("lookup-variable-value",
                  (fn (exp, env) => Obj.lookupEnv env exp)),
       M.fromFn1 ("text-of-quotation",
                  Syntax.textOfQuotation),
       M.fromFn1 ("lambda-parameters",
                  Syntax.lambdaParameters),
       M.fromFn1 ("lambda-body",
                  Syntax.lambdaBody),
       M.fromFn3 ("make-procedure",
                  Obj.expr),
       M.fromFn1 ("operands",
                  Syntax.operands),
       M.fromFn1 ("operator",
                  Syntax.operator),
       M.fromFn0 ("empty-arglist",
                  (fn () => Obj.null)),
       M.fromFn1 ("no-operands?",
                  Obj.bool o Obj.isNull),
       M.fromFn1 ("first-operand",
                  Obj.car),
       M.fromFn1 ("last-operand?",
                  Obj.bool o Obj.isNull o Obj.cdr),
       M.fromFn2 ("adjoin-arg",
                  (fn (arg, args) =>
                      Obj.append (args, Obj.fromList [arg]))),
       M.fromFn1 ("rest-operands",
                  Obj.cdr),
       M.fromFn1 ("primitive-procedure?",
                  Obj.bool o Obj.isSubr),
       M.fromFn1 ("compound-procedure?",
                  Obj.bool o Obj.isExpr),
       M.fromFn2 ("apply-primitive-procedure",
                  (fn (proc, args) =>
                      Obj.applySubr proc (Obj.toList args))),
       M.fromFn1 ("procedure-parameters",
                  Obj.exprParams),
       M.fromFn1 ("procedure-environment",
                  Obj.exprEnv),
       M.fromFn3 ("extend-environment",
                  (fn (vars, vals, env) =>
                      Obj.extendEnv env (Obj.toList vars,
                                         Obj.toList vals))),
       M.fromFn1 ("procedure-body",
                  Obj.exprBody),
       M.fromFn1 ("begin-actions",
                  Syntax.beginActions),
       M.fromFn1 ("first-exp",
                  Obj.car),
       M.fromFn1 ("last-exp?",
                  Obj.bool o Obj.isNull o Obj.cdr),
       M.fromFn1 ("rest-exps",
                  Obj.cdr),
       M.fromFn1 ("no-more-exps?",
                  Obj.bool o Obj.isNull),
       M.fromFn1 ("if-predicate",
                  Syntax.ifPredicate),
       M.fromFn1 ("true?",
                  Obj.bool o Obj.isTrue),
       M.fromFn1 ("false?",
                  Obj.bool o Obj.isFalse),
       M.fromFn1 ("if-alternative",
                  Syntax.ifAlternative),
       M.fromFn1 ("if-consequent",
                  Syntax.ifConsequent),
       M.fromFn1 ("assignment-variable",
                  Syntax.assignmentVariable),
       M.fromFn1 ("assignment-value",
                  Syntax.assignmentValue),
       M.fromFn3 ("set-variable-value!",
                  (fn (var, valu, env) =>
                      (Obj.setEnv env (var, valu); var))),
       M.fromFn1 ("definition-variable",
                  Syntax.definitionVariable),
       M.fromFn1 ("definition-value",
                  Syntax.definitionValue),
       M.fromFn3 ("define-variable!",
                  (fn (var, valu, env) =>
                      (Obj.defineEnv env (var, valu); var))),
       M.fromFn2 ("eq?",
                  Obj.bool o Obj.eq),
       M.fromFn2 ("equal?",
                  Obj.bool o Obj.equal),
       M.fromFn1 ("derived?",       (* for Exercise 5.23 *)
                  Obj.bool o Syntax.isDerived),
       M.fromFn1 ("expand-derived", (* for Exercise 5.23 *)
                  Syntax.expandDerived),
       M.fromFn1 ("compiled-procedure?",            (* for chap5_5.sml *)
                  Obj.bool o Obj.isCexpr),
       opeMakeCompProc "make-compiled-procedure",   (* for chap5_5.sml *)
       opeCompProcEntry "compiled-procedure-entry", (* for chap5_5.sml *)
       M.fromFn1 ("compiled-procedure-env",         (* for chap5_5.sml *)
                  Obj.cexprEnv),
       M.fromFn1 ("compile",                        (* for chap5_5.sml *)
                  (fn _ => raise Fail ("compile operation will be "^
                                       "defined in chap5_5.sml."))),
       opeList "list",
       M.fromFn2 ("cons", Obj.cons)]
  end

  (* control-text *)
  val ctrls =
      [M.Label "main",
       M.Assign ("continue", M.L "return-from-main"),
       M.Goto (M.L "read-eval-print-loop"),

       (* Eval-dispatch *)
       M.Label "eval-dispatch",
       M.Test ("self-evaluating?", [M.R "exp"]),
       M.Branch (M.L "ev-self-eval"),
       M.Test ("variable?", [M.R "exp"]),
       M.Branch (M.L "ev-variable"),
       M.Test ("quoted?", [M.R "exp"]),
       M.Branch (M.L "ev-quoted"),
       M.Test ("assignment?", [M.R "exp"]),
       M.Branch (M.L "ev-assignment"),
       M.Test ("definition?", [M.R "exp"]),
       M.Branch (M.L "ev-definition"),
       M.Test ("if?", [M.R "exp"]),
       M.Branch (M.L "ev-if"),
       M.Test ("lambda?", [M.R "exp"]),
       M.Branch (M.L "ev-lambda"),
       M.Test ("begin?", [M.R "exp"]),
       M.Branch (M.L "ev-begin"),
       M.Test ("derived?", [M.R "exp"]), (* for Exercise 5.23 *)
       M.Branch (M.L "ev-derived"),      (* for Exercise 5.23 *)
       M.Test ("application?", [M.R "exp"]),
       M.Branch (M.L "ev-application"),
       M.Goto (M.L "unknown-expression-type"),

       (* Evaluating simple expressions *)
       M.Label "ev-self-eval",
       M.Assign ("val", M.R "exp"),
       M.Goto (M.R "continue"),
       M.Label "ev-variable",
       M.AssignOp ("val", "lookup-variable-value",
                   [M.R "exp", M.R "env"]),
       M.Goto (M.R "continue"),
       M.Label "ev-quoted",
       M.AssignOp ("val", "text-of-quotation", [M.R "exp"]),
       M.Goto (M.R "continue"),
       M.Label "ev-lambda",
       M.AssignOp ("unev", "lambda-parameters", [M.R "exp"]),
       M.AssignOp ("exp", "lambda-body", [M.R "exp"]),
       M.AssignOp ("val", "make-procedure",
                   [M.R "unev", M.R "exp", M.R "env"]),
       M.Goto (M.R "continue"),

       (* Evaluating procedure applications *)
       M.Label "ev-application",
       M.Save "continue",
       M.Save "env",
       M.AssignOp ("unev", "operands", [M.R "exp"]),
       M.Save "unev",
       M.AssignOp ("exp", "operator", [M.R "exp"]),
       M.Assign ("continue", M.L "ev-appl-did-operator"),
       M.Goto (M.L "eval-dispatch"),
       M.Label "ev-appl-did-operator",
       M.Restore "unev",                  (* the operands *)
       M.Restore "env",
       M.AssignOp ("argl", "empty-arglist", []),
       M.Assign ("proc", M.R "val"),        (* the operator *)
       M.Test ("no-operands?", [M.R "unev"]),
       M.Branch (M.L "apply-dispatch"),
       M.Save "proc",
       M.Label "ev-appl-operand-loop",
       M.Save "argl",
       M.AssignOp ("exp", "first-operand", [M.R "unev"]),
       M.Test ("last-operand?", [M.R "unev"]),
       M.Branch (M.L "ev-appl-last-arg"),
       M.Save "env",
       M.Save "unev",
       M.Assign ("continue", M.L "ev-appl-accumulate-arg"),
       M.Goto (M.L "eval-dispatch"),
       M.Label "ev-appl-accumulate-arg",
       M.Restore "unev",
       M.Restore "env",
       M.Restore "argl",
       M.AssignOp ("argl", "adjoin-arg", [M.R "val", M.R "argl"]),
       M.AssignOp ("unev", "rest-operands", [M.R "unev"]),
       M.Goto (M.L "ev-appl-operand-loop"),
       (*
        * from http://aggregate.org/LAR/p163-steele.pdf;
        * ... Wand has observed that a lexically scoped LISP evaluator
        * need never save the environment over the evaluation of the
        * last argument in a procedure call (see [Wand]); this is called
        * "evils tail-recursion".
        *)
       M.Label "ev-appl-last-arg",
       M.Assign ("continue", M.L "ev-appl-accum-last-arg"),
       M.Goto (M.L "eval-dispatch"),
       M.Label "ev-appl-accum-last-arg",
       M.Restore "argl",
       M.AssignOp ("argl", "adjoin-arg", [M.R "val", M.R "argl"]),
       M.Restore "proc",
       M.Goto (M.L "apply-dispatch"),
       M.Label "apply-dispatch",
       M.Test ("primitive-procedure?", [M.R "proc"]),
       M.Branch (M.L "primitive-apply"),
       M.Test ("compound-procedure?", [M.R "proc"]),
       M.Branch (M.L "compound-apply"),
       M.Test ("compiled-procedure?", [M.R "proc"]),
       M.Branch (M.L "compiled-apply"),
       M.Goto (M.L "unknown-procedure-type"),
       M.Label "primitive-apply",
       M.AssignOp ("val", "apply-primitive-procedure",
                   [M.R "proc", M.R "argl"]),
       M.Restore "continue",
       M.Goto (M.R "continue"),
       M.Label "compound-apply",
       M.AssignOp ("unev", "procedure-parameters", [M.R "proc"]),
       M.AssignOp ("env", "procedure-environment", [M.R "proc"]),
       M.AssignOp ("env", "extend-environment",
                   [M.R "unev", M.R "argl", M.R "env"]),
       M.AssignOp ("unev", "procedure-body", [M.R "proc"]),
       M.Goto (M.L "ev-sequence"),
       M.Label "compiled-apply", (* for chap5_5.sml *)
       M.Restore "continue",
       M.AssignOp ("val", "compiled-procedure-entry", [M.R "proc"]),
       M.Goto (M.R "val"),

       (* Sequence *)
       M.Label "ev-begin",
       M.AssignOp ("unev", "begin-actions", [M.R "exp"]),
       M.Save "continue",
       M.Goto (M.L "ev-sequence"),
       M.Label "ev-sequence",
       M.AssignOp ("exp", "first-exp", [M.R "unev"]),
       M.Test ("last-exp?", [M.R "unev"]),
       M.Branch (M.L "ev-sequence-last-exp"),
       M.Save "unev",
       M.Save "env",
       M.Assign ("continue", M.L "ev-sequence-continue"),
       M.Goto (M.L "eval-dispatch"),
       M.Label "ev-sequence-continue",
       M.Restore "env",
       M.Restore "unev",
       M.AssignOp ("unev", "rest-exps", [M.R "unev"]),
       M.Goto (M.L "ev-sequence"),
       (*
        * from "5.4.2 Sequence Evaluation and Tail Recursion" of SICP;
        * ... Rather than setting up continue to arrange for eval-dispatch
        * to return here and then restoring continue from the stack and
        * continuing at that entry point, we restore continue from the
        * stack before going to eval-dispatch, so that eval-dispatch will
        * continue at that entry point after evaluating the expression.
        *)
       M.Label "ev-sequence-last-exp",
       M.Restore "continue",
       M.Goto (M.L "eval-dispatch"),

       (* Conditionals *)
       M.Label "ev-if",
       M.Save "exp",                (* save expression for later *)
       M.Save "env",
       M.Save "continue",
       M.Assign ("continue", M.L "ev-if-decide"),
       M.AssignOp ("exp", "if-predicate", [M.R "exp"]),
       M.Goto (M.L "eval-dispatch"),  (* evaluate the predicate *)
       M.Label "ev-if-decide",
       M.Restore "continue",
       M.Restore "env",
       M.Restore "exp",
       M.Test ("true?", [M.R "val"]),
       M.Branch (M.L "ev-if-consequent"),
       M.Label "ev-if-alternative",
       M.AssignOp ("exp", "if-alternative", [M.R "exp"]),
       M.Goto (M.L "eval-dispatch"),
       M.Label "ev-if-consequent",
       M.AssignOp ("exp", "if-consequent", [M.R "exp"]),
       M.Goto (M.L "eval-dispatch"),

       (* Assignments *)
       M.Label "ev-assignment",
       M.AssignOp ("unev", "assignment-variable", [M.R "exp"]),
       M.Save "unev",               (* save variable for later *)
       M.AssignOp ("exp", "assignment-value", [M.R "exp"]),
       M.Save "env",
       M.Save "continue",
       M.Assign ("continue", M.L "ev-assignment-1"),
       M.Goto (M.L "eval-dispatch"),  (* evaluate the assignment value *)
       M.Label "ev-assignment-1",
       M.Restore "continue",
       M.Restore "env",
       M.Restore "unev",
       M.Perform ("set-variable-value!", [M.R "unev", M.R "val", M.R "env"]),
       M.Assign ("val", M.C Obj.undef),
       M.Goto (M.R "continue"),

       (* Definitions *)
       M.Label "ev-definition",
       M.AssignOp ("unev", "definition-variable", [M.R "exp"]),
       M.Save "unev",               (* save variable for later *)
       M.AssignOp ("exp", "definition-value", [M.R "exp"]),
       M.Save "env",
       M.Save "continue",
       M.Assign ("continue", M.L "ev-definition-1"),
       M.Goto (M.L "eval-dispatch"),  (* evaluate the definition value *)
       M.Label "ev-definition-1",
       M.Restore "continue",
       M.Restore "env",
       M.Restore "unev",
       M.Perform ("define-variable!", [M.R "unev", M.R "val", M.R "env"]),
       M.Assign ("val", M.C Obj.undef),
       M.Goto (M.R "continue"),

       (* Derived expressions: for Exercise 5.23 *)
       M.Label "ev-derived",
       M.AssignOp ("exp", "expand-derived", [M.R "exp"]),
       M.Goto (M.L "eval-dispatch"),

       (* Read-Eval-Print-Loop and error handlers *)
       M.Label "read-eval-print-loop",
       M.Perform ("initialize-stack", []),
       M.Perform ("prompt-for-input", [M.C inputPrompt]),
       M.AssignOp ("exp", "read", []),
       M.Test ("eq?", [M.R "exp", M.C Obj.eof]), (* eof? *)
       M.Branch (M.L "return-from-main"),
       M.Test ("eq?", [M.R "exp", M.C quit]), (* :q? *)
       M.Branch (M.L "return-from-main"),
       M.Test ("eq?", [M.R "exp", M.C compileAndRun]), (* :c? *)
       M.Branch (M.L "external-entry"),
       M.AssignOp ("env", "get-global-environment", []),
       M.Assign ("continue", M.L "print-result"),
       M.Goto (M.L "eval-dispatch"),
       M.Label "print-result",
       M.Perform ("print-stack-stats", []),
       M.Perform ("announce-output", [M.C outputPrompt]),
       M.Perform ("user-print", [M.R "val"]),
       M.Goto (M.L "read-eval-print-loop"),
       M.Label "unknown-expression-type",
       M.Assign ("val", M.C (Obj.str "Error: Unknown expression type")),
       M.Goto (M.L "signal-error"),
       M.Label "unknown-procedure-type",
       M.Restore "continue",    (* clean up stack (from apply-dispatch) *)
       M.Assign ("val", M.C (Obj.str "Error: Unknown procedure type")),
       M.Goto (M.L "signal-error"),
       M.Label "signal-error",
       M.Perform ("user-print", [M.R "val"]),
       M.Goto (M.L "read-eval-print-loop"),

       (* for chap5_5.sml; compile-and-run *)
       M.Label "external-entry",
       M.Perform ("initialize-stack", []),
       M.AssignOp ("exp", "read", []),
       M.AssignOp ("val", "compile", [M.R "exp"]),
       M.AssignOp ("env", "get-global-environment", []),
       M.Assign ("continue", M.L "print-result"),
       M.Goto (M.R "val"),

       (* end of main *)
       M.Label "return-from-main"]

  fun make () =
      let
        (* lisp runtime *)
        val rt = makeRuntime ()
        (* operators that depend on rt (lisp runtime) *)
        val ops' =
            [M.fromFn1 ("prompt-for-input",
                        (fn msg =>
                            (Printer.format (stdOut rt,
                                             Obj.toString msg, nil);
                             Printer.terpri (stdOut rt);
                             Obj.undef))),
             M.fromFn0 ("read",
                        (fn () => Reader.read (stdIn rt))),
             M.fromFn0 ("get-global-environment",
                        (fn () => env rt)),
             M.fromFn1 ("announce-output",
                        (fn msg =>
                            (Printer.format (stdOut rt,
                                             Obj.toString msg, nil);
                             Printer.terpri (stdOut rt);
                             Obj.undef))),
             M.fromFn1 ("user-print",
                        (fn obj =>
                            (Printer.print (stdOut rt, obj);
                             Printer.terpri (stdOut rt);
                             Obj.undef)))]
        (* machine model *)
        val m = M.makeMachine (regs, ops' @ ops, ctrls)
      in
        (rt, m)
      end

  fun addOps m ops =
      (M.installOps m ops;
       M.installInsts m (M.assemble m ctrls)) (* rebuild insts *)
end;

local
  open LispRegisterMachine
in
(* explicit-control evaluator *)
fun startRepl debug =
    let
      val (rt, m) = make ()
    in
      M.debug := debug;
      M.start m
      handle e => ignore (Printer.printException (stdErr rt, e))
    end
end;

(*
 * startRepl <debug>; (* => activates top-level *)
 *)
