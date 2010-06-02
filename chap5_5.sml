(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml, chap4_1.sml, chap5_2.sml, chap5_4.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 5  Computing with Register Machines *)
(* 5.5  Compilation *)
(* 5.5.1  Structure of the Compiler *)
(* 5.5.2  Compiling Expressions *)
(* 5.5.3  Compiling Combinations *)
(* 5.5.4  Combining Instruction Sequences *)
(* 5.5.5  An Example of Compiled Code *)
(* 5.5.6  Lexical Addressing *)
(* 5.5.7  Interfacing Compiled Code to the Evaluator *)

signature LISP_COMPILER =
sig
  (* type of source program *)
  type src

  (* type of object program *)
  type obj

  (* type of target, which specifies the register in which the
   * compiled code is to return the value of the expression. *)
  type target

  (* type of linkage, which describes how the code resulting
   * from the compilation of the expression should proceed
   * when it has finished its execution. *)
  type linkage

  (* linkage for "next" *)
  val next : linkage
  (* linkage for "return" *)
  val return : linkage
  (* linkage for "label" *)
  val label : string -> linkage
  (* the top-level dispatch in the compiler. *)
  val compile : src -> target -> linkage -> obj
  (* dump compile output *)
  val dump : TextIO.outstream * obj -> unit
end;

functor LispCompilerFn (structure Obj: LISP_OBJECT
                        and Syntax: LISP_SYNTAX
                        and M: REGISTER_MACHINE
                        sharing type Syntax.obj = Obj.t
                        and type M.value = Obj.t)
        : LISP_COMPILER =
struct
  (* src: lisp object (sexp) *)
  type src = Obj.t
  (* obj: instruction sequence *)
  type obj = string list * string list * M.ctrl list
  (* target: name of a register *)
  type target = string
  (* linkage *)
  datatype linkage = Next | Return | Label of string

  val next = Next
  val return = Return
  val label = Label

  val allRegs = ["env", "proc", "val", "argl", "continue"]
  val labelCounter = ref 0

  fun compile exp target linkage : obj =
      if Syntax.isSelfEvaluating exp then
        compileSelfEvaluating exp target linkage
      else if Syntax.isQuoted exp then
        compileQuoted exp target linkage
      else if Syntax.isVariable exp then
        compileVariable exp target linkage
      else if Syntax.isAssignment exp then
        compileAssignment exp target linkage
      else if Syntax.isDefinition exp then
        compileDefinition exp target linkage
      else if Syntax.isIf exp then
        compileIf exp target linkage
      else if Syntax.isLambda exp then
        compileLambda exp target linkage
      else if Syntax.isBegin exp then
        compileSequence (Syntax.beginActions exp) target linkage
      else if Syntax.isDerived exp then
        compile (Syntax.expandDerived exp) target linkage
      else if Syntax.isApplication exp then
        compileApplication exp target linkage
      else
        raise Obj.Error ("Unknown expression type -- compile: ~S",
                         [exp])

  and compileSelfEvaluating exp target linkage =
      let
        val iseq = ([],[target],
                    [M.Assign (target, M.C exp)])
      in
        endWithLinkage linkage iseq
      end

  and compileQuoted exp target linkage =
      let
        val text = Syntax.textOfQuotation exp
        val iseq = ([],[target],
                    [M.Assign (target, M.C text)])
      in
        endWithLinkage linkage iseq
      end

  and compileVariable exp target linkage =
      let
        val iseq = (["env"], [target],
                    [M.AssignOp (target,
                                 "lookup-variable-value",
                                 [M.C exp,
                                  M.R "env"])])
      in
        endWithLinkage linkage iseq
      end

  and compileAssignment exp target linkage =
      let
        val var = Syntax.assignmentVariable exp
        val valu = Syntax.assignmentValue exp
        val valuCode = compile valu "val" Next
        val iseq = (["env","val"], [target],
                    [M.Perform ("set-variable-value!",
                                [M.C var,
                                 M.R "val",
                                 M.R "env"]),
                     M.Assign (target,
                               M.C Obj.undef)])
      in
        endWithLinkage linkage (preserving ["env"] (valuCode, iseq))
      end

  and compileDefinition exp target linkage =
      let
        val var = Syntax.definitionVariable exp
        val valu = Syntax.definitionValue exp
        val valuCode = compile valu "val" Next
        val iseq = (["env","val"], [target],
                    [M.Perform ("define-variable!",
                                [M.C var,
                                 M.R "val",
                                 M.R "env"]),
                     M.Assign (target,
                               M.C Obj.undef)])
      in
        endWithLinkage linkage (preserving ["env"] (valuCode, iseq))
      end

  and compileIf exp target linkage =
      let
        val pred = Syntax.ifPredicate exp
        val con = Syntax.ifConsequent exp
        val alt = Syntax.ifAlternative exp
        val tBranch = makeLabel "true-branch"
        val fBranch = makeLabel "false-branch"
        val afterIf = makeLabel "after-if"
        val conLinkage = case linkage of
                           Next => Label afterIf
                         | _ => linkage
        val pCode = compile pred "val" Next
        val cCode = compile con target conLinkage
        val aCode = compile alt target linkage
        val iseq1 = (["val"], [],
                     [M.Test ("false?", [M.R "val"]),
                      M.Branch (M.L fBranch)])
        val iseq2 = parallelISeq (appendISeq [toISeq tBranch, cCode],
                                  appendISeq [toISeq fBranch, aCode])
        val iseq3 = toISeq afterIf
      in
        preserving ["env", "continue"]
                   (pCode, appendISeq [iseq1, iseq2, iseq3])
      end

  and compileSequence exps target linkage =
      if Obj.isNull (Obj.cdr exps) then
        compile (Obj.car exps) target linkage
      else
        preserving ["env","continue"]
                   (compile (Obj.car exps) target Next,
                    compileSequence (Obj.cdr exps) target linkage)

  and compileLambda exp target linkage =
      let
        val procEntry = makeLabel "entry"
        val afterLambda = makeLabel "after-lambda"
        val lambdaLinkage = case linkage of
                              Next => Label afterLambda
                            | _ => linkage
        val iseq0 = (["env"], [target],
                     [M.AssignOp (target,
                                  "make-compiled-procedure",
                                  [M.L procEntry,
                                   M.R "env"])])
        val iseq1 = tackOnISeq (endWithLinkage lambdaLinkage iseq0,
                                compileLambdaBody exp procEntry)
        val iseq2 = toISeq afterLambda
      in
        appendISeq [iseq1, iseq2]
      end

  and compileLambdaBody exp procEntry =
      let
        val formals = Syntax.lambdaParameters exp
        val body = Syntax.lambdaBody exp
        val iseq1 = (["env","proc","argl"],["env"],
                     [M.Label procEntry,
                      M.AssignOp ("env",
                                  "compiled-procedure-env",
                                  [M.R "proc"]),
                      M.AssignOp ("env",
                                  "extend-environment",
                                  [M.C formals,
                                   M.R "argl",
                                   M.R "env"])])
        val iseq2 = compileSequence body "val" Return
      in
        appendISeq [iseq1, iseq2]
      end

  and compileApplication exp target linkage =
      let
        val procCode =
            compile (Syntax.operator exp) "proc" Next
        val operandCodes =
            map (fn operand => compile operand "val" Next)
                (Obj.toList (Syntax.operands exp))
      in
        preserving ["env","continue"]
                   (procCode,
                    preserving ["proc","continue"]
                               (constructArglist operandCodes,
                                compileProcedureCall target linkage))
      end

  and constructArglist operandCodes =
      case rev operandCodes of
        [] => ([],["argl"],
               [M.Assign ("argl", M.C Obj.null)])
      | (operandCode'::operandCodes') =>
        let
          val codeToGetLastArg =
              appendISeq [operandCode',
                          (["val"],["argl"],
                           [M.AssignOp ("argl", "list",
                                        [M.R "val"])])]
        in
          case operandCodes' of
            [] => codeToGetLastArg
          | _ =>
            preserving ["env"]
                       (codeToGetLastArg,
                        codeToGetRestArgs operandCodes')
        end

  and codeToGetRestArgs [] =
      raise Obj.Error ("operandCodes is empty", nil)
    | codeToGetRestArgs (operandCode::operandCodes) =
      let
        val codeForNextArg =
            preserving ["argl"]
                       (operandCode,
                        (["val", "argl"],["argl"],
                         [M.AssignOp ("argl", "cons",
                                      [M.R "val",
                                       M.R "argl"])]))
      in
        case operandCodes of
          [] => codeForNextArg
        | _ => preserving ["env"]
                          (codeForNextArg,
                           codeToGetRestArgs operandCodes)
      end

  and compileProcedureCall target linkage =
      let
        val primBranch = makeLabel "primitive-branch"
        val compBranch = makeLabel "compiled-branch"
        val afterCall = makeLabel "after-call"
        val compLinkage = case linkage of
                            Next => Label afterCall
                          | _ => linkage
        val iseq1 =
            (["proc"],[],
             [M.Test ("primitive-procedure?", [M.R "proc"]),
              M.Branch (M.L primBranch)])
        val iseq2a =
            appendISeq [toISeq compBranch,
                        compileProcAppl target compLinkage]
        val iseq2b =
            appendISeq [toISeq primBranch,
                        endWithLinkage
                            linkage
                            (["proc","argl"], [target],
                             [M.AssignOp (target,
                                          "apply-primitive-procedure",
                                          [M.R "proc",
                                           M.R "argl"])])]
        val iseq2 = parallelISeq (iseq2a, iseq2b)
        val iseq3 = toISeq afterCall
      in
        appendISeq [iseq1, iseq2, iseq3]
      end

  and compileProcAppl target linkage =
      if target = "val" then
        case linkage of
          Return =>
          (* target == val && linkage == Return *)
          (["proc","continue"],allRegs,
           [M.AssignOp ("val", "compiled-procedure-entry",
                        [M.R "proc"]),
            M.Goto (M.R "val")])
        | Label name =>
          (* target == val && linkage == Label *)
          (["proc"],allRegs,
           [M.Assign ("continue", M.L name),
            M.AssignOp ("val", "compiled-procedure-entry",
                        [M.R "proc"]),
            M.Goto (M.R "val")])
        | Next =>
          (* target == val && linkage == Next *)
          raise Obj.Error ("Unexpected linkage: Next", nil)
      else
        case linkage of
          Return =>
          (* target != val && linkage == Return *)
          raise Obj.Error ("Return linkage, target not val: "^
                           target, nil)
        | Label name =>
          (* target != val && linkage == Label *)
          let
            val procReturn = makeLabel "proc-return"
          in
            (["proc"],allRegs,
             [M.Assign ("continue", M.L procReturn),
              M.AssignOp ("val", "compiled-procedure-entry",
                          [M.R "proc"]),
              M.Goto (M.R "val"),
              M.Label procReturn,
              M.Assign (target, M.R "val"),
              M.Goto (M.L name)])
          end
        | Next =>
          (* target != val && linkage == Next *)
          raise Obj.Error ("Unexpected linkage: Next", nil)

  and compileLinkage Return : obj =
      (["continue"],[],[M.Goto (M.R "continue")])
    | compileLinkage Next =
      ([],[],[])
    | compileLinkage (Label name) =
      ([],[],[M.Goto (M.L name)])

  and endWithLinkage linkage iseq : obj =
      preserving ["continue"] (iseq, compileLinkage linkage)

  and preserving nil (iseq1, iseq2) = appendISeq [iseq1, iseq2]
    | preserving (reg::regs) (iseq1, iseq2) =
      if needsRegister iseq2 reg andalso
         modifiesRegister iseq1 reg
      then
        let
          val (needed1, modified1, statements1) = iseq1
        in
          preserving regs
                     ((listUnion ([reg], needed1),
                       listDiff (modified1, [reg]),
                       [M.Save reg] @
                       statements1 @
                       [M.Restore reg]),
                      iseq2)
        end
      else
        preserving regs (iseq1, iseq2)

  and appendISeq nil = ([],[],[])
    | appendISeq (iseq::iseqs) =
      appendISeq2 (iseq, appendISeq iseqs)

  and appendISeq2 ((needed1, modified1, statements1),
                   (needed2, modified2, statements2)) =
      (listUnion (needed1, listDiff (needed2, modified1)),
       listUnion (modified1, modified2),
       statements1 @ statements2)

  and parallelISeq ((needed1, modified1, statements1),
                    (needed2, modified2, statements2)) =
      (listUnion (needed1, needed2),
       listUnion (modified1, modified2),
       statements1 @ statements2)

  and tackOnISeq ((needed1, modified1, statements1),
                  (_, _, statements2)) =
      (needed1,
       modified1,
       statements1 @ statements2)

  and needsRegister (needed, _, _) reg =
      List.exists (fn name => name = reg) needed

  and modifiesRegister (_, modified, _) reg =
      List.exists (fn name => name = reg) modified

  and listUnion (nil, s2) = s2
    | listUnion (h1::t1, s2) =
      if List.exists (fn a => a = h1) s2 then
        listUnion (t1, s2)
      else
        h1 :: (listUnion (t1, s2))

  and listDiff (nil, s2) = nil
    | listDiff (h1::t1, s2) =
      if List.exists (fn a => a = h1) s2 then
        listDiff (t1, s2)
      else
        h1 :: (listDiff (t1, s2))

  and makeLabel name =
      name ^ "-" ^ Int.toString (newLabelNumber ())

  and newLabelNumber () =
      !labelCounter before labelCounter := (!labelCounter) + 1

  and toISeq name : obj =
      ([],[],[M.Label name])

  fun dump (out, (needed, modified, statements)) =
      let
        val p = fn v => TextIO.output (out, v)
        val pCtrl = fn v => (M.printCtrl (out, v); p "\n")
        fun p1 pv nil = p "[]"
          | p1 pv (v::vs) = (p "["; pv v; p2 pv vs)
        and p2 pv nil = p "]"
          | p2 pv (v::vs) = (p " "; pv v; p2 pv vs)
      in
        p "needed:\n";
        p1 p needed;
        p "\n";
        p "modified:\n";
        p1 p modified;
        p "\n";
        p "statements:\n";
        p1 pCtrl statements;
        p "\n"
      end
end;

local
  open LispRegisterMachine

  structure Compiler
    = LispCompilerFn (structure Obj = Obj and Syntax = Syntax and M = M)
in
(* compile and dump instruction sequence *)
fun compileAndDump () =
    let
      val (rt, m) = make ()
      val _ = Printer.format (stdOut rt, "input: ", nil)
      val exp = Reader.read (stdIn rt)
      val out = Obj.toOutstream (stdOut rt)
    in
      Compiler.dump (out, Compiler.compile exp "val" Compiler.next)
      handle e => ignore (Printer.printException (stdErr rt, e))
    end

(* Exercise 5.48. (explicit-control evaluator with compile-and-run) *)
fun startRepl' debug =
    let
      val (rt, m) = make ()
      val ops =
          [("compile",
            (fn [M.Value exp] =>
                let
                  val obj as (_,_,statements) =
                      Compiler.compile exp "val" Compiler.return
                  val insts = M.assemble m statements
                  fun debugDump () =
                      if !M.debug then
                        let val out = Obj.toOutstream (stdOut rt)
                        in Compiler.dump (out, obj) end
                      else ()
                in
                  debugDump ();
                  M.Insts insts
                end
              | _ => raise Fail ("Unexpected arguments: compile")))]
    in
      M.debug := debug;
      addOps m ops;
      M.start m
      handle e => ignore (Printer.printException (stdErr rt, e))
    end
end;

(*
 * compileAndDump ();  (* => compile and dump instruction sequence *)
 * startRepl' <debug>; (* => activates top-level *)
 *)

(*
 * [startRepl' supports compile-and-run feature]
 * Lisp expressions can be compiled by executing
 * the following at the lisp top-level:
 * :c <exp>  ; <exp> means any lisp expression
 *)

structure LispRegisterMachineAndCompiler
          : sig
            include LISP_REGISTER_MACHINE
            structure Compiler : LISP_COMPILER
            end =
struct
  open DefaultLispRuntime

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
  structure Compiler
    = LispCompilerFn (structure Obj = Obj and Syntax = Syntax and M = M)
  end

  (* constants *)
  val quit = Obj.sym ":q"
  val inputPrompt = Obj.str ";;; EC-Eval input:"
  val outputPrompt = Obj.str ";;; EC-Eval value:"

  (* registers ("exp", "unev" => not used!) *)
  val regs =
      ["env", "val", "continue", "proc", "argl"]

  (* operators *)
  local
    (* special operators *)
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
      [M.fromFn2 ("lookup-variable-value",
                  (fn (exp, env) => Obj.lookupEnv env exp)),
       M.fromFn3 ("set-variable-value!",
                  (fn (var, valu, env) =>
                      (Obj.setEnv env (var, valu); var))),
       M.fromFn3 ("define-variable!",
                  (fn (var, valu, env) =>
                      (Obj.defineEnv env (var, valu); var))),
       M.fromFn1 ("true?",
                  Obj.bool o Obj.isTrue),
       M.fromFn1 ("false?",
                  Obj.bool o Obj.isFalse),
       M.fromFn2 ("eq?",
                  Obj.bool o Obj.eq),
       M.fromFn2 ("equal?",
                  Obj.bool o Obj.equal),
       opeList "list",
       M.fromFn2 ("cons", Obj.cons),
       M.fromFn1 ("primitive-procedure?",
                  Obj.bool o Obj.isSubr),
       M.fromFn1 ("compound-procedure?",
                  Obj.bool o Obj.isExpr),
       M.fromFn1 ("compiled-procedure?",
                  Obj.bool o Obj.isCexpr),
       M.fromFn2 ("apply-primitive-procedure",
                  (fn (proc, args) =>
                      Obj.applySubr proc (Obj.toList args))),
       M.fromFn3 ("make-procedure",
                  Obj.expr),
       M.fromFn1 ("procedure-parameters",
                  Obj.exprParams),
       M.fromFn1 ("procedure-environment",
                  Obj.exprEnv),
       M.fromFn1 ("procedure-body",
                  Obj.exprBody),
       opeMakeCompProc "make-compiled-procedure",
       opeCompProcEntry "compiled-procedure-entry",
       M.fromFn1 ("compiled-procedure-env",
                  Obj.cexprEnv),
       M.fromFn3 ("extend-environment",
                  (fn (vars, vals, env) =>
                      Obj.extendEnv env (Obj.toList vars,
                                         Obj.toList vals)))]
  end

  (* control-text *)
  val ctrls =
      [M.Label "read-compile-execute-print-loop",
       M.Perform ("initialize-stack", []),
       M.Perform ("prompt-for-input", [M.C inputPrompt]),
       M.AssignOp ("val", "read", []),
       M.Test ("eq?", [M.R "val", M.C Obj.eof]), (* eof? *)
       M.Branch (M.L "return-from-read-compile-execute-print-loop"),
       M.Test ("eq?", [M.R "val", M.C quit]), (* :q? *)
       M.Branch (M.L "return-from-read-compile-execute-print-loop"),
       M.AssignOp ("val", "compile", [M.R "val"]),
       M.AssignOp ("env", "get-global-environment", []),
       M.Assign ("continue", M.L "print-result"),
       M.Goto (M.R "val"),
       M.Label "print-result",
       M.Perform ("print-stack-stats", []),
       M.Perform ("announce-output", [M.C outputPrompt]),
       M.Perform ("user-print", [M.R "val"]),
       M.Goto (M.L "read-compile-execute-print-loop"),
       M.Label "return-from-read-compile-execute-print-loop"]

  fun make () =
      let
        (* lisp runtime *)
        val rt = makeRuntime ()
        (* machine model with empty ops and empty ctrls *)
        val m = M.makeMachine (regs, nil, nil)
        (* operators that depend on rt or m *)
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
                             Obj.undef))),
             ("compile",
              (fn [M.Value exp] =>
                  let
                    val obj as (_,_,statements) =
                        Compiler.compile exp "val" Compiler.return
                    val insts = M.assemble m statements
                    fun debugDump () =
                        if !M.debug then
                          let val out = Obj.toOutstream (stdOut rt)
                          in Compiler.dump (out, obj) end
                        else ()
                  in
                    debugDump ();
                    M.Insts insts
                  end
                | _ => raise Fail ("Unexpected arguments: compile")))]
      in
        M.installOps m (ops' @ ops);
        M.installInsts m (M.assemble m ctrls);
        (rt, m)
      end

  and addOps m ops =
      (M.installOps m ops;
       M.installInsts m (M.assemble m ctrls)) (* rebuild insts *)
end;

local
  open LispRegisterMachineAndCompiler
in
(* Exercise 5.49. (read-compile-execute-print-loop) *)
fun startRepl'' debug =
    let
      val (rt, m) = make ()
    in
      M.debug := debug;
      M.start m
      handle e => ignore (Printer.printException (stdErr rt, e))
    end
end;

(*
 * startRepl'' <debug>; (* => activates top-level *)
 *)
