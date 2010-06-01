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
          (* target != val && linkage == Label *)
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

(* explicit-control evaluator with compile-and-run *)
fun startRepl' debug =
    let
      val (rt, m) = make ()
      fun opeCompile name =
        (name,
         (fn [M.Value exp] =>
             let
               val (_,_,statements) =
                   Compiler.compile exp "val" Compiler.return
               val insts = M.assemble m statements
             in
               M.Insts insts
             end
           | _ => raise Fail ("Unexpected arguments: "^name)))
    in
      M.debug := debug;
      addOps m [opeCompile "compile"];
      M.start m
      handle e => ignore (Printer.printException (stdErr rt, e))
    end
end;

(*
 * compileAndDump ();  (* => compile and dump instruction sequence *)
 * startRepl' <debug>; (* => activates top-level *)
 *)

(*
 * Lisp expressions can be compiled by executing
 * the following at the lisp top-level:
 * :c <exp>  ; <exp> means any lisp expression
 *)
