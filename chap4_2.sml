(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml, chap4_1.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 4  Metalinguistic Abstraction *)
(* 4.2  Variations on a Scheme -- Lazy Evaluation *)
(* 4.2.1  Normal Order and Applicative Order *)

(* 4.2.2  An Interpreter with Lazy Evaluation *)

functor LispEvaluatorFn'' (structure Lisp: LISP
                           and Syntax: LISP_SYNTAX
                           sharing type Syntax.obj = Lisp.obj
                           val log : string * Lisp.obj list -> unit)
        : LISP_EVALUATOR =
struct
  type obj = Lisp.obj

  fun eval' exp env =
      if Syntax.isSelfEvaluating exp then
        exp
      else if Syntax.isVariable exp then
        Lisp.lookupEnv env exp
      else if Syntax.isQuoted exp then
        Syntax.textOfQuotation exp
      else if Syntax.isAssignment exp then
        evalAssignment exp env
      else if Syntax.isDefinition exp then
        evalDefinition exp env
      else if Syntax.isIf exp then
        evalIf exp env
      else if Syntax.isLambda exp then
        evalLambda exp env
      else if Syntax.isBegin exp then
        evalSequence (Syntax.beginActions exp) env
      else if Syntax.isDerived exp then
        eval' (Syntax.expandDerived exp) env
      else if Syntax.isApplication exp then
        apply' (actualValue (Syntax.operator exp) env)
               (Syntax.operands exp)
               env
      else
        raise Lisp.Error ("Unknown expression type -- eval: ~S",
                          [exp])

  and evalAssignment exp env =
      (Lisp.setEnv env (Syntax.assignmentVariable exp,
                        eval' (Syntax.assignmentValue exp) env);
       Syntax.assignmentVariable exp)

  and evalDefinition exp env =
      (Lisp.defineEnv env (Syntax.definitionVariable exp,
                           eval' (Syntax.definitionValue exp) env);
       Syntax.definitionVariable exp)

  and evalIf exp env =
      if Lisp.isTrue (actualValue (Syntax.ifPredicate exp) env) then
        eval' (Syntax.ifConsequent exp) env
      else
        eval' (Syntax.ifAlternative exp) env

  and evalLambda exp env =
      Lisp.expr (Syntax.lambdaParameters exp,
                 Syntax.lambdaBody exp,
                 env)

  and evalSequence exp env =
      if Lisp.isNull exp then
        raise Lisp.Error ("Empty sequence -- evalSequence",
                          nil)
      else if Lisp.isCons exp then
        let
          val car = Lisp.car exp
          val cdr = Lisp.cdr exp
        in
          if Lisp.isNull cdr then
            eval' car env
          else
            (eval' car env; evalSequence cdr env)
        end
      else
        raise Lisp.Error ("Improper sequence -- evalSequence: ~S",
                          [exp])

  and apply' proc args env =
      if Lisp.isSubr proc then
        Lisp.applySubr proc (listOfArgValues args env)
      else if Lisp.isExpr proc then
        let
          val body = Lisp.exprBody proc
          val lexEnv = Lisp.exprEnv proc
          val params = Lisp.toList (Lisp.exprParams proc)
          val delayedArgs = listOfDelayedArgs args env
        in
          evalSequence body (Lisp.extendEnv
                                 lexEnv (params, delayedArgs))
        end
      else
        raise Lisp.Error ("Not a procedure -- apply': ~S",
                          [proc])

  and listOfArgValues exps env =
      if Lisp.isNull exps then
        nil
      else if Lisp.isCons exps then
        let
          val car = Lisp.car exps
          val cdr = Lisp.cdr exps
          val v = actualValue car env
          val vs = listOfArgValues cdr env
        in
          v :: vs
        end
      else
        raise Lisp.Error ("Improper sequence -- listOfArgValues: ~S",
                          [exps])

  and listOfDelayedArgs exps env =
      if Lisp.isNull exps then
        nil
      else if Lisp.isCons exps then
        let
          val car = Lisp.car exps
          val cdr = Lisp.cdr exps
          val v = delayIt car env
          val vs = listOfDelayedArgs cdr env
        in
          v :: vs
        end
      else
        raise Lisp.Error ("Improper sequence -- listOfDelayedArgs: ~S",
                          [exps])

  and actualValue exp env =
      (log ("actualValue called: ~S", [exp]);
       forceIt (eval' exp env))
(*
  and forceIt obj =
      (log ("forceIt called: ~S", [obj]);
       if Lisp.isThunk obj then
         actualValue (Lisp.thunkExp obj) (Lisp.thunkEnv obj)
       else
         obj)
 *)
  and forceIt obj =
      (log ("forceIt called: ~S", [obj]);
       if Lisp.isThunk obj then
         if Lisp.isEvaluated obj then
           Lisp.thunkValue obj
         else
           let
             val ret = actualValue (Lisp.thunkExp obj)
                                   (Lisp.thunkEnv obj)
           in
             Lisp.setThunkValue obj ret;
             ret
           end
       else
         obj)

  and delayIt obj env =
      (log ("delayIt called: ~S", [obj]);
       Lisp.thunk (obj, env))

  (* declared in LISP_EVALUATOR signature *)
  fun eval exp env =
      actualValue exp env

  (* declared in LISP_EVALUATOR signature *)
  fun apply proc args =
      if Lisp.isSubr proc then
        Lisp.applySubr proc args
      else if Lisp.isExpr proc then
        let
          val body = Lisp.exprBody proc
          val env = Lisp.exprEnv proc
          val params = Lisp.toList (Lisp.exprParams proc)
        in
          evalSequence body (Lisp.extendEnv env (params, args))
        end
      else
        raise Lisp.Error ("Not a procedure -- apply: ~S",
                          [proc])
end;

local
  structure E = Env
  structure L = LispFn (structure Env = E)
  structure LS = LispSyntaxFn (structure Lisp = L)
  structure LR = LispReaderFn (structure Lisp = L and Syntax = LS)
  structure LP = LispPrinterFn (structure Lisp = L and Syntax = LS)
  fun log (_, _) = () (* null-logger *)
  (*
  fun log (ctrlstr, args) =
      (LP.format (L.stdErr, ctrlstr, args);
       LP.terpri L.stdErr)
   *)
  structure LE = LispEvaluatorFn'' (structure Lisp = L and Syntax = LS
                                    val log = log)
in
structure LI'' = LispInterpreterFn (structure Lisp = L
                                    and Syntax = LS
                                    and Reader = LR
                                    and Printer = LP
                                    and Evaluator = LE)
end;

(*
 * LI''.go (); (* => activates top-level *)
 * LI''.test (); (* => executes predefined unit tests *)
 *)

fun lazyEvalTest () =
    let
      val ut = LI''.ut ()
    in
      ut ("(define (try a b) (if (= a 0) 1 b))", "'try");
      ut ("(try 0 (/ 1 0))", "1");
      ut ("(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))", "'fact");
      ut ("(fact 5)", "120");
      print "done\n"
    end
    handle e => LI''.onError (e, fn () => ())
;

(*
 * lazyEvalTest (); (* => executes unit tests for lazy evaluation *)
 *)

(* 4.2.3  Streams as Lazy Lists *)

fun streamTest () =
    let
      val ut = LI''.ut ()
    in
      ut ("(define (cons x y) (lambda (m) (m x y)))", "'cons");
      ut ("(define (car z) (z (lambda (p q) p)))", "'car");
      ut ("(define (cdr z) (z (lambda (p q) q)))", "'cdr");
      ut ("(define (list-ref items n)"^
          "  (if (= n 0)"^
          "      (car items)"^
          "      (list-ref (cdr items) (- n 1))))",
          "'list-ref");
      ut ("(define (map proc items)"^
          "  (if (null? items)"^
          "      '()"^
          "      (cons (proc (car items))"^
          "            (map proc (cdr items)))))",
          "'map");
      ut ("(define (scale-list items factor)"^
          "  (map (lambda (x) (* x factor))"^
          "       items))",
          "'scale-list");
      ut ("(define (add-lists list1 list2)"^
          "  (cond ((null? list1) list2)"^
          "        ((null? list2) list1)"^
          "        (else (cons (+ (car list1) (car list2))"^
          "                    (add-lists (cdr list1) (cdr list2))))))",
          "'add-lists");
      ut ("(define ones (cons 1 ones))",
          "'ones");
      ut ("(define integers (cons 1 (add-lists ones integers)))",
          "'integers");
      ut ("(list-ref integers 17)", "18");
      ut ("(define (integral integrand initial-value dt)"^
          "  (define int"^
          "    (cons initial-value"^
          "          (add-lists (scale-list integrand dt)"^
          "                    int)))"^
          "  int)",
          "'integral");
      ut ("(define (solve f y0 dt)"^
          "  (define y (integral dy y0 dt))"^
          "  (define dy (map f y))"^
          "  y)",
          "'solve");
      ut ("(begin"^
          "  (format \"solve[1000]=~S, expected=2.716924~%\""^
          "          (list-ref (solve (lambda (x) x) 1 0.001) 1000))"^
          "  'ok)",
          "'ok");
      print "done\n"
    end
    handle e => LI''.onError (e, fn () => ())
;

(*
 * streamTest (); (* => executes unit tests for stream manipulation *)
 *)

