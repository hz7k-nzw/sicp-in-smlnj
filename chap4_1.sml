(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 4  Metalinguistic Abstraction *)
(* 4.1  The Metacircular Evaluator *)

signature LISP =
sig
  type obj (* type for lisp object *)

  exception Error of string * obj list

  (* constants *)
  val undef : obj
  val eof : obj
  val null : obj
  val t : obj
  val f : obj
  val stdIn : obj
  val stdOut : obj
  val stdErr : obj

  (* constructors *)
  val cons : obj * obj -> obj
  val sym : string -> obj
  val bool : bool -> obj
  val int : int -> obj
  val str : string -> obj
  val subr0 : string * (unit -> obj) -> obj
  val subr1 : string * (obj -> obj) -> obj
  val subr2 : string * (obj * obj -> obj) -> obj
  val subr0R : string * (obj list -> obj) -> obj
  val subr1R : string * (obj * obj list -> obj) -> obj
  val subr2R : string * (obj * obj * obj list -> obj) -> obj
  val expr : obj * obj * obj -> obj
  val inputStream : TextIO.instream -> obj
  val outputSream : TextIO.outstream -> obj
  val thunk : obj * obj -> obj (* used in chap4_2.sml *)

  (* predicates for equality tests *)
  val eq : obj * obj -> bool
  val equal : obj * obj -> bool

  (* obj <-> (sml) list *)
  val fromList : obj list -> obj
  val toList : obj -> obj list

  (* predicates for boolean tests *)
  val isTrue : obj -> bool
  val isFalse : obj -> bool

  (* predicates for data type tests *)
  val isUndef : obj -> bool
  val isEof : obj -> bool
  val isNull : obj -> bool
  val isBool : obj -> bool
  val isCons : obj -> bool
  val isSym : obj -> bool
  val isInt : obj -> bool
  val isStr : obj -> bool
  val isSubr : obj -> bool
  val isExpr : obj -> bool
  val isInputStream : obj -> bool
  val isOutputStream : obj -> bool
  val isEnv : obj -> bool
  val isThunk : obj -> bool (* used in chap4_2.sml *)

  (* for cons *)
  val car : obj -> obj
  val cdr : obj -> obj
  val caar : obj -> obj
  val cadr : obj -> obj
  val cdar : obj -> obj
  val cddr : obj -> obj
  val caaar : obj -> obj
  val caadr : obj -> obj
  val cadar : obj -> obj
  val cdaar : obj -> obj
  val caddr : obj -> obj
  val cdadr : obj -> obj
  val cddar : obj -> obj
  val cdddr : obj -> obj
  val setCar : obj -> obj -> obj
  val setCdr : obj -> obj -> obj

  (* for symbol *)
  val pname : obj -> string

  (* for bool *)
  val toBool : obj -> bool

  (* for int *)
  val toInt : obj -> int

  (* for string *)
  val toString : obj -> string

  (* for subr (primitive procedure) *)
  val subrName : obj -> string
  val applySubr : obj -> obj list -> obj

  (* for expr (compound procedure) *)
  val exprParams : obj -> obj
  val exprBody : obj -> obj
  val exprEnv : obj -> obj

  (* for input/output stream *)
  val toInstream : obj -> TextIO.instream
  val toOutstream : obj -> TextIO.outstream

  (* for env *)
  val newEnv : unit -> obj
  val lookupEnv : obj -> obj -> obj
  val extendEnv : obj -> (obj list * obj list) -> obj
  val defineEnv : obj -> (obj * obj) -> obj
  val setEnv : obj -> (obj * obj) -> obj

  (* for thunk: used in chap4_2.sml *)
  val thunkExp : obj -> obj
  val thunkEnv : obj -> obj
  val thunkValue : obj -> obj
  val setThunkValue : obj -> obj -> unit
  val isEvaluated : obj -> bool
end;

signature LISP_SYNTAX =
sig
  type obj

  (* predefined symbols *)
  val BEGIN : obj
  val DEFINE : obj
  val FALSE : obj
  val IF : obj
  val LAMBDA : obj
  val QUOTE : obj
  val SET : obj
  val TRUE : obj

  (* for self-evaluating-values *)
  val isSelfEvaluating : obj -> bool

  (* for variables *)
  val isVariable : obj -> bool

  (* for quotations *)
  val isQuoted : obj -> bool
  val textOfQuotation : obj -> obj
  val makeQuote : obj -> obj

  (* for lambda-expressions *)
  val isLambda : obj -> bool
  val lambdaParameters : obj -> obj
  val lambdaBody : obj -> obj
  val makeLambda : obj * obj -> obj

  (* for assignments *)
  val isAssignment : obj -> bool
  val assignmentVariable : obj -> obj
  val assignmentValue : obj -> obj
  val makeAssign : obj * obj -> obj

  (* for definitions *)
  val isDefinition : obj -> bool
  val definitionVariable : obj -> obj
  val definitionValue : obj -> obj
  val makeDefinition : obj * obj -> obj

  (* for if-expressions *)
  val isIf : obj -> bool
  val ifPredicate : obj -> obj
  val ifConsequent : obj -> obj
  val ifAlternative : obj -> obj
  val makeIf : obj * obj * obj -> obj

  (* for begin-expressions *)
  val isBegin : obj -> bool
  val beginActions : obj -> obj
  val makeBegin : obj -> obj

  (* for function-applications *)
  val isApplication : obj -> bool
  val operator : obj -> obj
  val operands : obj -> obj
  val makeApplication : obj * obj -> obj

  (* for derived-expressions *)
  val isDerived : obj -> bool
  val expandDerived : obj -> obj
end;

signature LISP_EVALUATOR =
sig
  type obj
  val eval : obj -> obj -> obj
  val apply : obj -> obj list -> obj
end;

signature LISP_READER =
sig
  type obj
  val read : obj -> obj
end;

signature LISP_PRINTER =
sig
  type obj
  val print : obj * obj -> unit
  val printString : obj * string -> unit
  val terpri : obj -> unit
  val flush : obj -> unit
  val format : obj * string * obj list -> unit
end;

signature ENV =
sig
  type ('a, 'b) t
  val make   : ('a * 'a -> bool) -> ('a, 'b) t
  val lookup : ('a, 'b) t -> 'a -> 'b
  val define : ('a, 'b) t -> 'a * 'b -> unit
  val set    : ('a, 'b) t -> 'a * 'b -> unit
  val extend : ('a, 'b) t -> 'a list * 'b list -> ('a, 'b) t
end;

(* 4.1.1  The Core of the Evaluator *)

functor LispEvaluatorFn (structure Lisp: LISP
                         and Syntax: LISP_SYNTAX
                         sharing type Syntax.obj = Lisp.obj)
        : LISP_EVALUATOR =
struct
  type obj = Lisp.obj

  fun eval exp env =
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
        eval (Syntax.expandDerived exp) env
      else if Syntax.isApplication exp then
        apply (eval (Syntax.operator exp) env)
              (listOfValues (Syntax.operands exp) env)
      else
        raise Lisp.Error ("Unknown expression type -- eval: ~S",
                          [exp])

  and evalAssignment exp env =
      (Lisp.setEnv env (Syntax.assignmentVariable exp,
                        eval (Syntax.assignmentValue exp) env);
       Syntax.assignmentVariable exp)

  and evalDefinition exp env =
      (Lisp.defineEnv env (Syntax.definitionVariable exp,
                           eval (Syntax.definitionValue exp) env);
       Syntax.definitionVariable exp)

  and evalIf exp env =
      if Lisp.isTrue (eval (Syntax.ifPredicate exp) env) then
        eval (Syntax.ifConsequent exp) env
      else
        eval (Syntax.ifAlternative exp) env

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
            eval car env
          else
            (eval car env; evalSequence cdr env)
        end
      else
        raise Lisp.Error ("Improper sequence -- evalSequence: ~S",
                          [exp])

  and apply proc args =
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

  and listOfValues exps env =
      if Lisp.isNull exps then
        nil
      else if Lisp.isCons exps then
        let
          val car = Lisp.car exps
          val cdr = Lisp.cdr exps
          (* evaluates operands from left to right *)
          val v = eval car env
          val vs = listOfValues cdr env
        in
          v :: vs
        end
      else
        raise Lisp.Error ("Improper sequence -- listOfValues: ~S",
                          [exps])
end;

functor LispReaderFn (structure Lisp: LISP
                      and Syntax: LISP_SYNTAX
                      sharing type Syntax.obj = Lisp.obj)
        : LISP_READER =
struct
  type obj = Lisp.obj

  datatype token = Literal of string
                 | LParen
                 | RParen
                 | Dot
                 | Sharp
                 | Quote
                 | DoubleQuote
                 | Eof
                 | None (* token for initial state *)

  type tokenstate = token ref * (* current token *)
                    bool ref * (* unread flag *)
                    TextIO.instream (* input stream *)

  fun readError (msg) = raise Lisp.Error (msg, nil)

  fun read is =
      let
        val istream = Lisp.toInstream is
      in
        doRead istream
      end

  and doRead istream =
      let
        val ts = (ref None, ref false, istream)
      in
        readToken ts;
        case getToken ts of
          Eof => Lisp.eof
        | _ => (unreadToken ts; parseObj ts)
      end

  and parseObj ts =
      (readToken ts;
       case getToken ts of
         Literal s => parseLiteral s
       | LParen => parseCons ts
       | RParen => readError ("Unexpected right paren")
       | Dot => readError ("Unexpected dot")
       | Sharp => parseSharp ts
       | Quote => parseQuotation ts
       | DoubleQuote => parseString ts
       | Eof => readError ("Unexpected eof")
       | None => readError ("Unexpected token: None"))

  and parseLiteral s =
      let
        val ss = Substring.full s
        val radix = StringCvt.DEC
      in
        case Int.scan radix Substring.getc ss of
          SOME (i, ss') => if Substring.isEmpty ss' then
                             Lisp.int i
                           else
                             Lisp.sym s
        | NONE => Lisp.sym s
      end

  and parseCons ts =
      (readToken ts;
       case getToken ts of
         RParen => Lisp.null
       | _ =>
         (unreadToken ts;
          let
            val car = parseObj ts
          in
            readToken ts;
            case getToken ts of
              RParen => Lisp.cons (car, Lisp.null)
            | Dot =>
              let
                val cdr = parseObj ts
              in
                readToken ts;
                case getToken ts of
                  RParen => Lisp.cons (car, cdr)
                | _ => readError ("Right paren expected")
              end
            | _ =>
              let
                val cdr = (unreadToken ts; parseCons ts)
              in
                Lisp.cons (car, cdr)
              end
          end))

  and parseSharp ts =
      readError ("Unreadable string representation")

  and parseQuotation ts =
      let
        val exp = parseObj ts
      in
        Syntax.makeQuote exp
      end

  and parseString (_, _, istream) =
      let
        fun isEof () =
            TextIO.endOfStream istream
        fun isEos () =
            case TextIO.lookahead istream of
              SOME c => if c = #"\"" then
                          (TextIO.input1 istream; true)
                        else false
            | NONE => false
        fun scan () =
            case TextIO.scanStream Char.scan istream of
              NONE => readError ("Unexpected input")
            | SOME c => c
        fun iter s =
            if isEof () then
              (U.log "c=EOF";
               readError ("Unexpected eof"))
            else if isEos () then
              (U.log "c=\\\"(EOS)";
               Lisp.str s)
            else
              let
                val c = scan ()
              in
                (U.log ("c=" ^ Char.toString c);
                 iter (s ^ String.str c))
              end
      in
        iter ""
      end

  and readToken (tokenref, unreadref, istream) =
      if !unreadref then
        unreadref := false (* true -> false *)
      else
        tokenref := (tokenize istream) (* update token *)

  and getToken (tokenref, _, _) = !tokenref

  and unreadToken (_, unreadref, _) =
      if !unreadref then
        readError ("Repeatedly called")
      else
        unreadref := true (* false -> true *)

  and isUnread (_, unreadref, _) = !unreadref

  and tokenize istream =
      let
        fun isEof () =
            TextIO.endOfStream istream
        fun peek () =
            valOf (TextIO.lookahead istream)
        fun read () =
            valOf (TextIO.input1 istream)
        fun skipSpaces () =
            case TextIO.lookahead istream of
              SOME c => if Char.isSpace c then
                          (TextIO.input1 istream; skipSpaces ())
                        else ()
            | _ => ()
        fun iter "" =
            (skipSpaces ();
             if isEof () then Eof
             else
               let
                 val c = read ()
               in
                 if Char.isCntrl c then
                   readError ("Unexpected char: " ^ Char.toString c)
                 else
                   case c of
                     #"(" => LParen
                   | #")" => RParen
                   | #"." => Dot
                   | #"#" => Sharp
                   | #"'" => Quote
                   | #"\"" => DoubleQuote
                   | _ => iter (String.str c)
               end)
          | iter s =
            if isEof () then Literal s
            else
              let
                val c = peek ()
              in
                if Char.isSpace c then
                  Literal s
                else if Char.isCntrl c then
                  readError ("Unexpected char: " ^ Char.toString c)
                else
                  case c of
                    #"(" => Literal s
                  | #")" => Literal s
                  | #"'" => Literal s
                  | #"\"" => Literal s
                  | _ => (read(); iter (s ^ String.str c))
              end
      in
        iter ""
      end
end;

functor LispPrinterFn (structure Lisp: LISP
                      and Syntax: LISP_SYNTAX
                      sharing type Syntax.obj = Lisp.obj)
        : LISP_PRINTER =
struct
  type obj = Lisp.obj

  fun print (os, obj) =
      let
        val ostream = Lisp.toOutstream os
      in
        doPrint (ostream, obj);
        TextIO.flushOut ostream
      end

  and doPrint (ostream, obj) =
      let
        fun p s =
            TextIO.output (ostream, s)
        and p1 obj =
            if Lisp.isNull obj then
              p "()"
            else if Lisp.isCons obj then
              (p "(";
               p1 (Lisp.car obj);
               p2 (Lisp.cdr obj);
               p ")")
            else if Lisp.isSym obj then
              pSym obj
            else if Lisp.isBool obj then
              pBool obj
            else if Lisp.isInt obj then
              pInt obj
            else if Lisp.isStr obj then
              pStr obj
            else if Lisp.isSubr obj then
              pSubr obj
            else if Lisp.isExpr obj then
              pExpr obj
            else if Lisp.isInputStream obj then
              pInputStream obj
            else if Lisp.isOutputStream obj then
              pOutputStream obj
            else if Lisp.isEnv obj then
              pEnv obj
            else if Lisp.isUndef obj then
              pUndef obj
            else if Lisp.isThunk obj then
              pThunk obj
            else
              pUnknown obj
        and p2 obj =
            if Lisp.isNull obj then
              ()
            else if Lisp.isCons obj then
              (p " ";
               p1 (Lisp.car obj);
               p2 (Lisp.cdr obj))
            else if Lisp.isSym obj then
              (p " . "; pSym obj)
            else if Lisp.isBool obj then
              (p " . "; pBool obj)
            else if Lisp.isInt obj then
              (p " . "; pInt obj)
            else if Lisp.isStr obj then
              (p " . "; pStr obj)
            else if Lisp.isSubr obj then
              (p " . "; pSubr obj)
            else if Lisp.isExpr obj then
              (p " . "; pExpr obj)
            else if Lisp.isInputStream obj then
              (p " . "; pInputStream obj)
            else if Lisp.isOutputStream obj then
              (p " . "; pOutputStream obj)
            else if Lisp.isEnv obj then
              (p " . "; pEnv obj)
            else if Lisp.isUndef obj then
              (p " . "; pUndef obj)
            else if Lisp.isThunk obj then
              (p " . "; pThunk obj)
            else
              (p " . "; pUnknown obj)
        and pSym obj =
            p (Lisp.pname obj)
        and pBool obj =
            if Lisp.toBool obj then p "#T" else p "#F"
        and pInt obj =
            p (Int.toString (Lisp.toInt obj))
        and pStr obj =
            (p "\"";
             p (String.toString (Lisp.toString obj));
             p "\"")
        and pSubr obj =
            (p "#<Subr: ";
             p (Lisp.subrName obj);
             p ">")
        and pExpr obj =
            let
              val params = Lisp.exprParams obj
              val body = Lisp.exprBody obj
            in
              (p "#<Expr: ";
               p1 (Syntax.makeLambda (params, body));
               p ">")
            end
        and pInputStream obj =
            p "#<InputStream>"
        and pOutputStream obj =
            p "#<OutputStream>"
        and pEnv obj =
            p "#<Env>"
        and pUndef obj =
            p "#<Undef>"
        and pThunk obj =
            let
              val evaluated = Lisp.isEvaluated obj
              val content = if evaluated then Lisp.thunkValue obj
                            else Lisp.thunkExp obj
            in
              (p "#<Thunk";
               if evaluated then p "(evaluated): "
               else p "(not evaluated): ";
               p1 content;
               p ">")
            end
        and pUnknown obj =
            p "#<Unknown>"
      in
        p1 obj
      end

  fun printString (os, s) =
      let
        val ostream = Lisp.toOutstream os
      in
        TextIO.output (ostream, s);
        TextIO.flushOut ostream
      end

  fun terpri os =
      let
        val ostream = Lisp.toOutstream os
      in
        TextIO.output (ostream, "\n");
        TextIO.flushOut ostream
      end

  fun flush os =
      let
        val ostream = Lisp.toOutstream os
      in
        TextIO.flushOut ostream
      end

  fun format (os, ctrlstr, args) =
      let
        datatype format = Literal of string
                        | Sexp
                        | Newline

        val ss = Substring.full ctrlstr

        fun parse ss =
            let
              val (s1, ss) = StringCvt.splitl (fn c => c <> #"~")
                                              Substring.getc ss
              val prefix = if s1 = "" then nil else [Literal s1]
            in
              if Substring.isEmpty ss then
                prefix
              else
                let
                  val (f,ss) = parseOne ss
                  val fs = parse ss
                in
                  prefix @ (f::fs)
                end
            end

        and parseOne ss =
            let
              val ss = Substring.triml 1 ss
            in
              if Substring.isPrefix "~" ss then
                (Literal "~", Substring.triml 1 ss)
              else if Substring.isPrefix "S" ss then
                (Sexp, Substring.triml 1 ss)
              else if Substring.isPrefix "%" ss then
                (Newline, Substring.triml 1 ss)
              else
                raise Lisp.Error ("Unexpected ctrlstr: " ^
                                  Substring.string ss, nil)
            end

        fun traverse (nil, _) = ()
          | traverse ((Literal s)::fs, args) =
            (printString (os, s); traverse (fs, args))
          | traverse (Sexp::fs, arg::args) =
            (print (os, arg); traverse (fs, args))
          | traverse (Sexp::_, nil) =
            raise Lisp.Error ("Not enough arguments: " ^
                              Substring.string ss, nil)
          | traverse (Newline::fs, args) =
            (terpri os; traverse (fs, args))
      in
        traverse (parse ss, args)
      end
end;

(* 4.1.2  Representing Expressions *)

functor LispSyntaxFn (structure Lisp: LISP) : LISP_SYNTAX =
struct
  type obj = Lisp.obj

  val BEGIN = Lisp.sym "begin"
  val DEFINE = Lisp.sym "define"
  val FALSE = Lisp.sym "false"
  val IF = Lisp.sym "if"
  val LAMBDA = Lisp.sym "lambda"
  val QUOTE = Lisp.sym "quote"
  val SET = Lisp.sym "set!"
  val TRUE = Lisp.sym "true"

  val AND = Lisp.sym "and"
  val ARROW = Lisp.sym "=>"
  val COND = Lisp.sym "cond"
  val ELSE = Lisp.sym "else"
  val OR = Lisp.sym "or"
  val LET = Lisp.sym "let"
  val LET2 = Lisp.sym "let*"
  val LETREC = Lisp.sym "letrec"

  val useScanOutDefines = false

  fun isSelfEvaluating exp =
      (*Lisp.isNull exp orelse*)
      (*Lisp.isBool exp orelse*)
      Lisp.isInt exp orelse
      Lisp.isStr exp

  val isVariable = Lisp.isSym

  fun isTaggedList tag exp =
      Lisp.isCons exp andalso
      Lisp.eq (Lisp.car exp, tag)

  fun isQuoted exp = isTaggedList QUOTE exp
  and textOfQuotation exp = Lisp.cadr exp
  and makeQuote exp = Lisp.fromList [QUOTE, exp]

  and makeTrue () = TRUE (* makeQuote Lisp.t *)
  and makeFalse () = FALSE (* makeQuote Lisp.f *)

  and isLambda exp = isTaggedList LAMBDA exp
  and lambdaParameters exp = Lisp.cadr exp
  and lambdaBody exp = Lisp.cddr exp
  and makeLambda (params, body) =
      let
        val body' = if useScanOutDefines then
                      scanOutDefines body
                    else body
      in
        Lisp.cons (LAMBDA, Lisp.cons (params, body'))
      end
  (*
   * Exercise 4.16
   * I placed scanOutDefines in the body of makeLambda because
   * both expr (make-procedure) and exprBody (procedure-body)
   * are included in LispFn functor, from which the functions
   * declared in LispSyntaxFn functor cannot be called.
   *)
  and scanOutDefines body =
      (*
       * (lambda <vars>
       *   (define u <e1>)
       *   (define v <e2>)
       *   <e3>)
       * -> (lambda <vars>
       *      (let ((u '*unassigned* )
       *            (v '*unassigned* ))
       *        (set! u <e1>)
       *        (set! v <e2>)
       *        <e3>))
       *)
      let
        val (defines, nonDefines) : obj list * obj list
          = List.partition isDefinition (Lisp.toList body)
      in
        if null defines then
          body
        else
          let
            fun toInit exp =
                let
                  val dvar = definitionVariable exp
                in
                  Lisp.fromList [dvar, makeQuote Lisp.undef]
                end
            fun toAssign exp =
                let
                  val dvar = definitionVariable exp
                  val dval = definitionValue exp
                in
                  makeAssign (dvar, dval)
                end
            val letParams : obj =
                Lisp.fromList (List.map toInit defines)
            val letBody : obj list =
                (List.map toAssign defines) @ nonDefines
            val letForm : obj =
                Lisp.fromList (LET :: letParams :: letBody)
          in
            Lisp.fromList [letForm]
          end
      end

  and isAssignment exp = isTaggedList SET exp
  and assignmentVariable exp = Lisp.cadr exp
  and assignmentValue exp = Lisp.caddr exp
  and makeAssign (variable, value) =
      Lisp.fromList [SET, variable, value]

  and isDefinition exp = isTaggedList DEFINE exp
  and definitionVariable exp =
      let
        val cadr = Lisp.cadr exp
      in
        if Lisp.isSym cadr then cadr
        else Lisp.car cadr
      end
  and definitionValue exp =
      let
        val cadr = Lisp.cadr exp
      in
        if Lisp.isSym cadr then
          Lisp.caddr exp
        else
          makeLambda (Lisp.cdr cadr, (* formal params *)
                      Lisp.cddr exp) (* body *)
      end
  and makeDefinition (variable, value) =
      Lisp.fromList [DEFINE, variable, value]

  and isIf exp = isTaggedList IF exp
  and ifPredicate exp = Lisp.cadr exp
  and ifConsequent exp = Lisp.caddr exp
  and ifAlternative exp =
      let
        val cdddr = Lisp.cdddr exp
      in
        if Lisp.isNull cdddr then makeFalse ()
        else Lisp.car cdddr
      end
  and makeIf (pred, con, alt) =
      Lisp.fromList [IF, pred, con, alt]

  and isBegin exp = isTaggedList BEGIN exp
  and beginActions exp = Lisp.cdr exp
  and makeBegin actions = Lisp.cons (BEGIN, actions)

  and isApplication exp = Lisp.isCons exp
  and operator exp = Lisp.car exp
  and operands exp = Lisp.cdr exp
  and makeApplication (operator, operands) =
      Lisp.cons (operator, operands)

  (*
   * derived expressions
   *)

  fun seqToExp seq =
      if Lisp.isNull seq then seq
      else if (Lisp.isNull o Lisp.cdr) seq then
        Lisp.car seq
      else makeBegin seq

  fun map' f seq =
      if Lisp.isNull seq then Lisp.null
      else
        let
          val h = f (Lisp.car seq)
          val t = map' f (Lisp.cdr seq)
        in
          Lisp.cons (h, t)
        end

  fun append' (x, y) =
      if Lisp.isNull x then y
      else Lisp.cons (Lisp.car x, append' (Lisp.cdr x, y))

  val isCond = isTaggedList COND
  val expandCond =
      let
        val condClauses = Lisp.cdr
        val condPredicate = Lisp.car
        val condActions = Lisp.cdr
        val condArrowProc = Lisp.cadr
        fun isCondElseClause clause =
            Lisp.eq (condPredicate clause, ELSE)
        fun isCondArrowClause clause =
            let
              val actions = condActions clause
            in
              if not (Lisp.isNull actions) andalso
                 Lisp.eq (Lisp.car actions, ARROW) then
                if (Lisp.isNull o Lisp.cdr) actions then
                  raise Lisp.Error ("Too few actions -- expandCond: ~S",
                                    [actions])
                else if (not o Lisp.isNull o Lisp.cddr) actions then
                  raise Lisp.Error ("Too many actions -- expandCond: ~S",
                                    [actions])
                else
                  true
              else
                false
            end
        fun expandClauses clauses =
            (*
             * (cond) -> false
             * (cond (else . <actions>))
             * -> (begin ,@<actions>)
             * (cond (<test> => <proc>) <rest>)
             * -> ((lambda (v r1 r2) (if v ((r1) v) (r2))
             *     ,<test>
             *     (lambda () ,<proc>)
             *     (lambda () (cond ,@<rest>)))
             * (cond (<test> . <actions>) . <rest>)
             * -> (if ,<test> (begin ,@<actions>) (cond ,@<rest>))
             *)
            if Lisp.isNull clauses then
              makeFalse () (* no else clause *)
            else
              let
                val first = Lisp.car clauses
                val rest = Lisp.cdr clauses
              in
                if isCondElseClause first then
                  if Lisp.isNull rest then
                    (seqToExp o condActions) first
                  else
                    raise Lisp.Error ("ELSE clause isn't last" ^
                                      " -- expandCond: ~S", [clauses])
                else if isCondArrowClause first then
                  let
                    val test = condPredicate first
                    val actions = condActions first
                    val proc = condArrowProc actions
                    val thunk1 = makeLambda (Lisp.null,
                                            Lisp.fromList [proc])
                    val condForm = Lisp.cons (COND, rest)
                    val thunk2 = makeLambda (Lisp.null,
                                            Lisp.fromList [condForm])
                    val opParam1 = Lisp.sym "V"
                    val opParam2 = Lisp.sym "R1"
                    val opParam3 = Lisp.sym "R2"
                    val opParams = Lisp.fromList
                                       [opParam1, opParam2, opParam3]
                    val appForm1 = makeApplication (opParam2,
                                                    Lisp.null)
                    val appForm2 = makeApplication (opParam3,
                                                    Lisp.null)
                    val ifForm = makeIf (opParam1,
                                         makeApplication (appForm1,
                                                          Lisp.fromList
                                                              [opParam1]),
                                         appForm2)
                    val operator = makeLambda (opParams,
                                               Lisp.fromList [ifForm])
                    val operands = Lisp.fromList [test, thunk1, thunk2]
                  in
                    makeApplication (operator, operands)
                  end
                else
                  makeIf (condPredicate first,
                          (seqToExp o condActions) first,
                          (* expandClauses rest) *)
                          Lisp.cons (COND, rest))
              end
      in
        expandClauses o condClauses
      end

 (* Exercise 4.4 *)
  val isAnd = isTaggedList AND
  val expandAnd =
      let
        fun expandClauses clauses =
            (*
             * (and) -> true
             * (and <x>) -> <x>
             * (and <x> . <rest>)
             *  -> ((lambda (v r) (if v (r) false))
             *       ,<x>
             *       (lambda () (and ,@<rest>)))
             *)
            if Lisp.isNull clauses then
              makeTrue ()
            else
              let
                val first = Lisp.car clauses
                val rest = Lisp.cdr clauses
              in
                if Lisp.isNull rest then
                  first
                else
                  let
                    val andForm = Lisp.cons (AND, rest)
                    val thunk = makeLambda (Lisp.null,
                                            Lisp.fromList [andForm])
                    val opParam1 = Lisp.sym "V"
                    val opParam2 = Lisp.sym "R"
                    val opParams = Lisp.fromList [opParam1, opParam2]
                    val ifForm = makeIf (opParam1,
                                         makeApplication (opParam2,
                                                          Lisp.null),
                                         makeFalse ())
                    val operator = makeLambda (opParams,
                                               Lisp.fromList [ifForm])
                    val operands = Lisp.fromList [first, thunk]
                  in
                    makeApplication (operator, operands)
                  end
              end
      in
        expandClauses o Lisp.cdr
      end

 (* Exercise 4.4 *)
  val isOr = isTaggedList OR
  val expandOr =
      let
        fun expandClauses clauses =
            (*
             * (or) -> false
             * (or <x> . <rest>)
             *  -> ((lambda (v r) (if v v (r)))
             *      ,<x>
             *      (lambda () (or ,@<rest>)))
             *)
            if Lisp.isNull clauses then
              makeFalse ()
            else
              let
                val first = Lisp.car clauses
                val rest = Lisp.cdr clauses

                val orForm = Lisp.cons (OR, rest)
                val thunk = makeLambda (Lisp.null,
                                        Lisp.fromList [orForm])
                val opParam1 = Lisp.sym "V"
                val opParam2 = Lisp.sym "R"
                val opParams = Lisp.fromList [opParam1, opParam2]
                val ifForm = makeIf (opParam1,
                                     opParam1,
                                     makeApplication (opParam2,
                                                      Lisp.null))
                val operator = makeLambda (opParams,
                                           Lisp.fromList [ifForm])
                val operands = Lisp.fromList [first, thunk]
              in
                makeApplication (operator, operands)
              end
      in
        expandClauses o Lisp.cdr
      end

  (* Exercise 4.6 *)
  val isLet = isTaggedList LET
  val expandLet =
      let
        fun expandClauses clauses =
            let
              val car = Lisp.car clauses
            in
              if Lisp.isNull car orelse Lisp.isCons car then
                expandClausesForOrdinaryLet clauses
              else if Lisp.isSym car then
                expandClausesForNamedLet clauses
              else
                raise Lisp.Error ("Unexpected form -- expandLet: ~S",
                                  [car, clauses])
            end
        and expandClausesForOrdinaryLet clauses =
            (*
             * (let <params> . <body>)
             * -> ((lambda ,(MAP CAR <params>) ,@<body>)
             *     ,@(MAP CADR <params>))
             *)
            let
              val params = Lisp.car clauses
              val body = Lisp.cdr clauses

              val operator = makeLambda (map' Lisp.car params, body)
              val operands = map' Lisp.cadr params
            in
              makeApplication (operator, operands)
            end
        (* Exercise 4.8 *)
        and expandClausesForNamedLet clauses =
            (*
             * (let <name> <params> . <body>)
             * -> (let (,<name> true)
             *     (set! ,<name> (lambda ,(MAP CAR <params>) ,@<body>))
             *     (,<name> ,@(MAP CADR <params>))))
             *)
            let
              val name = Lisp.car clauses
              val params = Lisp.cadr clauses
              val body = Lisp.cddr clauses

              val letParam = Lisp.fromList [name, makeTrue ()]
              val letParams = Lisp.fromList [letParam]
              val operator = makeLambda (map' Lisp.car params, body)
              val operands = map' Lisp.cadr params
              val assignForm = makeAssign (name, operator)
              val appForm = makeApplication (name, operands)
            in
              Lisp.fromList [LET, letParams, assignForm, appForm]
            end
      in
        expandClauses o Lisp.cdr
      end

  (* Exercise 4.7 *)
  val isLet2 = isTaggedList LET2
  val expandLet2 =
      let
        fun expandClauses clauses =
            (*
             * (let* () . <body>) -> (let () ,@<body>)
             * (let* <params> . <body>)
             * -> (let ,(CAR <params>) (let* ,(CDR <params>) ,@<body>))
             *)
            let
              val params = Lisp.car clauses
              val body = Lisp.cdr clauses
            in
              if Lisp.isNull params then
                Lisp.cons (LET,
                           Lisp.cons (params, body))
              else
                let
                  val firstParam = Lisp.car params
                  val restParams = Lisp.cdr params
                  val letParams = Lisp.fromList [firstParam]
                  val let2Form = Lisp.cons (LET2,
                                            Lisp.cons (restParams, body))
                in
                  Lisp.fromList [LET, letParams, let2Form]
                end
            end
      in
        expandClauses o Lisp.cdr
      end

  (* Exercise 4.20 *)
  val isLetrec = isTaggedList LETREC
  val expandLetrec =
      let
        fun expandClauses clauses =
            (*
             * (letrec
             *   ((u <e1>)
             *    (v <e2>))
             *   <e3>)
             * -> (let
             *      ((u '*unassigned* )
             *       (v '*unassigned* ))
             *      (set! u <e1>)
             *      (set! v <e2>)
             *      <e3>)
             *)
            let
              val params = Lisp.car clauses
              val body = Lisp.cdr clauses

              fun toInit exp =
                  let
                    val var = Lisp.car exp
                  in
                    Lisp.fromList [var, makeQuote Lisp.undef]
                  end
              fun toAssign exp =
                  let
                    val var = Lisp.car exp
                    val value = Lisp.cadr exp
                  in
                    makeAssign (var, value)
                  end
              val letParams = map' toInit params
              val letBody = append' (map' toAssign params, body)
            in
              Lisp.cons (LET, Lisp.cons (letParams, letBody))
            end
      in
        expandClauses o Lisp.cdr
      end

  fun isDerived exp =
      isCond exp orelse
      isAnd exp orelse
      isOr exp orelse
      isLet exp orelse
      isLet2 exp orelse
      isLetrec exp

  fun expandDerived exp =
      if isCond exp then expandCond exp
      else if isAnd exp then expandAnd exp
      else if isOr exp then expandOr exp
      else if isLet exp then expandLet exp
      else if isLet2 exp then expandLet2 exp
      else if isLetrec exp then expandLetrec exp
      else raise Lisp.Error ("Unsupported derived expression: ~S", [exp])
end;

(* 4.1.3  Evaluator Data Structures *)

functor LispFn (structure Env: ENV) :> LISP =
struct
  datatype obj = Undef
               | Eof
               | Nil
               | Cons of obj ref * obj ref
               | Sym of string
               | Bool of bool
               | Int of int
               | Str of string
               | Subr of int * string * (obj list -> obj)
               | Expr of int * obj * obj * obj
               | InputStream of int * TextIO.instream
               | OutputStream of int * TextIO.outstream
               | Environment of int * (obj, obj) Env.t
               | Thunk of int * thunk ref

       and thunk = NotEvaluated of obj * obj
                 | Evaluated of obj

  exception Error of string * obj list

  val counter = ref 0 (* counter for obj-id *)

  fun inc () =
      let
        val count = !counter
      in
        counter := count + 1;
        count
      end

  (* constants *)
  val undef = Undef
  val eof = Eof
  val null = Nil
  val t = Bool true
  val f = Bool false
  val stdIn = InputStream (inc (), TextIO.stdIn)
  val stdOut = OutputStream (inc (), TextIO.stdOut)
  val stdErr = OutputStream (inc (), TextIO.stdErr)

  (* constants for type info *)
  val t_list = Sym "list"
  val t_sym = Sym "symbol"
  val t_bool = Sym "bool"
  val t_int = Sym "int"
  val t_str = Sym "string"
  val t_subr = Sym "subr"
  val t_expr = Sym "expr"
  val t_input_stream = Sym "input-stream"
  val t_output_stream = Sym "output-stream"
  val t_env = Sym "env"
  val t_thunk = Sym "thunk"

  (* error functions *)
  fun typeError (expected, obj) =
      raise Error ("Unexpected data specified: ~S (expected type: ~S)",
                   [obj, expected])
  fun emptyError () =
      raise Error ("Empty list specified", nil)

  fun argsError (name, args, expected) =
      let
        val actual = length args
        val msg = if actual < expected then
                    "Too few arguments given to " ^ name ^
                    " (expected: " ^ Int.toString expected ^ ")"
                  else if expected < actual then
                    "Too many arguments given to " ^ name ^
                    " (expected: " ^ Int.toString expected ^ ")"
                  else
                    "? -- " ^ name ^
                    " (expected: " ^ Int.toString expected ^ ")"
      in
        raise Error (msg, nil)
      end

  fun thunkError (expectedState, thunk) =
      let
        val msg = if expectedState then
                    "Unexpected thunk (expected: evaluated): ~S"
                  else
                    "Unexpected thunk (expected: not evaluated): ~S"
      in
        raise Error (msg, [thunk])
      end


  (* constructors *)
  fun cons (h, t) = Cons (ref h, ref t)
  and sym s = Sym s
  and bool b = Bool b
  and int i = Int i
  and str s = Str s
  and subr (name, p) = Subr (inc (), name, p) (* not exported *)
  and subr0 (name, proc) =
      subr (name, fn [] => proc ()
                   | args => argsError (name, args, 0))
  and subr1 (name, proc) =
      subr (name, fn [x1] => proc x1
                   | args => argsError (name, args, 1))
  and subr2 (name, proc) =
      subr (name, fn [x1,x2] => proc (x1,x2)
                   | args => argsError (name, args, 2))
  and subr0R (name, proc) =
      subr (name, fn xs => proc xs)
  and subr1R (name, proc) =
      subr (name, fn (x1::xs) => proc (x1,xs)
                   | args => argsError (name, args, 1))
  and subr2R (name, proc) =
      subr (name, fn (x1::x2::xs) => proc (x1,x2,xs)
                   | args => argsError (name, args, 2))
  and expr (params, body, env) = Expr (inc (), params, body, env)
  and inputStream is = InputStream (inc (), is)
  and outputSream os = OutputStream (inc (), os)
  and environment e = Environment (inc (), e) (* not exported *)
  and thunk (exp, env) = Thunk (inc (), ref (NotEvaluated (exp, env)))

  (* obj <-> list *)
  and fromList nil = Nil
    | fromList (h::t) = cons (h, fromList t)
  and toList (Cons (h, t)) = (!h)::(toList (!t))
    | toList Nil = nil
    | toList obj = typeError (t_list, obj)

  (* predicates for equality tests *)
  fun eq (Undef, Undef) = true
    | eq (Eof, Eof) = true
    | eq (Nil, Nil) = true
    | eq (Cons (h1, t1), Cons (h2, t2)) = h1 = h2 andalso t1 = t2
    | eq (Sym s1, Sym s2) = s1 = s2
    | eq (Bool b1, Bool b2) = b1 = b2
    | eq (Int i1, Int i2) = i1 = i2
    | eq (Str s1, Str s2) = s1 = s2
    | eq (Subr (id1,_,_), Subr (id2,_,_)) = id1 = id2
    | eq (Expr (id1,_,_,_), Expr (id2,_,_,_)) = id1 = id2
    | eq (InputStream (id1,_), InputStream (id2,_)) = id1 = id2
    | eq (OutputStream (id1,_), OutputStream (id2,_)) = id1 = id2
    | eq (Environment (id1,_), Environment (id2,_)) = id1 = id2
    | eq (Thunk (id1,_), Thunk (id2,_)) = id1 = id2
    | eq _ = false
  fun equal (Undef, Undef) = true
    | equal (Eof, Eof) = true
    | equal (Nil, Nil) = true
    | equal (Cons (h1, t1), Cons (h2, t2)) = equal (!h1, !h2) andalso
                                             equal (!t1, !t2)
    | equal (Sym s1, Sym s2) = s1 = s2
    | equal (Bool b1, Bool b2) = b1 = b2
    | equal (Int i1, Int i2) = i1 = i2
    | equal (Str s1, Str s2) = s1 = s2
    | equal (Subr (id1,_,_), Subr (id2,_,_)) = id1 = id2
    | equal (Expr (id1,_,_,_), Expr (id2,_,_,_)) = id1 = id2
    | equal (InputStream (id1,_), InputStream (id2,_)) = id1 = id2
    | equal (OutputStream (id1,_), OutputStream (id2,_)) = id1 = id2
    | equal (Environment (id1,_), Environment (id2,_)) = id1 = id2
    | equal (Thunk (id1,_), Thunk (id2,_)) = id1 = id2
    | equal _ = false

  (* predicates for boolean tests *)
  fun isTrue (Bool false) = false
    | isTrue _ = true
  fun isFalse (Bool false) = true
    | isFalse _ = false

  (* predicates for data type tests *)
  fun isUndef Undef = true
    | isUndef _ = false
  fun isEof Eof = true
    | isEof _ = false
  fun isNull Nil = true
    | isNull _ = false
  fun isBool (Bool _) = true
    | isBool _ = false
  fun isCons (Cons _) = true
    | isCons _ = false
  fun isSym (Sym _) = true
    | isSym _ = false
  fun isInt (Int _) = true
    | isInt _ = false
  fun isStr (Str _) = true
    | isStr _ = false
  fun isSubr (Subr _) = true
    | isSubr _ = false
  fun isExpr (Expr _) = true
    | isExpr _ = false
  fun isInputStream (InputStream _) = true
    | isInputStream _ = false
  fun isOutputStream (OutputStream _) = true
    | isOutputStream _ = false
  fun isEnv (Environment _) = true
    | isEnv _ = false
  fun isThunk (Thunk _) = true
    | isThunk _ = false

  (* for cons *)
  fun car (Cons (h,_)) = !h
    | car Nil = emptyError ()
    | car obj = typeError (t_list, obj)
  fun cdr (Cons (_,t)) = !t
    | cdr Nil = emptyError ()
    | cdr obj = typeError (t_list, obj)
  val caar = car o car
  val cadr = car o cdr
  val cdar = cdr o car
  val cddr = cdr o cdr
  val caaar = car o car o car
  val caadr = car o car o cdr
  val cadar = car o cdr o car
  val cdaar = cdr o car o car
  val caddr = car o cdr o cdr
  val cdadr = cdr o car o cdr
  val cddar = cdr o cdr o car
  val cdddr = cdr o cdr o cdr
  fun setCar (Cons (h,_)) newObj = (h := newObj; undef)
    | setCar Nil _ = emptyError ()
    | setCar obj _ = typeError (t_list, obj)
  fun setCdr (Cons (_,t)) newObj = (t := newObj; undef)
    | setCdr Nil _ = emptyError ()
    | setCdr obj _ = typeError (t_list, obj)

  (* for symbol *)
  fun pname (Sym s) = s
    | pname obj = typeError (t_sym, obj)

  (* for bool *)
  fun toBool (Bool b) = b
    | toBool obj = typeError (t_bool, obj)

  (* for int *)
  fun toInt (Int i) = i
    | toInt obj = typeError (t_int, obj)

  (* for string *)
  fun toString (Str s) = s
    | toString obj = typeError (t_str, obj)

  (* for subr (primitive procedure) *)
  fun subrName (Subr (_,name,_)) = name
    | subrName obj = typeError (t_subr, obj)
  fun applySubr (Subr (_,_,p)) vs = p vs
    | applySubr obj _ = typeError (t_subr, obj)

  (* for expr (compound procedure) *)
  fun exprParams (Expr (_,params,_,_)) = params
    | exprParams obj = typeError (t_expr, obj)
  fun exprBody (Expr (_,_,body,_)) = body
    | exprBody obj = typeError (t_expr, obj)
  fun exprEnv (Expr (_,_,_,env)) = env
    | exprEnv obj = typeError (t_expr, obj)

  (* for input/output stream *)
  fun toInstream (InputStream (_,is)) = is
    | toInstream obj = typeError (t_input_stream, obj)
  fun toOutstream (OutputStream (_,os)) = os
    | toOutstream obj = typeError (t_output_stream, obj)

  (* for env *)
  fun symeq (Sym s1, Sym s2) = s1 = s2
    | symeq _ = raise Fail "Variable must be symbol"
  fun newEnv () =
      environment (Env.make symeq)
      handle cause =>
             raise Error ("Env make failed (cause: " ^
                          exnMessage cause ^ ")", nil)
  fun lookupEnv' (Environment (_,env)) var =
      (Env.lookup env var
       handle cause =>
              raise Error ("Env lookup failed: ~S (cause: " ^
                           exnMessage cause ^ ")", [var]))
    | lookupEnv' obj _ = typeError (t_env, obj)
  fun lookupEnv e var =
      let
        val value = lookupEnv' e var
      in
        (* Exercise 4.16 *)
        if isUndef value then
          raise Error ("Env lookup failed: ~S (cause: " ^
                       "undefined variable)", [var])
        else value
      end
  fun extendEnv (Environment (_,env)) (vars, vals) =
      (environment (Env.extend env (vars, vals))
       handle cause =>
              raise Error ("Env extend failed: ~S (cause: " ^
                           exnMessage cause ^ ")", [fromList vars]))
    | extendEnv obj _ = typeError (t_env, obj)
  fun defineEnv (Environment (_,env)) (var, value) =
      ((Env.define env (var, value); undef)
       handle cause =>
              raise Error ("Env define failed: ~S, ~S (cause: " ^
                           exnMessage cause ^ ")", [var, value]))
    | defineEnv obj _ = typeError (t_env, obj)
  fun setEnv (Environment (_,env)) (var, value) =
      ((Env.set env (var, value); undef)
       handle cause =>
              raise Error ("Env set failed: ~S, ~S (cause: " ^
                           exnMessage cause ^ ")", [var, value]))
    | setEnv obj _ = typeError (t_env, obj)

  (* for thunk: used in chap4_2.sml *)
  fun thunkExp (obj as Thunk (_,tref)) =
      (case !tref of
         NotEvaluated (exp,_) => exp
       | Evaluated _ => thunkError (false, obj))
    | thunkExp obj = typeError (t_thunk, obj)
  fun thunkEnv (obj as Thunk (_,tref)) =
      (case !tref of
         NotEvaluated (_,env) => env
       | Evaluated _ => thunkError (false, obj))
    | thunkEnv obj = typeError (t_thunk, obj)
  fun thunkValue (obj as Thunk (_,tref)) =
      (case !tref of
         NotEvaluated _ => thunkError (true, obj)
       | Evaluated value => value)
    | thunkValue obj = typeError (t_thunk, obj)
  fun setThunkValue (obj as Thunk (_,tref)) value =
      (case !tref of
         NotEvaluated _ => tref := (Evaluated value)
       | Evaluated _ => thunkError (false, obj))
    | setThunkValue obj _ = typeError (t_thunk, obj)
  fun isEvaluated (Thunk (_,tref)) =
      (case !tref of
         NotEvaluated _ => false
       | Evaluated _ => true)
    | isEvaluated obj = typeError (t_thunk, obj)
end;

structure Env :> ENV =
struct
  type ('a, 'b) frame = ('a list * ('b ref) list) ref
  type ('a, 'b) t = (('a, 'b) frame list) * ('a * 'a -> bool)

  fun make eq = (U.log "Env: created"; (nil, eq))

  fun lookup (frames, eq) var =
      let
        fun loop nil = raise Fail "Unbound variable -- lookup"
          | loop (frame::frames) =
            let
              fun scan (nil, nil) = loop frames
                | scan (curvar::vars, curval::vals) =
                  if eq (curvar, var) then
                    !curval
                  else
                    scan (vars, vals)
                | scan _ = raise Fail "Invalid frame -- lookup"
            in
              scan (!frame)
            end
      in
        loop frames
      end

  fun define (frames, eq) (var, value) =
      case frames of
        nil => raise Fail "Empty env -- define"
      | frame::_ =>
        let
          fun scan (nil, nil) =
              let
                val (vars, vals) = !frame
              in
                frame := (var::vars, (ref value)::vals)
              end
            | scan (curvar::vars, curval::vals) =
              if eq (curvar, var) then
                curval := value
              else
                scan (vars, vals)
            | scan _ = raise Fail "Invalid frame -- define"
        in
          scan (!frame)
        end

  fun set (frames, eq) (var, value) =
      let
        fun loop nil = raise Fail "Unbound variable -- set"
          | loop (frame::frames) =
            let
              fun scan (nil, nil) = loop frames
                | scan (curvar::vars, curval::vals) =
                  if eq (curvar, var) then
                    curval := value
                  else
                    scan (vars, vals)
                | scan _ = raise Fail "Invalid frame -- set"
            in
              scan (!frame)
            end
      in
        loop frames
      end

  fun extend (frames, eq) (vars, vals) =
      let
        val lenvars = length vars
        val lenvals = length vals
      in
        if lenvars = lenvals then
          ((ref (vars, map ref vals))::frames, eq)
        else if lenvars < lenvals then
          raise Fail "Too many arguments supplied -- extend"
        else
          raise Fail "Too few arguments supplied -- extend"
      end
end;

(* Exercise 4.11 *)
structure Env' :> ENV =
struct
  type ('a, 'b) bind = ('a * 'b ref)
  type ('a, 'b) frame = (('a, 'b) bind list) ref
  type ('a, 'b) t = (('a, 'b) frame list) * ('a * 'a -> bool)

  fun make eq = (U.log "Env': created"; (nil, eq))

  fun lookup (frames, eq) var =
      let
        fun loop nil = raise Fail "Unbound variable -- lookup"
          | loop (frame::frames) =
            let
              fun scan nil = loop frames
                | scan ((curvar, curval)::binds) =
                  if eq (curvar, var) then
                    !curval
                  else
                    scan binds
            in
              scan (!frame)
            end
      in
        loop frames
      end

  fun define (frames, eq) (var, value) =
      case frames of
        nil => raise Fail "Empty env -- define"
      | frame::_ =>
        let
          fun scan nil =
              let
                val binds = !frame
              in
                frame := (var, ref value)::binds
              end
            | scan ((curvar, curval)::binds) =
              if eq (curvar, var) then
                curval := value
              else
                scan binds
        in
          scan (!frame)
        end

  fun set (frames, eq) (var, value) =
      let
        fun loop nil = raise Fail "Unbound variable -- set"
          | loop (frame::frames) =
            let
              fun scan nil = loop frames
                | scan ((curvar, curval)::binds) =
                  if eq (curvar, var) then
                    curval := value
                  else
                    scan binds
            in
              scan (!frame)
            end
      in
        loop frames
      end

  fun extend (frames, eq) (vars, vals) =
      let
        fun mkBind (var, value) = (var, ref value)
        val binds = ListPair.mapEq mkBind (vars, vals)
      in
        ((ref binds)::frames, eq)
      end
end;

(* Exercise 4.12 *)
structure Env'' :> ENV =
struct
  type ('a, 'b) frame = ('a list * ('b ref) list) ref
  type ('a, 'b) t = (('a, 'b) frame list) * ('a * 'a -> bool)

  fun make eq = (U.log "Env'': created"; (nil, eq))

  fun traverse f g frames =
      let
        fun loop nil = NONE
          | loop (frame::frames) =
            let
              fun scan (nil, nil) =
                  g (frame, fn () => loop frames)
                | scan (curvar::vars, curval::vals) =
                  (case f (curvar, curval) of
                     v as SOME _ => v
                   | NONE => scan (vars, vals))
                | scan _ = raise Fail "Invalid frame -- traverse"
            in
              scan (!frame)
            end
      in
        loop frames
      end

  fun lookup (frames, eq) var =
      let
        fun f (curvar, curval) =
            if eq (curvar, var) then SOME (!curval)
            else NONE
        fun g (_, gotoNextFrame) = gotoNextFrame ()
      in
        case traverse f g frames of
          SOME value => value
        | NONE => raise Fail "Unbound variable -- lookup"
      end

  fun define (frames, eq) (var, value) =
      let
        fun f (curvar, curval) =
            if eq (curvar, var) then
              (curval := value; SOME ())
            else
              NONE
        fun g (curframe, _) =
            let
              val (vars, vals) = !curframe
            in
              curframe := (var::vars, (ref value)::vals);
              SOME ()
            end
      in
        case traverse f g frames of
          SOME _ => ()
        | NONE => raise Fail "Empty env -- define"
      end

  fun set (frames, eq) (var, value) =
      let
        fun f (curvar, curval) =
            if eq (curvar, var) then
              (curval := value; SOME ())
            else
              NONE
        fun g (_, gotoNextFrame) = gotoNextFrame ()
      in
        case traverse f g frames of
          SOME _ => ()
        | NONE => raise Fail "Unbound variable -- set"
      end

  fun extend (frames, eq) (vars, vals) =
      let
        val lenvars = length vars
        val lenvals = length vals
      in
        if lenvars = lenvals then
          ((ref (vars, map ref vals))::frames, eq)
        else if lenvars < lenvals then
          raise Fail "Too many arguments supplied -- extend"
        else
          raise Fail "Too few arguments supplied -- extend"
      end
end;

(* 4.1.4  Running the Evaluator as a Program *)

signature LISP_INTERPRETER =
sig
  (* activates new top-level *)
  val go : unit -> unit
  (* returns an executor for unit test *)
  val ut : unit -> (string * string -> bool)
  (* run predefined unit tests *)
  val test : unit -> unit
end;

functor LispInterpreterFn (structure Lisp : LISP
                           and Syntax : LISP_SYNTAX
                           and Reader : LISP_READER
                           and Printer : LISP_PRINTER
                           and Evaluator : LISP_EVALUATOR
                           sharing type Syntax.obj = Lisp.obj
                           and type Reader.obj = Lisp.obj
                           and type Printer.obj = Lisp.obj
                           and type Evaluator.obj = Lisp.obj)
        : LISP_INTERPRETER =
struct
  val stdIn = Lisp.stdIn
  val stdOut = Lisp.stdOut
  val stdErr = Lisp.stdErr
  val quit = Lisp.sym ":q"

  fun subr0 (name, proc) =
      (Lisp.sym name, Lisp.subr0 (name, proc))
  fun subr1 (name, proc) =
      (Lisp.sym name, Lisp.subr1 (name, proc))
  fun subr2 (name, proc) =
      (Lisp.sym name, Lisp.subr2 (name, proc))
  fun subr0R (name, proc) =
      (Lisp.sym name, Lisp.subr0R (name, proc))
  fun subr1R (name, proc) =
      (Lisp.sym name, Lisp.subr1R (name, proc))
  fun subr2R (name, proc) =
      (Lisp.sym name, Lisp.subr2R (name, proc))
  val primitiveProcedures =
      [subr2 ("cons", Lisp.cons),
       subr1 ("car", Lisp.car),
       subr1 ("cdr", Lisp.cdr),
       subr1 ("caar", Lisp.caar),
       subr1 ("cadr", Lisp.cadr),
       subr1 ("cdar", Lisp.cdar),
       subr1 ("cddr", Lisp.cddr),
       subr1 ("caaar", Lisp.caaar),
       subr1 ("caadr", Lisp.caadr),
       subr1 ("cadar", Lisp.cadar),
       subr1 ("cdaar", Lisp.cdaar),
       subr1 ("caddr", Lisp.caddr),
       subr1 ("cdadr", Lisp.cdadr),
       subr1 ("cddar", Lisp.cddar),
       subr1 ("cdddr", Lisp.cdddr),
       subr2 ("set-car!", fn (lst,obj) => Lisp.setCar lst obj),
       subr2 ("set-cdr!", fn (lst,obj) => Lisp.setCdr lst obj),
       subr0R ("list", Lisp.fromList),
       subr1 ("length",
              (fn lst =>
                  let
                    fun len l =
                        if Lisp.isNull l then 0
                        else 1 + (len (Lisp.cdr l))
                  in
                    Lisp.int (len lst)
                  end)),
       subr2 ("nth",
              (fn (lst,n) =>
                  let
                    fun error () =
                        raise Lisp.Error ("Subscript out of bounds", nil)
                    fun iter (l, i) =
                        if Lisp.isNull l then
                          error ()
                        else
                          if i = 0 then
                            Lisp.car l
                          else
                            iter (Lisp.cdr l, i - 1)
                    val i = Lisp.toInt n
                  in
                    if i < 0 then error ()
                    else iter (lst, i)
                  end)),
       subr2 ("assoc",
              (fn (key,lst) =>
                  let
                    fun iter (k,l) =
                        if Lisp.isNull l then
                          Lisp.f
                        else
                          let
                            val p = Lisp.car l
                            val k' = Lisp.car p
                          in
                            if Lisp.equal (k,k') then p
                            else iter (k, Lisp.cdr l)
                          end
                  in
                    iter (key, lst)
                  end)),
       subr2R ("map",
               (fn (p,lst,lsts) =>
                   let
                     fun isEnd nil = false
                       | isEnd (l::ll) =
                         if Lisp.isNull l then true
                         else isEnd ll
                     fun first nil = nil
                       | first (l::ll) = (Lisp.car l)::(first ll)
                     fun rest nil = nil
                       | rest (l::ll) = (Lisp.cdr l)::(rest ll)
                     fun map ll =
                         if isEnd ll then Lisp.null
                         else
                           Lisp.cons (Evaluator.apply p (first ll),
                                      map (rest ll))
                   in
                     map (lst::lsts)
                   end)),
       subr2 ("eq?", Lisp.bool o Lisp.eq),
       subr2 ("equal?", Lisp.bool o Lisp.equal),
       subr1 ("null?", Lisp.bool o Lisp.isNull),
       subr1 ("true?", Lisp.bool o Lisp.isTrue),
       subr1 ("false?", Lisp.bool o Lisp.isFalse),
       subr1 ("pair?", Lisp.bool o Lisp.isCons),
       subr1 ("symbol?", Lisp.bool o Lisp.isSym),
       subr1 ("bool?", Lisp.bool o Lisp.isBool),
       subr1 ("int?", Lisp.bool o Lisp.isInt),
       subr1 ("string?", Lisp.bool o Lisp.isStr),
       subr1 ("subr?", Lisp.bool o Lisp.isSubr),
       subr1 ("expr?", Lisp.bool o Lisp.isExpr),
       subr1 ("input-stream?", Lisp.bool o Lisp.isInputStream),
       subr1 ("output-stream?", Lisp.bool o Lisp.isOutputStream),
       subr0R ("+",
               (fn ns =>
                   let
                     fun f (a, b) = b + (Lisp.toInt a)
                   in
                     (*
                      * foldl f i [i0,i1,...,iN]; where f(a,b) = b+a
                      * = f(iN,...,f(i1,f(i0,i)))
                      * = (((i+i0)+i1)+...+iN)
                      *)
                     Lisp.int (List.foldl f 0 ns)
                   end)),
       subr0R ("-",
               (fn nil => Lisp.int 0
                 | (n::nil) => Lisp.int (~ (Lisp.toInt n))
                 | (n::ns) =>
                   let
                     fun f (a, b) = b - (Lisp.toInt a)
                     val i = Lisp.toInt n
                   in
                     (*
                      * foldl f i [i0,i1,...,iN]; where f(a,b) = b-a
                      * = f(iN,...,f(i1,f(i0,i)))
                      * = (((i-i0)-i1)-...-iN)
                      *)
                     Lisp.int (List.foldl f i ns)
                   end)),
       subr0R ("*",
               (fn ns =>
                   let
                     fun f (a, b) = b * (Lisp.toInt a)
                   in
                     (*
                      * foldl f i [i0,i1,...,iN]; where f(a,b) = b*a
                      * = f(iN,...,f(i1,f(i0,i)))
                      * = (((i*i0)*i1)*...*iN)
                      *)
                     Lisp.int (List.foldl f 1 ns)
                   end)),
       subr2 ("/",
              (fn (n1,n2) =>
                  let
                    val i1 = Lisp.toInt n1
                    val i2 = Lisp.toInt n2
                  in
                    Lisp.int (i1 div i2)
                  end)),
       subr2 ("%",
              (fn (n1,n2) =>
                  let
                    val i1 = Lisp.toInt n1
                    val i2 = Lisp.toInt n2
                  in
                    Lisp.int (i1 mod i2)
                  end)),
       subr2 ("=",
              (fn (n1,n2) =>
                  let
                    val i1 = Lisp.toInt n1
                    val i2 = Lisp.toInt n2
                  in
                    Lisp.bool (i1 = i2)
                  end)),
       subr2 (">=",
              (fn (n1,n2) =>
                  let
                    val i1 = Lisp.toInt n1
                    val i2 = Lisp.toInt n2
                  in
                    Lisp.bool (i1 >= i2)
                  end)),
       subr2 (">",
              (fn (n1,n2) =>
                  let
                    val i1 = Lisp.toInt n1
                    val i2 = Lisp.toInt n2
                  in
                    Lisp.bool (i1 > i2)
                  end)),
       subr2 ("<=",
              (fn (n1,n2) =>
                  let
                    val i1 = Lisp.toInt n1
                    val i2 = Lisp.toInt n2
                  in
                    Lisp.bool (i1 <= i2)
                  end)),
       subr2 ("<",
              (fn (n1,n2) =>
                  let
                    val i1 = Lisp.toInt n1
                    val i2 = Lisp.toInt n2
                  in
                    Lisp.bool (i1 < i2)
                  end)),
       subr0 ("read", fn () => Reader.read stdIn),
       subr1 ("print",
              (fn obj =>
                  (Printer.print (stdOut, obj); obj))),
       subr1 ("print-string",
              (fn obj =>
                  (Printer.printString (stdOut, Lisp.toString obj); obj))),
       subr0 ("terpri",
              (fn () => (Printer.terpri stdOut; Lisp.t))),
       subr0 ("flush",
              (fn () => (Printer.flush stdOut; Lisp.t))),
       subr1R ("format",
               (fn (fmt,args) =>
                   (Printer.format (stdOut, Lisp.toString fmt, args);
                    Lisp.t))),
       subr2 ("eval",
              (fn (exp, env) =>
                  Evaluator.eval exp env)),
       subr2 ("apply",
              (fn (proc, args) =>
                  Evaluator.apply proc (Lisp.toList args))),
       subr1 ("expand-syntax",
              (fn exp =>
                  if Syntax.isDerived exp then
                    Syntax.expandDerived exp
                  else
                    exp)),
       subr1R ("error",
               (fn (fmt,args) =>
                   raise Lisp.Error (Lisp.toString fmt, args)))]

  fun setupEnv () =
      let
        fun primitiveProcNames () =
            map #1 primitiveProcedures
        fun primitiveProcObjects () =
            map #2 primitiveProcedures
        val env = Lisp.extendEnv (Lisp.newEnv ())
                                 (primitiveProcNames (),
                                  primitiveProcObjects ())
        val (envFnName, envFnObj) =
            subr0 ("user-init-env", fn () => env)
      in
        Lisp.defineEnv env (Syntax.TRUE, Lisp.t);
        Lisp.defineEnv env (Syntax.FALSE, Lisp.f);
        Lisp.defineEnv env (envFnName, envFnObj);
        env
      end

  fun hello () =
      Printer.format (stdOut, "Hello!~%Press ':q' to exit", nil);

  fun bye () =
      Printer.format (stdOut, "Bye!~%", nil)

  fun onError (Lisp.Error (ctrlstr,args), cont) =
      let
        val msg = "Runtime error: " ^ ctrlstr ^ "~%"
      in
        Printer.format (stdErr, msg, args);
        cont ()
      end
    | onError (IO.Io {name,function,cause}, cont) =
      let
        val msg = "IO error: " ^ name ^ " -- " ^ function ^
                  " (cause: " ^ exnMessage cause ^ ")~%"
      in
        Printer.format (stdErr, msg, nil);
        cont ()
      end
    | onError (e, _) = raise e

  fun repl () =
      let
        val env = setupEnv ()
        fun loop () =
            let
              val obj = (Printer.format (stdOut, "~%> ", nil);
                         Reader.read stdIn)
            in
              if Lisp.isEof obj orelse Lisp.eq (obj, quit) then
                ()
              else
                let
                  val obj' = Evaluator.eval obj env
                in
                  Printer.print (stdOut, obj');
                  loop ()
                end
            end
            handle e => onError (e, loop)
      in
        loop ()
      end

  fun go () = (hello (); repl (); bye ())

  fun ut () =
      let
        val env = setupEnv ()
        val s2i = Lisp.inputStream o TextIO.openString
      in
        (fn (input, expected) =>
            let
              val is = s2i input
              val is' = s2i expected
              val obj = Reader.read is
              val obj' = Reader.read is'
              val ret = Evaluator.eval obj env
              val ret' = Evaluator.eval obj' env
            in
              if Lisp.equal (ret, ret') then
                (Printer.format (stdOut,
                                 "OK: ~S -> ~S~%",
                                 [obj, ret]);
                 true)
              else
                (Printer.format (stdOut,
                                 "NG: ~S -> ~S; (expected: ~S)~%",
                                 [obj, ret, ret']);
                 false)
            end)
      end

  fun test () =
      let
        val ut = ut ()
      in
        ut ("(true? true)", "true");
        ut ("(true? 1)", "true");
        ut ("(true? '())", "true");
        ut ("(true? false)", "false");
        ut ("(false? true)", "false");
        ut ("(false? 1)", "false");
        ut ("(false? '())", "false");
        ut ("(false? false)", "true");
        ut ("(null? true)", "false");
        ut ("(null? 1)", "false");
        ut ("(null? '())", "true");
        ut ("(null? false)", "false");
        ut ("(+)", "0");
        ut ("(+ 1)", "1");
        ut ("(+ 1 2)", "3");
        ut ("(+ 1 2 3)", "6");
        ut ("(-)", "0");
        ut ("(- 1)", "~1");
        ut ("(- 1 2)", "~1");
        ut ("(- 1 2 3)", "~4");
        ut ("(*)", "1");
        ut ("(* 2)", "2");
        ut ("(* 2 3)", "6");
        ut ("(* 2 3 4)", "24");
        ut ("(/ 7 3)", "2");
        ut ("(% 7 3)", "1");
        ut ("(< 1 2)", "true");
        ut ("(< 1 1)", "false");
        ut ("(< 2 1)", "false");
        ut ("(<= 1 2)", "true");
        ut ("(<= 1 1)", "true");
        ut ("(<= 2 1)", "false");
        ut ("(> 1 2)", "false");
        ut ("(> 1 1)", "false");
        ut ("(> 2 1)", "true");
        ut ("(>= 1 2)", "false");
        ut ("(>= 1 1)", "true");
        ut ("(>= 2 1)", "true");
        ut ("(if (> 2 1) 'a 'b)", "'a");
        ut ("(if (> 1 1) 'a 'b)", "'b");
        ut ("(cond ((> 2 1) 'a) (else 'b))", "'a");
        ut ("(cond ((> 1 1) 'a) (else 'b))", "'b");
        ut ("(cond ((assoc 'b '((a 1) (b 2))) => cadr)" ^
            "      (else false))", "2");
        ut ("(cond ((assoc 'c '((a 1) (b 2))) => cadr)" ^
            "      ((assoc 'a '((a 1) (b 2))) => cadr)" ^
            "      (else false))", "1");
        ut ("(and)", "true");
        ut ("(and 1)", "1");
        ut ("(and false (error \"oops!\"))", "false");
        ut ("(and 1 false)", "false");
        ut ("(and 1 true)", "true");
        ut ("(and true 1)", "1");
        ut ("(or)", "false");
        ut ("(or 1)", "1");
        ut ("(or false false)", "false");
        ut ("(or false 1)", "1");
        ut ("(or 1 (error \"oops!\"))", "1");
        ut ("(or true (error \"oops!\"))", "true");
        ut ("(let () 1)", "1");
        ut ("(let ((x 1) (y 2)) (+ x y))", "3");
        ut ("(begin (define x 1) x)", "1");
        ut ("x", "1");
        ut ("(begin (set! x 2) x)", "2");
        ut ("x", "2");
        ut ("(let ((x x)) (set! x 3) x)", "3");
        ut ("x", "2");
        ut ("(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))", "39");
        ut ("(let loop ((a 1) (b 0) (count 10))" ^
            " (if (= count 0) b (loop (+ a b) a (- count 1))))",
            "55"); (* fib 10 -> 55 *)
        ut ("(length '())", "0");
        ut ("(length '(1 2 3))", "3");
        ut ("(nth '(1 2 3 4) 2)", "3");
        ut ("(assoc 'b '((a . 1) (b . 2)))", "'(b . 2)");
        ut ("(assoc 'c '((a . 1) (b . 2)))", "false");
        ut ("(map (lambda (x) (+ x x)) '(1 2 3))", "'(2 4 6)");
        ut ("(map (lambda (x y) (+ x y)) '(1 2) '(3 2 1 0))", "'(4 4)");
        ut ("((lambda (x)"^
            "   (define (even? n)"^
            "     (if (= n 0) true (odd? (- n 1))))"^
            "   (define (odd? n)"^
            "     (if (= n 0) false (even? (- n 1))))"^
            "   (list (odd? x) (even? x))) 3)",
            "(list true false)");
        ut ("((lambda (x)"^
            "   (letrec ((even?"^
            "             (lambda (n)"^
            "               (if (= n 0)"^
            "                   true"^
            "                 (odd? (- n 1)))))"^
            "            (odd?"^
            "             (lambda (n)"^
            "               (if (= n 0)"^
            "                   false"^
            "                 (even? (- n 1))))))"^
            "           (list (odd? x) (even? x)))) 3)",
            "(list true false)");
        ut ("((lambda (n)"^
            "   ((lambda (fact)"^
            "      (fact fact n))"^
            "    (lambda (ft k)"^
            "      (if (= k 1)"^
            "          1"^
            "          (* k (ft ft (- k 1)))))))"^
            " 5)",
            "120");
        print "done\n"
      end
      handle e => onError (e, fn () => ())
end;

local
  structure E = Env
  (*
  structure E = Env' (* Exercise 4.11 *)
  structure E = Env'' (* Exercise 4.12 *)
   *)
  structure L = LispFn (structure Env = E)
  structure LS = LispSyntaxFn (structure Lisp = L)
  structure LR = LispReaderFn (structure Lisp = L and Syntax = LS)
  structure LP = LispPrinterFn (structure Lisp = L and Syntax = LS)
  structure LE = LispEvaluatorFn (structure Lisp = L and Syntax = LS)
in
structure LI = LispInterpreterFn (structure Lisp = L
                                  and Syntax = LS
                                  and Reader = LR
                                  and Printer = LP
                                  and Evaluator = LE)
end;

(*
 * LI.go (); (* => activates top-level *)
 * LI.test (); (* => executes predefined unit tests *)
 *)

(* 4.1.5  Data as Programs *)

(* 4.1.6  Internal Definitions *)

(*
 * scan-out-defines (Exercise 4.16) and letrec (Exercise 4.20)
 * are placed in the body of LispSyntaxFn functor (defined above).
 *)

(* 4.1.7  Separating Syntactic Analysis from Execution *)

functor LispEvaluatorFn' (structure Lisp: LISP
                          and Syntax: LISP_SYNTAX
                          sharing type Syntax.obj = Lisp.obj)
        : LISP_EVALUATOR =
struct
  type obj = Lisp.obj

  fun analyze exp : obj -> obj =
      if Syntax.isSelfEvaluating exp then
        analyzeSelfEvaluating exp
      else if Syntax.isVariable exp then
        analyzeVariable exp
      else if Syntax.isQuoted exp then
        analyzeQuoted exp
      else if Syntax.isAssignment exp then
        analyzeAssignment exp
      else if Syntax.isDefinition exp then
        analyzeDefinition exp
      else if Syntax.isIf exp then
        analyzeIf exp
      else if Syntax.isLambda exp then
        analyzeLambda exp
      else if Syntax.isBegin exp then
        analyzeSequence (Syntax.beginActions exp)
      else if Syntax.isDerived exp then
        analyze (Syntax.expandDerived exp)
      else if Syntax.isApplication exp then
        analyzeApplication exp
      else
        raise Lisp.Error ("Unknown expression type -- analyze: ~S",
                          [exp])

  and analyzeSelfEvaluating exp =
      (fn _ => exp)

  and analyzeVariable exp =
      (fn env => Lisp.lookupEnv env exp)

  and analyzeQuoted exp =
      let
        val qval = Syntax.textOfQuotation exp
      in
        (fn _ => qval)
      end

  and analyzeAssignment exp =
      let
        val var = Syntax.assignmentVariable exp
        val vproc = analyze (Syntax.assignmentValue exp)
      in
        (fn env => (Lisp.setEnv env (var, vproc env); var))
      end

  and analyzeDefinition exp =
      let
        val var = Syntax.definitionVariable exp
        val vproc = analyze (Syntax.definitionValue exp)
      in
        (fn env => (Lisp.defineEnv env (var, vproc env); var))
      end

  and analyzeIf exp =
      let
        val pproc = analyze (Syntax.ifPredicate exp)
        val cproc = analyze (Syntax.ifConsequent exp)
        val aproc = analyze (Syntax.ifAlternative exp)
      in
        (fn env =>
            if Lisp.isTrue (pproc env) then cproc env
            else aproc env)
      end

  and analyzeLambda exp =
      let
        val vars = Syntax.lambdaParameters exp
        val bproc = analyzeSequence (Syntax.lambdaBody exp)
      in
        (fn env => Lisp.expr (vars,
                              Lisp.subr1 ("bproc", bproc),
                              env))
      end

  and analyzeSequence exps =
      let
        fun sequentially (p1, p2) =
            (fn env => (p1 env; p2 env))
        fun loop (first, nil) =
            first
          | loop (first, second::rest) =
            loop (sequentially (first, second), rest)
      in
        case toProcs exps of
          nil => raise Lisp.Error ("Empty sequence -- analyzeSequence",
                                   nil)
        | (p::ps) => loop (p, ps)
      end

  and analyzeApplication exp =
      let
        val fproc = analyze (Syntax.operator exp)
        val aprocs = toProcs (Syntax.operands exp)
      in
        (fn env => executeApplication (fproc env)
                                      (map (fn p => p env) aprocs))
      end

  and executeApplication proc args =
      if Lisp.isSubr proc then
        Lisp.applySubr proc args
      else if Lisp.isExpr proc then
        let
          (* params *)
          val params = Lisp.toList (Lisp.exprParams proc)
          (* body: represented as procedure (env -> obj) *)
          val body = Lisp.exprBody proc
          (* env: environment to which body is applied *)
          val env = Lisp.extendEnv (Lisp.exprEnv proc) (params, args)
        in
          Lisp.applySubr body [env]
        end
      else
        raise Lisp.Error ("Not a procedure -- executeApplication: ~S",
                          [proc])

  and toProcs exps =
      if Lisp.isNull exps then
        nil
      else if Lisp.isCons exps then
        (analyze (Lisp.car exps)) :: (toProcs (Lisp.cdr exps))
      else
        raise Lisp.Error ("Improper sequence: ~S", [exps])

  val eval = analyze

  val apply = executeApplication
end;

local
  structure E = Env
  structure L = LispFn (structure Env = E)
  structure LS = LispSyntaxFn (structure Lisp = L)
  structure LR = LispReaderFn (structure Lisp = L and Syntax = LS)
  structure LP = LispPrinterFn (structure Lisp = L and Syntax = LS)
  structure LE = LispEvaluatorFn' (structure Lisp = L and Syntax = LS)
in
structure LI' = LispInterpreterFn (structure Lisp = L
                                   and Syntax = LS
                                   and Reader = LR
                                   and Printer = LP
                                   and Evaluator = LE)
end;

(*
 * LI'.go (); (* => activates top-level *)
 * LI'.test (); (* => executes predefined unit tests *)
 *)
