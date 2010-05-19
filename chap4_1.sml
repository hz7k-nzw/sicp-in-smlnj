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

signature LISP_OBJECT =
sig
  type t (* type for lisp object *)

  exception Error of string * t list

  (* constants *)
  val undef : t
  val eof : t
  val null : t
  val T : t
  val F : t
  val zero : t
  val one : t
  val stdIn : t
  val stdOut : t
  val stdErr : t

  (* constructors *)
  val cons : t * t -> t
  val sym : string -> t
  val bool : bool -> t
  val int : int -> t
  val real : real -> t
  val str : string -> t
  val subr0 : string * (unit -> t) -> t
  val subr1 : string * (t -> t) -> t
  val subr2 : string * (t * t -> t) -> t
  val subr3 : string * (t * t * t -> t) -> t
  val subr0R : string * (t list -> t) -> t
  val subr1R : string * (t * t list -> t) -> t
  val subr2R : string * (t * t * t list -> t) -> t
  val subr3R : string * (t * t * t * t list -> t) -> t
  val expr : t * t * t -> t
  val inputPort : TextIO.instream -> t
  val outputPort : TextIO.outstream -> t
  val thunk : t * t -> t (* used in chap4_2.sml *)

  (* predicates for equality tests *)
  val eq : t * t -> bool
  val equal : t * t -> bool

  (* t <-> (sml) list *)
  val fromList : t list -> t
  val toList : t -> t list

  (* predicates for boolean tests *)
  val isTrue : t -> bool
  val isFalse : t -> bool

  (* predicates for data type tests *)
  val isUndef : t -> bool
  val isEof : t -> bool
  val isNull : t -> bool
  val isBool : t -> bool
  val isCons : t -> bool
  val isSym : t -> bool
  val isNum : t -> bool
  val isStr : t -> bool
  val isSubr : t -> bool
  val isExpr : t -> bool
  val isInputPort : t -> bool
  val isOutputPort : t -> bool
  val isEnv : t -> bool
  val isThunk : t -> bool (* used in chap4_2.sml *)

  (* for cons *)
  val car : t -> t
  val cdr : t -> t
  val caar : t -> t
  val cadr : t -> t
  val cdar : t -> t
  val cddr : t -> t
  val caaar : t -> t
  val caadr : t -> t
  val cadar : t -> t
  val cdaar : t -> t
  val caddr : t -> t
  val cdadr : t -> t
  val cddar : t -> t
  val cdddr : t -> t
  val setCar : t -> t -> t
  val setCdr : t -> t -> t
  val map : (t -> t) -> t -> t
  val append : (t * t) -> t

  (* for symbol *)
  val pname : t -> string

  (* for bool *)
  val toBool : t -> bool
  val not : t -> t

  (* for num *)
  val isInt : t -> bool
  val isReal : t -> bool
  val toInt : t -> int
  val toReal : t -> real
  val negNum : t -> t
  val addNum : (t * t) -> t
  val subNum : (t * t) -> t
  val mulNum : (t * t) -> t
  val quoNum : (t * t) -> t
  val remNum : (t * t) -> t
  val eqNum : (t * t) -> bool
  val gtNum : (t * t) -> bool
  val geNum : (t * t) -> bool
  val ltNum : (t * t) -> bool
  val leNum : (t * t) -> bool

  (* for string *)
  val toString : t -> string

  (* for subr (primitive procedure) *)
  val subrName : t -> string
  val applySubr : t -> t list -> t

  (* for expr (compound procedure) *)
  val exprParams : t -> t
  val exprBody : t -> t
  val exprEnv : t -> t

  (* for input/output port *)
  val toInstream : t -> TextIO.instream
  val toOutstream : t -> TextIO.outstream
  val openIn : string -> t
  val openOut : string -> t
  val closeIn : t -> t
  val closeOut : t -> t

  (* for env *)
  val newEnv : unit -> t
  val lookupEnv : t -> t -> t
  val extendEnv : t -> (t list * t list) -> t
  val defineEnv : t -> (t * t) -> t
  val setEnv : t -> (t * t) -> t

  (* for thunk: used in chap4_2.sml *)
  val thunkExp : t -> t
  val thunkEnv : t -> t
  val thunkValue : t -> t
  val setThunkValue : t -> t -> t
  val isEvaluated : t -> bool
end;

signature LISP_SYNTAX =
sig
  type obj

  (* predefined symbols *)
  val AMB : obj
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

  (* for amb evaluator: used in chap4_3.sml *)
  val isAmb : obj -> bool
  val ambChoices : obj -> obj
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
  val print : obj * obj -> obj
  val printString : obj * string -> obj
  val terpri : obj -> obj
  val flush : obj -> obj
  val format : obj * string * obj list -> obj
  val printException : obj * exn -> obj
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

functor LispEvaluatorFn (structure Obj: LISP_OBJECT
                         and Syntax: LISP_SYNTAX
                         sharing type Syntax.obj = Obj.t)
        : LISP_EVALUATOR =
struct
  type obj = Obj.t

  fun eval exp env =
      if Syntax.isSelfEvaluating exp then
        exp
      else if Syntax.isVariable exp then
        Obj.lookupEnv env exp
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
        raise Obj.Error ("Unknown expression type -- eval: ~S",
                         [exp])

  and evalAssignment exp env =
      (Obj.setEnv env (Syntax.assignmentVariable exp,
                       eval (Syntax.assignmentValue exp) env);
       Syntax.assignmentVariable exp)

  and evalDefinition exp env =
      (Obj.defineEnv env (Syntax.definitionVariable exp,
                          eval (Syntax.definitionValue exp) env);
       Syntax.definitionVariable exp)

  and evalIf exp env =
      if Obj.isTrue (eval (Syntax.ifPredicate exp) env) then
        eval (Syntax.ifConsequent exp) env
      else
        eval (Syntax.ifAlternative exp) env

  and evalLambda exp env =
      Obj.expr (Syntax.lambdaParameters exp,
                Syntax.lambdaBody exp,
                env)

  and evalSequence exp env =
      if Obj.isNull exp then
        raise Obj.Error ("Empty sequence -- evalSequence",
                         nil)
      else if Obj.isCons exp then
        let
          val car = Obj.car exp
          val cdr = Obj.cdr exp
        in
          if Obj.isNull cdr then
            eval car env
          else
            (eval car env; evalSequence cdr env)
        end
      else
        raise Obj.Error ("Improper sequence -- evalSequence: ~S",
                         [exp])

  and apply proc args =
      if Obj.isSubr proc then
        Obj.applySubr proc args
      else if Obj.isExpr proc then
        let
          val body = Obj.exprBody proc
          val env = Obj.exprEnv proc
          val params = Obj.toList (Obj.exprParams proc)
        in
          evalSequence body (Obj.extendEnv env (params, args))
        end
      else
        raise Obj.Error ("Not a procedure -- apply: ~S",
                         [proc])

  and listOfValues exps env =
      if Obj.isNull exps then
        nil
      else if Obj.isCons exps then
        let
          val car = Obj.car exps
          val cdr = Obj.cdr exps
          (* evaluates operands from left to right *)
          val v = eval car env
          val vs = listOfValues cdr env
        in
          v :: vs
        end
      else
        raise Obj.Error ("Improper sequence -- listOfValues: ~S",
                         [exps])
end;

functor LispReaderFn (structure Obj: LISP_OBJECT
                      and Syntax: LISP_SYNTAX
                      sharing type Syntax.obj = Obj.t)
        : LISP_READER =
struct
  type obj = Obj.t

  datatype token = Literal of string
                 | LParen
                 | RParen
                 | Dot
                 | Sharp
                 | Quote
                 | DoubleQuote
                 | SemiColon
                 | Eof
                 | None (* token for initial state *)

  type tokenstate = token ref * (* current token *)
                    bool ref * (* unread flag *)
                    TextIO.instream (* input stream *)

  fun readError (msg) = raise Obj.Error (msg, nil)

  fun read is =
      let
        val istream = Obj.toInstream is
      in
        doRead istream
      end

  and doRead istream =
      let
        val ts = (ref None, ref false, istream)
      in
        parseObjOrEof ts
      end

  and parseObjOrEof ts =
      (readToken ts;
       case getToken ts of
         Eof => Obj.eof
       | SemiColon => (parseComment ts; parseObjOrEof ts)
       | _ => (unreadToken ts; parseObj ts))

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
       | SemiColon => (parseComment ts; parseObj ts)
       | Eof => readError ("Unexpected eof")
       | None => readError ("Unexpected token: None"))

  and parseLiteral s =
      let
        val ss = Substring.full s
        val radix = StringCvt.DEC
        fun parseInt ss =
            case Int.scan radix Substring.getc ss of
              SOME (i, ss') =>
              if Substring.isEmpty ss' then SOME i
              else NONE
            | NONE => NONE
        fun parseReal ss =
            case Real.scan Substring.getc ss of
              SOME (r, ss') =>
              if Substring.isEmpty ss' then SOME r
              else NONE
            | NONE => NONE
      in
        case parseInt ss of
          SOME i => Obj.int i
        | NONE =>
          (case parseReal ss of
             SOME r => Obj.real r
           | NONE => Obj.sym s)
      end

  and parseCons ts =
      (readToken ts;
       case getToken ts of
         RParen => Obj.null
       | _ =>
         (unreadToken ts;
          let
            val car = parseObj ts
          in
            readToken ts;
            case getToken ts of
              RParen => Obj.cons (car, Obj.null)
            | Dot =>
              let
                val cdr = parseObj ts
              in
                readToken ts;
                case getToken ts of
                  RParen => Obj.cons (car, cdr)
                | _ => readError ("Right paren expected")
              end
            | _ =>
              let
                val cdr = (unreadToken ts; parseCons ts)
              in
                Obj.cons (car, cdr)
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
               Obj.str s)
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

  and parseComment (_, _, istream) =
      let
        fun skip () =
            case TextIO.lookahead istream of
              SOME c => if #"\n" = c then ()
                        else (TextIO.input1 istream; skip ())
            | _ => ()
      in
        skip ()
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
                   | #";" => SemiColon
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
                  | #";" => Literal s
                  | _ => (read(); iter (s ^ String.str c))
              end
      in
        iter ""
      end
end;

functor LispPrinterFn (structure Obj: LISP_OBJECT
                      and Syntax: LISP_SYNTAX
                      sharing type Syntax.obj = Obj.t)
        : LISP_PRINTER =
struct
  type obj = Obj.t

  fun print (os, obj) =
      let
        val ostream = Obj.toOutstream os
      in
        doPrint (ostream, obj);
        TextIO.flushOut ostream;
        Obj.undef
      end

  and doPrint (ostream, obj) =
      let
        fun p s =
            TextIO.output (ostream, s)
        and p1 obj =
            if Obj.isNull obj then
              p "()"
            else if Obj.isCons obj then
              (p "(";
               p1 (Obj.car obj);
               p2 (Obj.cdr obj);
               p ")")
            else if Obj.isSym obj then
              pSym obj
            else if Obj.isBool obj then
              pBool obj
            else if Obj.isNum obj then
              pNum obj
            else if Obj.isStr obj then
              pStr obj
            else if Obj.isSubr obj then
              pSubr obj
            else if Obj.isExpr obj then
              pExpr obj
            else if Obj.isInputPort obj then
              pInputPort obj
            else if Obj.isOutputPort obj then
              pOutputPort obj
            else if Obj.isEnv obj then
              pEnv obj
            else if Obj.isUndef obj then
              pUndef obj
            else if Obj.isThunk obj then
              pThunk obj
            else
              pUnknown obj
        and p2 obj =
            if Obj.isNull obj then
              ()
            else if Obj.isCons obj then
              (p " ";
               p1 (Obj.car obj);
               p2 (Obj.cdr obj))
            else if Obj.isSym obj then
              (p " . "; pSym obj)
            else if Obj.isBool obj then
              (p " . "; pBool obj)
            else if Obj.isNum obj then
              (p " . "; pNum obj)
            else if Obj.isStr obj then
              (p " . "; pStr obj)
            else if Obj.isSubr obj then
              (p " . "; pSubr obj)
            else if Obj.isExpr obj then
              (p " . "; pExpr obj)
            else if Obj.isInputPort obj then
              (p " . "; pInputPort obj)
            else if Obj.isOutputPort obj then
              (p " . "; pOutputPort obj)
            else if Obj.isEnv obj then
              (p " . "; pEnv obj)
            else if Obj.isUndef obj then
              (p " . "; pUndef obj)
            else if Obj.isThunk obj then
              (p " . "; pThunk obj)
            else
              (p " . "; pUnknown obj)
        and pSym obj =
            p (Obj.pname obj)
        and pBool obj =
            if Obj.toBool obj then p "#T" else p "#F"
        and pNum obj =
            if Obj.isInt obj then
              p (Int.toString (Obj.toInt obj))
            else if Obj.isReal obj then
              p (Real.toString (Obj.toReal obj))
            else
              p "#<Unknown Number>"
        and pStr obj =
            (p "\"";
             p (String.toString (Obj.toString obj));
             p "\"")
        and pSubr obj =
            (p "#<Subr: ";
             p (Obj.subrName obj);
             p ">")
        and pExpr obj =
            let
              val params = Obj.exprParams obj
              val body = Obj.exprBody obj
            in
              (p "#<Expr: ";
               p1 (Syntax.makeLambda (params, body));
               p ">")
            end
        and pInputPort obj =
            p "#<InputPort>"
        and pOutputPort obj =
            p "#<OutputPort>"
        and pEnv obj =
            p "#<Env>"
        and pUndef obj =
            p "#<Undef>"
        and pThunk obj =
            let
              val evaluated = Obj.isEvaluated obj
              val content = if evaluated then Obj.thunkValue obj
                            else Obj.thunkExp obj
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
        val ostream = Obj.toOutstream os
      in
        TextIO.output (ostream, s);
        TextIO.flushOut ostream;
        Obj.undef
      end

  fun terpri os =
      let
        val ostream = Obj.toOutstream os
      in
        TextIO.output (ostream, "\n");
        TextIO.flushOut ostream;
        Obj.undef
      end

  fun flush os =
      let
        val ostream = Obj.toOutstream os
      in
        TextIO.flushOut ostream;
        Obj.undef
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
                raise Obj.Error ("Unexpected ctrlstr: " ^
                                 Substring.string ss, nil)
            end

        fun traverse (nil, _) = ()
          | traverse ((Literal s)::fs, args) =
            (printString (os, s); traverse (fs, args))
          | traverse (Sexp::fs, arg::args) =
            (print (os, arg); traverse (fs, args))
          | traverse (Sexp::_, nil) =
            raise Obj.Error ("Not enough arguments: " ^
                             Substring.string ss, nil)
          | traverse (Newline::fs, args) =
            (terpri os; traverse (fs, args))
      in
        traverse (parse ss, args);
        Obj.undef
      end

  fun printException (os, Obj.Error (ctrlstr,args)) =
      let
        val msg = "Runtime error: " ^ ctrlstr ^ "~%"
      in
        format (os, msg, args);
        Obj.undef
      end
    | printException (os, IO.Io {name,function,cause}) =
      let
        val msg = "IO error: " ^ name ^ " -- " ^ function ^
                  " (cause: " ^ exnMessage cause ^ ")~%"
      in
        format (os, msg, nil);
        Obj.undef
      end
    | printException (os, e) =
      let
        val msg = "Runtime Error: " ^ exnMessage e ^ "~%"
      in
        format (os, msg, nil);
        Obj.undef
      end
end;

(* 4.1.2  Representing Expressions *)

functor LispSyntaxFn (structure Obj: LISP_OBJECT) : LISP_SYNTAX =
struct
  type obj = Obj.t

  val AMB = Obj.sym "amb"
  val BEGIN = Obj.sym "begin"
  val DEFINE = Obj.sym "define"
  val FALSE = Obj.sym "false"
  val IF = Obj.sym "if"
  val LAMBDA = Obj.sym "lambda"
  val QUOTE = Obj.sym "quote"
  val SET = Obj.sym "set!"
  val TRUE = Obj.sym "true"

  val AND = Obj.sym "and"
  val ARROW = Obj.sym "=>"
  val COND = Obj.sym "cond"
  val ELSE = Obj.sym "else"
  val OR = Obj.sym "or"
  val LET = Obj.sym "let"
  val LET2 = Obj.sym "let*"
  val LETREC = Obj.sym "letrec"

  val useScanOutDefines = false

  fun isSelfEvaluating exp =
      (*Obj.isNull exp orelse*)
      (*Obj.isBool exp orelse*)
      Obj.isNum exp orelse
      Obj.isStr exp

  val isVariable = Obj.isSym

  fun isTaggedList tag exp =
      Obj.isCons exp andalso
      Obj.eq (Obj.car exp, tag)

  fun isQuoted exp = isTaggedList QUOTE exp
  and textOfQuotation exp = Obj.cadr exp
  and makeQuote exp = Obj.fromList [QUOTE, exp]

  and makeTrue () = TRUE (* makeQuote Obj.T *)
  and makeFalse () = FALSE (* makeQuote Obj.F *)

  and isLambda exp = isTaggedList LAMBDA exp
  and lambdaParameters exp = Obj.cadr exp
  and lambdaBody exp = Obj.cddr exp
  and makeLambda (params, body) =
      let
        val body' = if useScanOutDefines then
                      scanOutDefines body
                    else body
      in
        Obj.cons (LAMBDA, Obj.cons (params, body'))
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
          = List.partition isDefinition (Obj.toList body)
      in
        if null defines then
          body
        else
          let
            fun toInit exp =
                let
                  val dvar = definitionVariable exp
                in
                  Obj.fromList [dvar, makeQuote Obj.undef]
                end
            fun toAssign exp =
                let
                  val dvar = definitionVariable exp
                  val dval = definitionValue exp
                in
                  makeAssign (dvar, dval)
                end
            val letParams : obj =
                Obj.fromList (List.map toInit defines)
            val letBody : obj list =
                (List.map toAssign defines) @ nonDefines
            val letForm : obj =
                Obj.fromList (LET :: letParams :: letBody)
          in
            Obj.fromList [letForm]
          end
      end

  and isAssignment exp = isTaggedList SET exp
  and assignmentVariable exp = Obj.cadr exp
  and assignmentValue exp = Obj.caddr exp
  and makeAssign (variable, value) =
      Obj.fromList [SET, variable, value]

  and isDefinition exp = isTaggedList DEFINE exp
  and definitionVariable exp =
      let
        val cadr = Obj.cadr exp
      in
        if Obj.isSym cadr then cadr
        else Obj.car cadr
      end
  and definitionValue exp =
      let
        val cadr = Obj.cadr exp
      in
        if Obj.isSym cadr then
          Obj.caddr exp
        else
          makeLambda (Obj.cdr cadr, (* formal params *)
                      Obj.cddr exp) (* body *)
      end
  and makeDefinition (variable, value) =
      Obj.fromList [DEFINE, variable, value]

  and isIf exp = isTaggedList IF exp
  and ifPredicate exp = Obj.cadr exp
  and ifConsequent exp = Obj.caddr exp
  and ifAlternative exp =
      let
        val cdddr = Obj.cdddr exp
      in
        if Obj.isNull cdddr then makeFalse ()
        else Obj.car cdddr
      end
  and makeIf (pred, con, alt) =
      Obj.fromList [IF, pred, con, alt]

  and isBegin exp = isTaggedList BEGIN exp
  and beginActions exp = Obj.cdr exp
  and makeBegin actions = Obj.cons (BEGIN, actions)

  and isApplication exp = Obj.isCons exp
  and operator exp = Obj.car exp
  and operands exp = Obj.cdr exp
  and makeApplication (operator, operands) =
      Obj.cons (operator, operands)

  (*
   * derived expressions
   *)

  fun seqToExp seq =
      if Obj.isNull seq then seq
      else if (Obj.isNull o Obj.cdr) seq then
        Obj.car seq
      else makeBegin seq

  val isCond = isTaggedList COND
  val expandCond =
      let
        val condClauses = Obj.cdr
        val condPredicate = Obj.car
        val condActions = Obj.cdr
        val condArrowProc = Obj.cadr
        fun isCondElseClause clause =
            Obj.eq (condPredicate clause, ELSE)
        fun isCondArrowClause clause =
            let
              val actions = condActions clause
            in
              if not (Obj.isNull actions) andalso
                 Obj.eq (Obj.car actions, ARROW) then
                if (Obj.isNull o Obj.cdr) actions then
                  raise Obj.Error ("Too few actions -- expandCond: ~S",
                                   [actions])
                else if (not o Obj.isNull o Obj.cddr) actions then
                  raise Obj.Error ("Too many actions -- expandCond: ~S",
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
            if Obj.isNull clauses then
              makeFalse () (* no else clause *)
            else
              let
                val first = Obj.car clauses
                val rest = Obj.cdr clauses
              in
                if isCondElseClause first then
                  if Obj.isNull rest then
                    (seqToExp o condActions) first
                  else
                    raise Obj.Error ("ELSE clause isn't last" ^
                                     " -- expandCond: ~S", [clauses])
                else if isCondArrowClause first then
                  let
                    val test = condPredicate first
                    val actions = condActions first
                    val proc = condArrowProc actions
                    val thunk1 = makeLambda (Obj.null, Obj.fromList [proc])
                    val condForm = Obj.cons (COND, rest)
                    val thunk2 = makeLambda (Obj.null, Obj.fromList [condForm])
                    val opParam1 = Obj.sym "V"
                    val opParam2 = Obj.sym "R1"
                    val opParam3 = Obj.sym "R2"
                    val opParams = Obj.fromList [opParam1, opParam2, opParam3]
                    val appForm1 = makeApplication (opParam2, Obj.null)
                    val appForm2 = makeApplication (opParam3, Obj.null)
                    val ifForm = makeIf (opParam1,
                                         makeApplication (appForm1,
                                                          Obj.fromList
                                                              [opParam1]),
                                         appForm2)
                    val operator = makeLambda (opParams, Obj.fromList [ifForm])
                    val operands = Obj.fromList [test, thunk1, thunk2]
                  in
                    makeApplication (operator, operands)
                  end
                else
                  makeIf (condPredicate first,
                          (seqToExp o condActions) first,
                          (* expandClauses rest) *)
                          Obj.cons (COND, rest))
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
            if Obj.isNull clauses then
              makeTrue ()
            else
              let
                val first = Obj.car clauses
                val rest = Obj.cdr clauses
              in
                if Obj.isNull rest then
                  first
                else
                  let
                    val andForm = Obj.cons (AND, rest)
                    val thunk = makeLambda (Obj.null, Obj.fromList [andForm])
                    val opParam1 = Obj.sym "V"
                    val opParam2 = Obj.sym "R"
                    val opParams = Obj.fromList [opParam1, opParam2]
                    val ifForm = makeIf (opParam1,
                                         makeApplication (opParam2, Obj.null),
                                         makeFalse ())
                    val operator = makeLambda (opParams, Obj.fromList [ifForm])
                    val operands = Obj.fromList [first, thunk]
                  in
                    makeApplication (operator, operands)
                  end
              end
      in
        expandClauses o Obj.cdr
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
            if Obj.isNull clauses then
              makeFalse ()
            else
              let
                val first = Obj.car clauses
                val rest = Obj.cdr clauses

                val orForm = Obj.cons (OR, rest)
                val thunk = makeLambda (Obj.null, Obj.fromList [orForm])
                val opParam1 = Obj.sym "V"
                val opParam2 = Obj.sym "R"
                val opParams = Obj.fromList [opParam1, opParam2]
                val ifForm = makeIf (opParam1,
                                     opParam1,
                                     makeApplication (opParam2, Obj.null))
                val operator = makeLambda (opParams, Obj.fromList [ifForm])
                val operands = Obj.fromList [first, thunk]
              in
                makeApplication (operator, operands)
              end
      in
        expandClauses o Obj.cdr
      end

  (* Exercise 4.6 *)
  val isLet = isTaggedList LET
  val expandLet =
      let
        fun expandClauses clauses =
            let
              val car = Obj.car clauses
            in
              if Obj.isNull car orelse Obj.isCons car then
                expandClausesForOrdinaryLet clauses
              else if Obj.isSym car then
                expandClausesForNamedLet clauses
              else
                raise Obj.Error ("Unexpected form -- expandLet: ~S",
                                 [car, clauses])
            end
        and expandClausesForOrdinaryLet clauses =
            (*
             * (let <params> . <body>)
             * -> ((lambda ,(MAP CAR <params>) ,@<body>)
             *     ,@(MAP CADR <params>))
             *)
            let
              val params = Obj.car clauses
              val body = Obj.cdr clauses

              val operator = makeLambda (Obj.map Obj.car params, body)
              val operands = Obj.map Obj.cadr params
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
              val name = Obj.car clauses
              val params = Obj.cadr clauses
              val body = Obj.cddr clauses

              val letParam = Obj.fromList [name, makeTrue ()]
              val letParams = Obj.fromList [letParam]
              val operator = makeLambda (Obj.map Obj.car params, body)
              val operands = Obj.map Obj.cadr params
              val assignForm = makeAssign (name, operator)
              val appForm = makeApplication (name, operands)
            in
              Obj.fromList [LET, letParams, assignForm, appForm]
            end
      in
        expandClauses o Obj.cdr
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
              val params = Obj.car clauses
              val body = Obj.cdr clauses
            in
              if Obj.isNull params then
                Obj.cons (LET,
                          Obj.cons (params, body))
              else
                let
                  val firstParam = Obj.car params
                  val restParams = Obj.cdr params
                  val letParams = Obj.fromList [firstParam]
                  val let2Form = Obj.cons (LET2, Obj.cons (restParams, body))
                in
                  Obj.fromList [LET, letParams, let2Form]
                end
            end
      in
        expandClauses o Obj.cdr
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
              val params = Obj.car clauses
              val body = Obj.cdr clauses

              fun toInit exp =
                  let
                    val var = Obj.car exp
                  in
                    Obj.fromList [var, makeQuote Obj.undef]
                  end
              fun toAssign exp =
                  let
                    val var = Obj.car exp
                    val value = Obj.cadr exp
                  in
                    makeAssign (var, value)
                  end
              val letParams = Obj.map toInit params
              val letBody = Obj.append (Obj.map toAssign params, body)
            in
              Obj.cons (LET, Obj.cons (letParams, letBody))
            end
      in
        expandClauses o Obj.cdr
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
      else raise Obj.Error ("Unsupported derived expression: ~S", [exp])

  (* for amb evaluator: used in chap4_3.sml *)
  fun isAmb exp = isTaggedList AMB exp
  fun ambChoices exp = Obj.cdr exp
end;

(* 4.1.3  Evaluator Data Structures *)

functor LispObjectFn (structure Env: ENV) :> LISP_OBJECT =
struct
  datatype t = Undef
             | Eof
             | Nil
             | Cons of t ref * t ref
             | Sym of string
             | Bool of bool
             | Num of num
             | Str of string
             | Subr of int * string * proc
             | Expr of int * t * t * t
             | InputPort of int * TextIO.instream
             | OutputPort of int * TextIO.outstream
             | Environment of int * (t, t) Env.t
             | Thunk of int * thunk ref

       and num = Int of int
               | Real of real

       and proc = Proc0 of unit -> t
                | Proc1 of t -> t
                | Proc2 of t * t -> t
                | Proc3 of t * t * t -> t
                | Proc0R of t list -> t
                | Proc1R of t * t list -> t
                | Proc2R of t * t * t list -> t
                | Proc3R of t * t * t * t list -> t

       and thunk = NotEvaluated of t * t
                 | Evaluated of t

  exception Error of string * t list

  val counter = ref 0 (* counter for obj-id *)

  fun inc () =
      let
        val count = !counter
      in
        counter := count + 1;
        count
      end

  val i2r = Real.fromInt

  (* constants *)
  val undef = Undef
  val eof = Eof
  val null = Nil
  val T = Bool true
  val F = Bool false
  val zero = Num (Int 0)
  val one = Num (Int 1)
  val stdIn = InputPort (inc (), TextIO.stdIn)
  val stdOut = OutputPort (inc (), TextIO.stdOut)
  val stdErr = OutputPort (inc (), TextIO.stdErr)

  (* constants for type info *)
  val t_list = Sym "list"
  val t_sym = Sym "symbol"
  val t_bool = Sym "bool"
  val t_num = Sym "num"
  val t_str = Sym "string"
  val t_subr = Sym "subr"
  val t_expr = Sym "expr"
  val t_input_port = Sym "input-port"
  val t_output_port = Sym "output-port"
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

  fun numTypeError r =
      raise Error ("Unexpected num type (expected: int): ~S",
                   [Num (Real r)])

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
  and bool true = T
    | bool false = F
  and int 0 = zero
    | int 1 = one
    | int i = Num (Int i)
  and real r = Num (Real r)
  and str s = Str s
  and subr0 (name, proc) = Subr (inc (), name, Proc0 proc)
  and subr1 (name, proc) = Subr (inc (), name, Proc1 proc)
  and subr2 (name, proc) = Subr (inc (), name, Proc2 proc)
  and subr3 (name, proc) = Subr (inc (), name, Proc3 proc)
  and subr0R (name, proc) = Subr (inc (), name, Proc0R proc)
  and subr1R (name, proc) = Subr (inc (), name, Proc1R proc)
  and subr2R (name, proc) = Subr (inc (), name, Proc2R proc)
  and subr3R (name, proc) = Subr (inc (), name, Proc3R proc)
  and expr (params, body, env) = Expr (inc (), params, body, env)
  and inputPort is = InputPort (inc (), is)
  and outputPort os = OutputPort (inc (), os)
  and environment e = Environment (inc (), e) (* not exported *)
  and thunk (exp, env) = Thunk (inc (), ref (NotEvaluated (exp, env)))

  (* obj <-> list *)
  and fromList nil = Nil
    | fromList (h::t) = cons (h, fromList t)
  and toList (Cons (h, t)) = (!h)::(toList (!t))
    | toList Nil = nil
    | toList obj = typeError (t_list, obj)

  (* predicates for equality tests *)
  fun atomEq (Undef, Undef) = true
    | atomEq (Eof, Eof) = true
    | atomEq (Nil, Nil) = true
    | atomEq (Sym s1, Sym s2) = s1 = s2
    | atomEq (Bool b1, Bool b2) = b1 = b2
    | atomEq (Num (Int i1), Num (Int i2)) = i1 = i2
    | atomEq (Num (Real r1), Num (Real r2)) = Real.== (r1, r2)
    | atomEq (Str s1, Str s2) = s1 = s2
    | atomEq (Expr (id1,_,_,_), Expr (id2,_,_,_)) = id1 = id2
    | atomEq (Subr (id1,_,_), Subr (id2,_,_)) = id1 = id2
    | atomEq (InputPort (id1,_), InputPort (id2,_)) = id1 = id2
    | atomEq (OutputPort (id1,_), OutputPort (id2,_)) = id1 = id2
    | atomEq (Environment (id1,_), Environment (id2,_)) = id1 = id2
    | atomEq (Thunk (id1,_), Thunk (id2,_)) = id1 = id2
    | atomEq _ = false
  fun eq (Cons (h1, t1), Cons (h2, t2)) = h1 = h2 andalso t1 = t2
    | eq (o1, o2) = atomEq (o1, o2)
  fun equal (Cons (h1, t1), Cons (h2, t2)) = equal (!h1, !h2) andalso
                                             equal (!t1, !t2)
    | equal (o1, o2) = atomEq (o1, o2)

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
  fun isNum (Num _) = true
    | isNum _ = false
  fun isStr (Str _) = true
    | isStr _ = false
  fun isSubr (Subr _) = true
    | isSubr _ = false
  fun isExpr (Expr _) = true
    | isExpr _ = false
  fun isInputPort (InputPort _) = true
    | isInputPort _ = false
  fun isOutputPort (OutputPort _) = true
    | isOutputPort _ = false
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
  fun map f seq =
      if isNull seq then null
      else cons (f (car seq), map f (cdr seq))
  fun append (x, y) =
      if isNull x then y
      else cons (car x, append (cdr x, y))

  (* for symbol *)
  fun pname (Sym s) = s
    | pname obj = typeError (t_sym, obj)

  (* for bool *)
  fun toBool (Bool b) = b
    | toBool obj = typeError (t_bool, obj)
  fun not obj =
      if isTrue obj then F else T

  (* for num *)
  fun isInt (Num n) =
      (case n of
         Int _ => true
       | Real _ => false)
    | isInt obj = typeError (t_num, obj)
  fun isReal (Num n) =
      (case n of
         Int _ => false
       | Real _ => true)
    | isReal obj = typeError (t_num, obj)
  fun toInt (Num n) =
      (case n of
         Int i => i
       | Real r => numTypeError r)
    | toInt obj = typeError (t_num, obj)
  fun toReal (Num n) =
      (case n of
         Int i => i2r i
       | Real r => r)
    | toReal obj = typeError (t_num, obj)
  fun negNum (Num n) =
      (case n of
         Int i => int (~ i)
       | Real r => real (~ r))
    | negNum obj = typeError (t_num, obj)
  fun addNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => int (i1 + i2)
       | (Int i1, Real r2) => real ((i2r i1) + r2)
       | (Real r1, Int i2) => real (r1 + (i2r i2))
       | (Real r1, Real r2) => real (r1 + r2))
    | addNum (Num _, obj) = typeError (t_num, obj)
    | addNum (obj, _) = typeError (t_num, obj)
  fun subNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => int (i1 - i2)
       | (Int i1, Real r2) => real ((i2r i1) - r2)
       | (Real r1, Int i2) => real (r1 - (i2r i2))
       | (Real r1, Real r2) => real (r1 - r2))
    | subNum (Num _, obj) = typeError (t_num, obj)
    | subNum (obj, _) = typeError (t_num, obj)
  fun mulNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => int (i1 * i2)
       | (Int i1, Real r2) => real ((i2r i1) * r2)
       | (Real r1, Int i2) => real (r1 * (i2r i2))
       | (Real r1, Real r2) => real (r1 * r2))
    | mulNum (Num _, obj) = typeError (t_num, obj)
    | mulNum (obj, _) = typeError (t_num, obj)
  fun quoNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => int (i1 div i2)
       | (Int i1, Real r2) => real ((i2r i1) / r2)
       | (Real r1, Int i2) => real (r1 / (i2r i2))
       | (Real r1, Real r2) => real (r1 / r2))
    | quoNum (Num _, obj) = typeError (t_num, obj)
    | quoNum (obj, _) = typeError (t_num, obj)
  fun remNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => int (i1 mod i2)
       | (Int _, Real r2) => numTypeError r2
       | (Real r1, _) => numTypeError r1)
    | remNum (Num _, obj) = typeError (t_num, obj)
    | remNum (obj, _) = typeError (t_num, obj)
  fun eqNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => i1 = i2
       | (Int i1, Real r2) => Real.== (i2r i1, r2)
       | (Real r1, Int i2) => Real.== (r1, i2r i2)
       | (Real r1, Real r2) => Real.== (r1, r2))
    | eqNum (Num _, obj) = typeError (t_num, obj)
    | eqNum (obj, _) = typeError (t_num, obj)
  fun gtNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => i1 > i2
       | (Int i1, Real r2) => (i2r i1) > r2
       | (Real r1, Int i2) => r1 > (i2r i2)
       | (Real r1, Real r2) => r1 > r2)
    | gtNum (Num _, obj) = typeError (t_num, obj)
    | gtNum (obj, _) = typeError (t_num, obj)
  fun geNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => i1 >= i2
       | (Int i1, Real r2) => (i2r i1) >= r2
       | (Real r1, Int i2) => r1 >= (i2r i2)
       | (Real r1, Real r2) => r1 >= r2)
    | geNum (Num _, obj) = typeError (t_num, obj)
    | geNum (obj, _) = typeError (t_num, obj)
  fun ltNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => i1 < i2
       | (Int i1, Real r2) => (i2r i1) < r2
       | (Real r1, Int i2) => r1 < (i2r i2)
       | (Real r1, Real r2) => r1 < r2)
    | ltNum (Num _, obj) = typeError (t_num, obj)
    | ltNum (obj, _) = typeError (t_num, obj)
  fun leNum (Num n1, Num n2) =
      (case (n1, n2) of
         (Int i1, Int i2) => i1 <= i2
       | (Int i1, Real r2) => (i2r i1) <= r2
       | (Real r1, Int i2) => r1 <= (i2r i2)
       | (Real r1, Real r2) => r1 <= r2)
    | leNum (Num _, obj) = typeError (t_num, obj)
    | leNum (obj, _) = typeError (t_num, obj)

  (* for string *)
  fun toString (Str s) = s
    | toString obj = typeError (t_str, obj)

  (* for subr (primitive procedure) *)
  fun subrName (Subr (_,name,_)) = name
    | subrName obj = typeError (t_subr, obj)
  fun applySubr (Subr (_,name,proc)) vs =
      (case proc of
         Proc0 p => (case vs of
                       [] => p ()
                     | args => argsError (name, args, 0))
       | Proc1 p => (case vs of
                       [x1] => p x1
                     | args => argsError (name, args, 1))
       | Proc2 p => (case vs of
                       [x1,x2] => p (x1,x2)
                     | args => argsError (name, args, 2))
       | Proc3 p => (case vs of
                       [x1,x2,x3] => p (x1,x2,x3)
                     | args => argsError (name, args, 3))
       | Proc0R p => p vs
       | Proc1R p => (case vs of
                        (x1::xs) => p (x1,xs)
                      | args => argsError (name, args, 1))
       | Proc2R p => (case vs of
                        (x1::x2::xs) => p (x1,x2,xs)
                      | args => argsError (name, args, 2))
       | Proc3R p => (case vs of
                        (x1::x2::x3::xs) => p (x1,x2,x3,xs)
                      | args => argsError (name, args, 3)))
    | applySubr obj _ = typeError (t_subr, obj)

  (* for expr (compound procedure) *)
  fun exprParams (Expr (_,params,_,_)) = params
    | exprParams obj = typeError (t_expr, obj)
  fun exprBody (Expr (_,_,body,_)) = body
    | exprBody obj = typeError (t_expr, obj)
  fun exprEnv (Expr (_,_,_,env)) = env
    | exprEnv obj = typeError (t_expr, obj)

  (* for input/output port *)
  fun toInstream (InputPort (_,is)) = is
    | toInstream obj = typeError (t_input_port, obj)
  fun toOutstream (OutputPort (_,os)) = os
    | toOutstream obj = typeError (t_output_port, obj)
  fun openIn name = (inputPort o TextIO.openIn) name
  fun openOut name = (outputPort o TextIO.openOut) name
  fun closeIn (InputPort (_,is)) = (TextIO.closeIn is; undef)
    | closeIn obj = typeError (t_input_port, obj)
  fun closeOut (OutputPort (_,os)) = (TextIO.closeOut os; undef)
    | closeOut obj = typeError (t_output_port, obj)

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
         NotEvaluated _ => (tref := (Evaluated value); undef)
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
          ((ref (vars, List.map ref vals))::frames, eq)
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
          ((ref (vars, List.map ref vals))::frames, eq)
        else if lenvars < lenvals then
          raise Fail "Too many arguments supplied -- extend"
        else
          raise Fail "Too few arguments supplied -- extend"
      end
end;

(* 4.1.4  Running the Evaluator as a Program *)

signature LISP =
sig
structure Obj : LISP_OBJECT
      and Syntax : LISP_SYNTAX
      and Reader : LISP_READER
      and Printer : LISP_PRINTER
      and Evaluator : LISP_EVALUATOR
sharing type Syntax.obj = Obj.t
    and type Reader.obj = Obj.t
    and type Printer.obj = Obj.t
    and type Evaluator.obj = Obj.t
end;

signature LISP_RUNTIME =
sig
  include LISP
  type rt
  val env : rt -> Obj.t
  val stdIn : rt -> Obj.t
  val stdOut : rt -> Obj.t
  val stdErr : rt -> Obj.t
  val setStdIn : rt -> Obj.t -> unit
  val setStdOut : rt -> Obj.t -> unit
  val setStdErr : rt -> Obj.t -> unit
  val makeRuntime : unit -> rt
end;

signature INTERPRETER =
sig
  (* activates new top-level *)
  val go : unit -> unit
end;

signature TEST =
sig
  (* run the specified unit tests *)
  val doTest : (string * string) list -> unit
  (* run predefined unit tests *)
  val test : unit -> unit
end;

functor LispRuntimeFn (Lisp : LISP) : LISP_RUNTIME =
struct
  open Lisp

  type rt = {Env : Obj.t,
             StdIn : Obj.t ref,
             StdOut : Obj.t ref,
             StdErr : Obj.t ref}

  fun env ({Env,...}:rt) = Env
  fun stdIn ({StdIn,...}:rt) = !StdIn
  fun stdOut ({StdOut,...}:rt) = !StdOut
  fun stdErr ({StdErr,...}:rt) = !StdErr
  fun setStdIn ({StdIn,...}:rt) newStdIn = StdIn := newStdIn
  fun setStdOut ({StdOut,...}:rt) newStdOut = StdOut := newStdOut
  fun setStdErr ({StdErr,...}:rt) newStdErr = StdErr := newStdErr

  val subrs =
      [Obj.subr2 ("cons", Obj.cons),
       Obj.subr1 ("car", Obj.car),
       Obj.subr1 ("cdr", Obj.cdr),
       Obj.subr1 ("caar", Obj.caar),
       Obj.subr1 ("cadr", Obj.cadr),
       Obj.subr1 ("cdar", Obj.cdar),
       Obj.subr1 ("cddr", Obj.cddr),
       Obj.subr1 ("caaar", Obj.caaar),
       Obj.subr1 ("caadr", Obj.caadr),
       Obj.subr1 ("cadar", Obj.cadar),
       Obj.subr1 ("cdaar", Obj.cdaar),
       Obj.subr1 ("caddr", Obj.caddr),
       Obj.subr1 ("cdadr", Obj.cdadr),
       Obj.subr1 ("cddar", Obj.cddar),
       Obj.subr1 ("cdddr", Obj.cdddr),
       Obj.subr2 ("set-car!",
                  (fn (lst,obj) => Obj.setCar lst obj)),
       Obj.subr2 ("set-cdr!",
                  (fn (lst,obj) => Obj.setCdr lst obj)),
       Obj.subr0R ("list", Obj.fromList),
       Obj.subr1 ("length",
                  (fn lst =>
                      let
                        fun len l =
                            if Obj.isNull l then 0
                            else 1 + (len (Obj.cdr l))
                      in
                        Obj.int (len lst)
                      end)),
       Obj.subr2 ("list-ref",
                  (fn (lst,n) =>
                      let
                        fun error () =
                            raise Obj.Error
                                      ("Subscript out of bounds", nil)
                        fun iter (l, i) =
                            if Obj.isNull l then
                              error ()
                            else
                              if i = 0 then
                                Obj.car l
                              else
                                iter (Obj.cdr l, i - 1)
                        val i = Obj.toInt n
                      in
                        if i < 0 then error ()
                        else iter (lst, i)
                      end)),
       Obj.subr2 ("assq",
                  (fn (key,lst) =>
                      let
                        fun iter l =
                            if Obj.isNull l then
                              Obj.F
                            else
                              let
                                val p = Obj.car l
                                val k = Obj.car p
                              in
                                if Obj.eq (key, k) then p
                                else iter (Obj.cdr l)
                              end
                      in
                        iter lst
                      end)),
       Obj.subr2 ("assoc",
                  (fn (key,lst) =>
                      let
                        fun iter l =
                            if Obj.isNull l then
                              Obj.F
                            else
                              let
                                val p = Obj.car l
                                val k = Obj.car p
                              in
                                if Obj.equal (key, k) then p
                                else iter (Obj.cdr l)
                              end
                      in
                        iter lst
                      end)),
       Obj.subr2 ("memq",
                  (fn (key,lst) =>
                      let
                        fun iter l =
                            if Obj.isNull l then
                              Obj.F
                            else
                              if Obj.eq (Obj.car l, key) then l
                              else iter (Obj.cdr l)
                      in
                        iter lst
                      end)),
       Obj.subr2 ("member",
                  (fn (key,lst) =>
                      let
                        fun iter l =
                            if Obj.isNull l then
                              Obj.F
                            else
                              if Obj.equal (Obj.car l, key) then l
                              else iter (Obj.cdr l)
                      in
                        iter lst
                      end)),
       Obj.subr2R ("map",
                   (fn (p,lst,lsts) =>
                       let
                         fun isEnd nil = false
                           | isEnd (l::ll) =
                             if Obj.isNull l then true
                             else isEnd ll
                         fun first nil = nil
                           | first (l::ll) = (Obj.car l)::(first ll)
                         fun rest nil = nil
                           | rest (l::ll) = (Obj.cdr l)::(rest ll)
                         fun map ll =
                             if isEnd ll then Obj.null
                             else
                               Obj.cons (Evaluator.apply p (first ll),
                                         map (rest ll))
                       in
                         map (lst::lsts)
                       end)),
       Obj.subr2 ("eq?", Obj.bool o Obj.eq),
       Obj.subr2 ("equal?", Obj.bool o Obj.equal),
       Obj.subr1 ("null?", Obj.bool o Obj.isNull),
       Obj.subr1 ("true?", Obj.bool o Obj.isTrue),
       Obj.subr1 ("false?", Obj.bool o Obj.isFalse),
       Obj.subr1 ("pair?", Obj.bool o Obj.isCons),
       Obj.subr1 ("symbol?", Obj.bool o Obj.isSym),
       Obj.subr1 ("bool?", Obj.bool o Obj.isBool),
       Obj.subr1 ("number?", Obj.bool o Obj.isNum),
       Obj.subr1 ("string?", Obj.bool o Obj.isStr),
       Obj.subr1 ("subr?", Obj.bool o Obj.isSubr),
       Obj.subr1 ("expr?", Obj.bool o Obj.isExpr),
       Obj.subr1 ("input-port?", Obj.bool o Obj.isInputPort),
       Obj.subr1 ("output-port?", Obj.bool o Obj.isOutputPort),
       Obj.subr0R ("+",
                   (fn ns =>
                       let
                         fun f (a, b) = Obj.addNum (b, a)
                       in
                         (*
                          * foldl f i [i0,i1,...,iN]; where f(a,b) = b+a
                          * = f(iN,...,f(i1,f(i0,i)))
                          * = (((i+i0)+i1)+...+iN)
                          *)
                         List.foldl f Obj.zero ns
                       end)),
       Obj.subr0R ("-",
                   (fn nil => Obj.zero
                     | (n::nil) => Obj.negNum n
                     | (n::ns) =>
                       let
                         fun f (a, b) = Obj.subNum (b, a)
                       in
                         (*
                          * foldl f i [i0,i1,...,iN]; where f(a,b) = b-a
                          * = f(iN,...,f(i1,f(i0,i)))
                          * = (((i-i0)-i1)-...-iN)
                          *)
                         List.foldl f n ns
                       end)),
       Obj.subr0R ("*",
                   (fn ns =>
                       let
                         fun f (a, b) = Obj.mulNum (b, a)
                       in
                         (*
                          * foldl f i [i0,i1,...,iN]; where f(a,b) = b*a
                          * = f(iN,...,f(i1,f(i0,i)))
                          * = (((i*i0)*i1)*...*iN)
                          *)
                         List.foldl f Obj.one ns
                       end)),
       Obj.subr2 ("/", Obj.quoNum),
       Obj.subr2 ("%", Obj.remNum),
       Obj.subr2 ("=", Obj.bool o Obj.eqNum),
       Obj.subr2 (">", Obj.bool o Obj.gtNum),
       Obj.subr2 ("<", Obj.bool o Obj.ltNum),
       Obj.subr2 (">=", Obj.bool o Obj.geNum),
       Obj.subr2 ("<=", Obj.bool o Obj.leNum),
       Obj.subr1 ("abs",
                  (fn n => if Obj.isInt n then
                             (Obj.int o Int.abs o Obj.toInt) n
                           else if Obj.isReal n then
                             (Obj.real o Real.abs o Obj.toReal) n
                           else
                             raise Obj.Error ("Not a number: ~S", [n]))),
       Obj.subr1 ("not", Obj.not),
       Obj.subr1 ("open-input-file", Obj.openIn o Obj.toString),
       Obj.subr1 ("open-output-file", Obj.openOut o Obj.toString),
       Obj.subr1 ("close-input-port", Obj.closeIn),
       Obj.subr1 ("close-output-port", Obj.closeOut),
       Obj.subr2 ("eval",
                  (fn (exp, env) =>
                      Evaluator.eval exp env)),
       Obj.subr2 ("apply",
                  (fn (proc,args) =>
                      Evaluator.apply proc (Obj.toList args))),
       Obj.subr1 ("expand-syntax",
                  (fn exp =>
                      if Syntax.isDerived exp then
                        Syntax.expandDerived exp
                      else
                        exp)),
       Obj.subr1R ("error",
                   (fn (fmt,args) =>
                       raise Obj.Error (Obj.toString fmt, args)))]

  fun makeRuntime () =
      let
        val subrName = Obj.sym o Obj.subrName
        val env = Obj.extendEnv (Obj.newEnv ())
                                (List.map subrName subrs, subrs)
        val stdInRef = ref Obj.stdIn
        val stdOutRef = ref Obj.stdOut
        val stdErrRef = ref Obj.stdErr
        val subrs' = [
            Obj.subr0 ("user-init-env",
                       (fn () => env)),
            Obj.subr0 ("current-input-port",
                       (fn () => !stdInRef)),
            Obj.subr0 ("current-output-port",
                       (fn () => !stdOutRef)),
            Obj.subr0 ("current-error-port",
                       (fn () => !stdErrRef)),
            Obj.subr0 ("read",
                       (fn () => Reader.read (!stdInRef))),
            Obj.subr1 ("write",
                       (fn obj => Printer.print (!stdOutRef, obj))),
            Obj.subr1 ("print-string",
                       (fn obj => Printer.printString (!stdOutRef,
                                                       Obj.toString obj))),
            Obj.subr0 ("newline",
                       (fn () => Printer.terpri (!stdOutRef))),
            Obj.subr0 ("flush",
                       (fn () => Printer.flush (!stdOutRef))),
            Obj.subr1R ("format",
                        (fn (fmt,args) => Printer.format (!stdOutRef,
                                                          Obj.toString fmt,
                                                          args)))]
      in
        Obj.defineEnv env (Syntax.TRUE, Obj.T);
        Obj.defineEnv env (Syntax.FALSE, Obj.F);
        List.app (fn subr =>
                     ignore (Obj.defineEnv env (subrName subr, subr)))
                 subrs';
        {Env = env, StdIn = stdInRef,
         StdOut = stdOutRef, StdErr = stdErrRef}
      end
end;

functor LispInterpreterFn (Runtime : LISP_RUNTIME)
        : sig include INTERPRETER TEST end =
struct
  open Runtime

  exception Test

  val quit = Obj.sym ":q"

  fun makeRuntime () =
      let
        val rt = Runtime.makeRuntime ()
        (* load *)
        val fnLoad = (fn file => (load (rt, Obj.toString file); Obj.undef))
        val subrLoad = Obj.subr1 ("load", fnLoad)
        val symLoad = Obj.sym (Obj.subrName subrLoad)
        val _ = Obj.defineEnv (env rt) (symLoad, subrLoad)
      in
        rt
      end

  and hello rt =
      ignore (Printer.format (stdOut rt,
                              "Hello!~%"^
                              "Type '~S' to exit~%",
                              [quit]))

  and bye rt =
      ignore (Printer.format (stdOut rt,
                              "Bye!~%",
                              nil))

  and repl (rt, prompt) =
      let
        fun loop () =
            let
              val obj = (Printer.format (stdOut rt, prompt, nil);
                         Reader.read (stdIn rt))
            in
              if Obj.isEof obj orelse Obj.eq (obj, quit) then
                ()
              else
                let
                  val obj' = Evaluator.eval obj (env rt)
                in
                  Printer.print (stdOut rt, obj');
                  loop ()
                end
            end
            handle e => (Printer.printException (stdErr rt, e);
                         loop ())
      in
        loop ()
      end

  and load (rt, file) =
      let
        val oldIn = stdIn rt
        fun body () =
            let
              val newIn = Obj.openIn file
              fun body' () = (setStdIn rt newIn; repl (rt,"~%"))
              fun cleanup' () = ignore (Obj.closeIn newIn)
            in
              U.unwindProtect body' cleanup'
            end
        fun cleanup () = setStdIn rt oldIn
      in
        U.unwindProtect body cleanup
      end

  and go () =
      let
        val rt = makeRuntime ()
      in
        (hello rt; repl (rt,"~%> "); bye rt)
      end

  and ut rt =
      let
        val counter = ref 1
        fun inc () = let val n = !counter in counter := n+1; n end
        val s2i = Obj.inputPort o TextIO.openString
      in
        (fn (input, expected) =>
            let
              val n = Obj.int (inc ())
              val is = s2i input
              val is' = s2i expected
              val obj = Reader.read is
              val obj' = Reader.read is'
              val ret = Evaluator.eval obj (env rt)
              val ret' = Evaluator.eval obj' (env rt)
            in
              if Obj.equal (ret, ret') then
                (Printer.format (stdOut rt,
                                 "[~S] OK: ~S -> ~S~%",
                                 [n, obj, ret]);
                 ())
              else
                (Printer.format (stdOut rt,
                                 "[~S] NG: ~S -> ~S; (expected: ~S)~%",
                                 [n, obj, ret, ret']);
                 raise Test)
            end)
      end

  and doTest tests =
      let
        val rt = makeRuntime ()
        val ut = ut rt
      in
        List.app ut tests
        handle e => ignore (Printer.printException (stdErr rt, e))
      end

  and test () =
      let
        val tests =
            [("(true? true)", "true"),
             ("(true? 1)", "true"),
             ("(true? '())", "true"),
             ("(true? false)", "false"),
             ("(false? true)", "false"),
             ("(false? 1)", "false"),
             ("(false? '())", "false"),
             ("(false? false)", "true"),
             ("(null? true)", "false"),
             ("(null? 1)", "false"),
             ("(null? '())", "true"),
             ("(null? false)", "false"),
             ("(+)", "0"),
             ("(+ 1)", "1"),
             ("(+ 1 2)", "3"),
             ("(+ 1 2 3)", "6"),
             ("(-)", "0"),
             ("(- 1)", "~1"),
             ("(- 1 2)", "~1"),
             ("(- 1 2 3)", "~4"),
             ("(*)", "1"),
             ("(* 2)", "2"),
             ("(* 2 3)", "6"),
             ("(* 2 3 4)", "24"),
             ("(/ 7 3)", "2"),
             ("(% 7 3)", "1"),
             ("(< 1 2)", "true"),
             ("(< 1 1)", "false"),
             ("(< 2 1)", "false"),
             ("(<= 1 2)", "true"),
             ("(<= 1 1)", "true"),
             ("(<= 2 1)", "false"),
             ("(> 1 2)", "false"),
             ("(> 1 1)", "false"),
             ("(> 2 1)", "true"),
             ("(>= 1 2)", "false"),
             ("(>= 1 1)", "true"),
             ("(>= 2 1)", "true"),
             ("(if (> 2 1) 'a 'b)", "'a"),
             ("(if (> 1 1) 'a 'b)", "'b"),
             ("(cond ((> 2 1) 'a) (else 'b))", "'a"),
             ("(cond ((> 1 1) 'a) (else 'b))", "'b"),
             ("(cond ((assoc 'b '((a 1) (b 2))) => cadr)" ^
              "      (else false))", "2"),
             ("(cond ((assoc 'c '((a 1) (b 2))) => cadr)" ^
              "      ((assoc 'a '((a 1) (b 2))) => cadr)" ^
              "      (else false))", "1"),
             ("(and)", "true"),
             ("(and 1)", "1"),
             ("(and false (error \"oops!\"))", "false"),
             ("(and 1 false)", "false"),
             ("(and 1 true)", "true"),
             ("(and true 1)", "1"),
             ("(or)", "false"),
             ("(or 1)", "1"),
             ("(or false false)", "false"),
             ("(or false 1)", "1"),
             ("(or 1 (error \"oops!\"))", "1"),
             ("(or true (error \"oops!\"))", "true"),
             ("(let () 1)", "1"),
             ("(let ((x 1) (y 2)) (+ x y))", "3"),
             ("(begin (define x 1) x)", "1"),
             ("x", "1"),
             ("(begin (set! x 2) x)", "2"),
             ("x", "2"),
             ("(let ((x x)) (set! x 3) x)", "3"),
             ("x", "2"),
             ("(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))", "39"),
             ("(let loop ((a 1) (b 0) (count 10))" ^
              " (if (= count 0) b (loop (+ a b) a (- count 1))))",
              "55"), (* fib 10 -> 55 *)
             ("(length '())", "0"),
             ("(length '(1 2 3))", "3"),
             ("(list-ref '(1 2 3 4) 2)", "3"),
             ("(assoc 'b '((a . 1) (b . 2)))", "'(b . 2)"),
             ("(assoc 'c '((a . 1) (b . 2)))", "false"),
             ("(member 'b '(a b c))", "'(b c)"),
             ("(member 'd '(a b c))", "false"),
             ("(map (lambda (x) (+ x x)) '(1 2 3))", "'(2 4 6)"),
             ("(map (lambda (x y) (+ x y)) '(1 2) '(3 2 1 0))", "'(4 4)"),
             ("((lambda (x)"^
              "   (define (even? n)"^
              "     (if (= n 0) true (odd? (- n 1))))"^
              "   (define (odd? n)"^
              "     (if (= n 0) false (even? (- n 1))))"^
              "   (list (odd? x) (even? x))) 3)",
              "(list true false)"),
             ("((lambda (x)"^
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
              "(list true false)"),
             ("((lambda (n)"^
              "   ((lambda (fact)"^
              "      (fact fact n))"^
              "    (lambda (ft k)"^
              "      (if (= k 1)"^
              "          1"^
              "          (* k (ft ft (- k 1)))))))"^
              " 5)",
              "120")]
      in
        doTest tests
      end
end;

local
  structure Lisp : LISP =
  struct
    structure Obj
      = LispObjectFn (structure Env = Env)
    (*
      = LispObjectFn (structure Env = Env') (* Exercise 4.11 *)
      = LispObjectFn (structure Env = Env'') (* Exercise 4.12 *)
     *)
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
structure LI = LispInterpreterFn (Runtime)
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

functor AnalyzingEvaluatorFn (structure Obj: LISP_OBJECT
                              and Syntax: LISP_SYNTAX
                              sharing type Syntax.obj = Obj.t)
        : LISP_EVALUATOR =
struct
  type obj = Obj.t

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
        raise Obj.Error ("Unknown expression type -- analyze: ~S",
                          [exp])

  and analyzeSelfEvaluating exp =
      (fn _ => exp)

  and analyzeVariable exp =
      (fn env => Obj.lookupEnv env exp)

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
        (fn env => (Obj.setEnv env (var, vproc env); var))
      end

  and analyzeDefinition exp =
      let
        val var = Syntax.definitionVariable exp
        val vproc = analyze (Syntax.definitionValue exp)
      in
        (fn env => (Obj.defineEnv env (var, vproc env); var))
      end

  and analyzeIf exp =
      let
        val pproc = analyze (Syntax.ifPredicate exp)
        val cproc = analyze (Syntax.ifConsequent exp)
        val aproc = analyze (Syntax.ifAlternative exp)
      in
        (fn env =>
            if Obj.isTrue (pproc env) then cproc env
            else aproc env)
      end

  and analyzeLambda exp =
      let
        val vars = Syntax.lambdaParameters exp
        val proc = analyzeSequence (Syntax.lambdaBody exp)
        val body = toBody proc
      in
        (fn env => Obj.expr (vars, body, env))
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
          nil => raise Obj.Error ("Empty sequence -- analyzeSequence",
                                   nil)
        | (p::ps) => loop (p, ps)
      end

  and analyzeApplication exp =
      let
        val fproc = analyze (Syntax.operator exp)
        val aprocs = toProcs (Syntax.operands exp)
      in
        (fn env => executeApplication (fproc env)
                                      (List.map (fn p => p env) aprocs))
      end

  and executeApplication proc args =
      if Obj.isSubr proc then
        Obj.applySubr proc args
      else if Obj.isExpr proc then
        let
          (* params *)
          val params = Obj.toList (Obj.exprParams proc)
          (* body: represented as procedure (env -> obj) *)
          val body = Obj.exprBody proc
          (* env: environment to which body is applied *)
          val env = Obj.extendEnv (Obj.exprEnv proc) (params, args)
        in
          Obj.applySubr body [env]
        end
      else
        raise Obj.Error ("Not a procedure -- executeApplication: ~S",
                          [proc])

  and toBody p = Obj.subr1 ("body", p)

  and toProcs exps =
      if Obj.isNull exps then
        nil
      else if Obj.isCons exps then
        (analyze (Obj.car exps)) :: (toProcs (Obj.cdr exps))
      else
        raise Obj.Error ("Improper sequence: ~S", [exps])

  (* declared in LISP_EVALUATOR signature *)
  val eval = analyze

  (* declared in LISP_EVALUATOR signature *)
  val apply = executeApplication
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
      = AnalyzingEvaluatorFn (structure Obj = Obj and Syntax = Syntax)
  end
  structure Runtime = LispRuntimeFn (Lisp)
in
structure LI' = LispInterpreterFn (Runtime)
end;

(*
 * LI'.go (); (* => activates top-level *)
 * LI'.test (); (* => executes predefined unit tests *)
 *)
