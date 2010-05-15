(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml, chap1_2.sml, chap4_1.sml;
 *
 * Notice: the following code requires the lazy evaluation,
 * which is a non-standard feature of ML provided by SML/NJ.
 * The lazy evaluation features must be enabled by executing
 * the following at the top level:
 *  Control.lazysml := true;
 *
 * For more information, please see the following:
 *  Chapter 15 (Lazy Data Structures)
 *  of
 *  Programming in Standard ML
 *  (WORKING DRAFT OF AUGUST 20, 2009.)
 *  Robert Harper
 *  Carnegie Mellon University
 *  Spring Semester, 2005
 *  -> available online: http://www.cs.cmu.edu/~rwh/smlbook/
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 4  Metalinguistic Abstraction *)
(* 4.4  Logic Programming *)
(* 4.4.1  Deductive Information Retrieval *)
(* 4.4.2  How the Query System Works *)
(* 4.4.3  Is Logic Programming Mathematical Logic? *)
(* 4.4.4  Implementing the Query System *)

(*
 * helper modules for query language
 *)

structure Stream =
struct
  open Lazy;

  datatype lazy 'a t = Nil | Cons of 'a * 'a t

  fun nth n =
      if n < 0 then raise Subscript
      else
        let
          fun f 0 (Cons (x, s)) = x
            | f k (Cons (_, s)) = f (k-1) s
            | f _ Nil = raise Subscript
        in
          f n
        end

  fun take n =
      if n < 0 then raise Subscript
      else
        let
          fun f 0 _ = nil
            | f k (Cons (x, s)) = x :: f (k-1) s
            | f _ Nil = raise Subscript
        in
          f n
        end

  fun map proc =
      let
        fun lazy f (Cons (x, s)) = (U.log "map";Cons (proc x, f s))
          | f Nil = Nil
      in
        f
      end

  fun app proc =
      let
        fun f (Cons (x, s)) = (U.log "app"; proc x; f s)
          | f Nil = ()
      in
        f
      end

  fun filter pred =
      let
        fun lazy f (Cons (x, s)) =
            if pred x then Cons (x, f s)
            else f s
          | f Nil = Nil
      in
        f
      end

  fun append (s1, s2) =
      let
        fun lazy f (Cons (x1, s1), s2) =
            (U.log "append"; Cons (x1, f (s1, s2)))
          | f (Nil, s2) = s2
      in
        f (s1, s2)
      end

  fun interleave (s1, s2) =
      let
        fun lazy f (Cons (x1, s1), s2) =
            (U.log "interleave"; Cons (x1, f (s2, s1)))
          | f (Nil, s2) = s2
      in
        f (s1, s2)
      end

  fun flatten s =
      let
        fun lazy f (Cons (x, s)) = interleave (x, f s)
          | f Nil = Nil
      in
        f s
      end

  fun flatmap proc s = flatten (map proc s)

  fun singleton x = Cons (x, Nil)
end;
(*
structure S = Stream;
val rec lazy ones = S.Cons (1, ones);
val rec lazy twos = S.Cons (2, twos);
val rec lazy x = S.interleave (ones, twos);
*)

structure Frame :>
sig
  type ('a,'b) t
  val make : ('a * 'a -> bool) -> ('a,'b) t
  val bindingIn : ('a,'b) t -> 'a -> ('a * 'b) option
  val extend : ('a,'b) t -> ('a * 'b) ->  ('a,'b) t
end =
struct
  type ('a,'b) t = ('a * 'b) list * ('a * 'a -> bool)

  fun make eq = (nil, eq)

  fun bindingIn (nil, _) _ = NONE
    | bindingIn ((bind as (var', _))::binds, eq) var =
      if eq (var', var) then SOME bind
      else bindingIn (binds, eq) var

  fun extend (binds, eq) bind = (bind::binds, eq)
end;

structure Table :>
sig
  type ('a,'b) t
  val make : ('a * 'a -> bool) -> ('a,'b) t
  val lookup : ('a,'b) t -> 'a -> 'b option
  val insert : ('a,'b) t -> 'a -> 'b -> unit
end =
struct
  type ('a,'b) t = ('a * 'b ref) list ref * ('a * 'a -> bool)

  fun make eq = (ref nil, eq)

  fun assoc key nil _ = NONE
    | assoc key ((p as (key', _))::ps) eq =
      if eq (key, key') then SOME p
      else assoc key ps eq

  fun lookup (table, eq) key =
      case assoc key (!table) eq of
        SOME (_, vref) => SOME (!vref)
      | NONE => NONE

  fun insert (table, eq) key newValue =
      case assoc key (!table) eq of
        SOME (_, vref) =>
        (vref := newValue;
         U.log "vref updated!")
      | NONE =>
        (table := (key, ref newValue) :: (!table);
         U.log "vref inserted!")
end;

(*
 * signatures for query language
 *)

signature QUERY =
sig
  type obj
  type prt
  type qrt =
       {PRED_RUNTIME : prt,
        THE_ASSERTIONS : obj Stream.t ref,
        THE_RULES : obj Stream.t ref,
        THE_TABLE : ((obj * obj), obj Stream.t) Table.t}

  (* predefined symbols *)
  val ALWAYS_TRUE : obj
  val ASSERT : obj
  val ASSERTION_STREAM : obj
  val CONJUNCT : obj
  val DISJUNCT : obj
  val NEGATE : obj
  val PREDICATE : obj
  val QUESTION : obj
  val RULE : obj
  val RULE_STREAM : obj

  (* null streams *)
  val NULL_OBJ_STREAM : obj Stream.t
  val NULL_FRAME_STREAM : (obj,obj) Frame.t Stream.t

  (* for composite expressions *)
  val typeOf : obj -> obj
  val contentsOf : obj -> obj

  (* for add-assertion *)
  val isAssertionToBeAdded : obj -> bool
  val addAssertionBody : obj -> obj

  (* for conjunction (and) *)
  val isConjunction : obj -> bool
  val conjunctionBody : obj -> obj
  val isEmptyConjunction : obj -> bool
  val firstConjunct : obj -> obj
  val restConjuncts : obj -> obj

  (* for disjunction (or) *)
  val isDisjunction : obj -> bool
  val disjunctionBody : obj -> obj
  val isEmptyDisjunction : obj -> bool
  val firstDisjunct : obj -> obj
  val restDisjuncts : obj -> obj

  (* for negation (not) *)
  val isNegation : obj -> bool
  val negatedQuery : obj -> obj

  (* for predicate (lisp-value) *)
  val isPredicate : obj -> bool
  val predicateBody : obj -> obj
  val predicate : obj -> obj
  val args : obj -> obj

  (* for always-true *)
  val isAlwaysTrue : obj -> bool

  (* for rule *)
  val isRule : obj -> bool
  val conclusion : obj -> obj
  val ruleBody : obj -> obj

  (* for variables and constant-symbols *)
  val querySyntaxProcess : obj -> obj
  val isVar : obj -> bool
  val isConstantSymbol : obj -> bool
  val contractQuestionMark : obj -> obj

  (* for instantiation *)
  val instantiate : obj -> (obj,obj) Frame.t
                    -> (obj -> (obj,obj) Frame.t -> obj)
                    -> obj

  (* for pattern matching and unification *)
  val patternMatch : obj -> obj
                     -> (obj,obj) Frame.t option
                     -> (obj,obj) Frame.t option
  val renameVariablesIn : obj -> obj
  val unifyMatch : obj -> obj
                   -> (obj,obj) Frame.t option
                   -> (obj,obj) Frame.t option

  (* for others *)
  val makeRuntime : unit -> qrt
  val makeNewFrame : unit -> (obj,obj) Frame.t
  val executePredicate : prt -> obj -> bool
  val error : string * obj list -> 'a
  val log : string * obj list -> unit
  val debug : bool ref
end;

signature QUERY_DATA_BASE =
sig
  structure Q : QUERY

  val addRuleOrAssertion : Q.qrt -> Q.obj -> unit
  val fetchAssertions : Q.qrt
                        -> Q.obj * (Q.obj,Q.obj) Frame.t
                        -> Q.obj Stream.t
  val fetchRules : Q.qrt
                   -> Q.obj * (Q.obj,Q.obj) Frame.t
                   -> Q.obj Stream.t
end;

signature QUERY_EVALUATOR =
sig
  structure Q : QUERY
  structure QDB : QUERY_DATA_BASE

  val eval : Q.qrt -> Q.obj
             -> (Q.obj,Q.obj) Frame.t Stream.t
             -> (Q.obj,Q.obj) Frame.t Stream.t
end;

(*
 * implementations for query language
 *)

functor QueryFn (LispRuntime : LISP_RUNTIME) : QUERY =
struct
  structure Obj = LispRuntime.Obj
  structure Evaluator = LispRuntime.Evaluator
  structure Printer = LispRuntime.Printer

  type obj = Obj.t
  type prt = LispRuntime.rt
  type qrt =
       {PRED_RUNTIME : prt,
        THE_ASSERTIONS : obj Stream.t ref,
        THE_RULES : obj Stream.t ref,
        THE_TABLE : ((obj * obj), obj Stream.t) Table.t}

  val ALWAYS_TRUE = Obj.sym "always-true"
  val ASSERT = Obj.sym "assert!"
  val ASSERTION_STREAM = Obj.sym "assertion-stream"
  val CONJUNCT = Obj.sym "and"
  val DISJUNCT = Obj.sym "or"
  val NEGATE = Obj.sym "not"
  val PREDICATE = Obj.sym "lisp-value"
  val QUESTION = Obj.sym "?"
  val RULE = Obj.sym "rule"
  val RULE_STREAM = Obj.sym "rule-stream"

  val NULL_OBJ_STREAM : obj Stream.t = Stream.Nil
  val NULL_FRAME_STREAM : (obj,obj) Frame.t Stream.t = Stream.Nil

  val debug = ref false

  fun error (ctrlstr, args) = raise Obj.Error (ctrlstr, args)

  fun log (ctrlstr, args) =
      if !debug then
        let
          val msg = "DEBUG: "^ctrlstr^"~%"
        in
          ignore (Printer.format (Obj.stdErr, msg, args))
        end
      else
        ()

  fun isTaggedList tag exp =
      Obj.isCons exp andalso
      Obj.eq (Obj.car exp, tag)

  fun typeOf exp =
      if Obj.isCons exp then Obj.car exp
      else error ("Unknown expression: ~S", [exp])

  fun contentsOf exp =
      if Obj.isCons exp then Obj.cdr exp
      else error ("Unknown expression: ~S", [exp])

  (*fun isAssertionToBeAdded exp = Obj.eq (typeOf exp, ASSERT)*)
  fun isAssertionToBeAdded exp = isTaggedList ASSERT exp
  fun addAssertionBody exp = (Obj.car o contentsOf) exp

  fun isConjunction exp = isTaggedList CONJUNCT exp
  fun conjunctionBody exp = contentsOf exp
  fun isEmptyConjunction exps = Obj.isNull exps
  fun firstConjunct exps = Obj.car exps
  fun restConjuncts exps = Obj.cdr exps

  fun isDisjunction exp = isTaggedList DISJUNCT exp
  fun disjunctionBody exp = contentsOf exp
  fun isEmptyDisjunction exps = Obj.isNull exps
  fun firstDisjunct exps = Obj.car exps
  fun restDisjuncts exps = Obj.cdr exps

  fun isNegation exp = isTaggedList NEGATE exp
  (*fun negatedQuery exps = Obj.car exps*)
  fun negatedQuery exp = (Obj.car o contentsOf) exp

  fun isPredicate exp = isTaggedList PREDICATE exp
  fun predicateBody exp = contentsOf exp
  fun predicate exps = Obj.car exps
  fun args exps = Obj.cdr exps

  fun isAlwaysTrue exp = isTaggedList ALWAYS_TRUE exp

  fun isRule exp = isTaggedList RULE exp
  fun conclusion rule = Obj.cadr rule
  fun ruleBody rule =
      if Obj.isNull (Obj.cddr rule) then
        Obj.fromList [ALWAYS_TRUE]
      else Obj.caddr rule

  fun querySyntaxProcess exp =
      let
        fun mapOverSymbols proc exp =
            if Obj.isCons exp then
              Obj.cons (mapOverSymbols proc (Obj.car exp),
                        mapOverSymbols proc (Obj.cdr exp))
            else if Obj.isSym exp then
              proc exp
            else
              exp
        fun expandQuestionMark sym =
            let
              val str = Obj.pname sym
            in
              log ("expandQuestionMark: ~S", [sym]);
              if String.isPrefix "?" str then
                Obj.fromList [QUESTION,
                              Obj.sym (String.extract (str,1,NONE))]
              else sym
            end
      in
        mapOverSymbols expandQuestionMark exp
      end

  fun isVar exp = isTaggedList QUESTION exp
  fun isConstantSymbol exp = Obj.isSym exp

  fun contractQuestionMark var =
      let
        val str = "?" ^
                  (if Obj.isNum (Obj.cadr var) then
                     (Obj.pname o Obj.caddr) var ^
                     "-" ^
                     (Int.toString o Obj.toInt o Obj.cadr) var
                   else
                     (Obj.pname o Obj.cadr) var)
      in
        Obj.sym str
      end

  fun instantiate exp frame unboundVarHandler =
      let
        fun copy exp =
            if isVar exp then
              case Frame.bindingIn frame exp of
                SOME (_, dat) => copy dat
              | NONE => unboundVarHandler exp frame
            else if Obj.isCons exp then
              Obj.cons (copy (Obj.car exp), copy (Obj.cdr exp))
            else
              exp
      in
        copy exp
      end

  fun patternMatch pat dat frameOpt =
      (log ("patternMatch: ~S ~S", [pat,dat]);
      case frameOpt of
        NONE => (log ("patternMatch: frameOpt is NONE",nil); NONE)
      | SOME frame =>
        if Obj.equal (pat, dat) then
          (log ("patternMatch: pat = dat",nil); SOME frame)
        else if isVar pat then
          (log ("patternMatch: pat is var",nil);
           extendIfConsistent pat dat frame)
        else if Obj.isCons pat andalso Obj.isCons dat then
          (log ("patternMatch: both pat and dat is cons",nil);
          patternMatch (Obj.cdr pat)
                       (Obj.cdr dat)
                       (patternMatch (Obj.car pat)
                                     (Obj.car dat)
                                     (SOME frame))
          )
        else
          (log ("pat is NONE",nil); NONE)
      )

  and extendIfConsistent var dat frame =
      (log ("extendIfConsistent: ~S ~S", [var,dat]);
      case Frame.bindingIn frame var of
        SOME (_, dat') =>
        (log ("extendIfConsistent: binding found",nil);
         patternMatch dat' dat (SOME frame))
      | NONE =>
        (log ("extendIfConsistent: binding not found",nil);
         SOME (Frame.extend frame (var, dat)))
      )

  local
    val ruleCounter = ref 0
  in
  fun renameVariablesIn rule =
      let
        val ruleApplicationId = newRuleApplicationId ()
        fun treeWalk exp =
            if isVar exp then
              makeNewVariable (exp, ruleApplicationId)
            else if Obj.isCons exp then
              Obj.cons (treeWalk (Obj.car exp),
                        treeWalk (Obj.cdr exp))
            else
              exp
      in
        treeWalk rule
      end
  and newRuleApplicationId () =
      !ruleCounter before ruleCounter := !ruleCounter + 1
  and makeNewVariable (var, ruleApplicationId) =
      Obj.cons (QUESTION,
                  Obj.cons (Obj.int ruleApplicationId, Obj.cdr var))
  end

  fun unifyMatch p1 p2 frameOpt =
      (log ("unifyMatch: ~S ~S", [p1,p2]);
      case frameOpt of
        NONE => NONE
      | SOME frame =>
        if Obj.equal (p1, p2) then SOME frame
        else if isVar p1 then
          extendIfPossible p1 p2 frame
        else if isVar p2 then
          extendIfPossible p2 p1 frame
        else if Obj.isCons p1 andalso Obj.isCons p2 then
          unifyMatch (Obj.cdr p1)
                     (Obj.cdr p2)
                     (unifyMatch (Obj.car p1)
                                 (Obj.car p2)
                                 (SOME frame))
        else
          NONE
      )

  and extendIfPossible var dat frame =
      (log ("extendIfPossible: ~S ~S", [var,dat]);
      case Frame.bindingIn frame var of
        SOME (_, dat') => unifyMatch dat' dat (SOME frame)
      | NONE =>
        if isVar dat then
          case Frame.bindingIn frame dat of
            SOME (_, dat') => unifyMatch var dat' (SOME frame)
          | NONE => SOME (Frame.extend frame (var, dat))
        else if dependsOn dat var frame then
          NONE
        else
          SOME (Frame.extend frame (var, dat))
      )

  and dependsOn exp var frame =
      let
        fun treeWalk exp =
            if isVar exp then
              if Obj.equal (var, exp) then
                true
              else
                case Frame.bindingIn frame exp of
                  SOME (_, dat) => treeWalk dat
                | NONE => false
            else if Obj.isCons exp then
              treeWalk (Obj.car exp) orelse
              treeWalk (Obj.cdr exp)
            else
              false
      in
        treeWalk exp
      end

  fun makeRuntime () =
      {PRED_RUNTIME = LispRuntime.makeRuntime (),
       THE_ASSERTIONS = ref NULL_OBJ_STREAM,
       THE_RULES = ref NULL_OBJ_STREAM,
       THE_TABLE = Table.make
                       (fn ((x1,y1),(x2,y2)) =>
                           Obj.eq (x1,x2) andalso Obj.eq (y1,y2))}

  fun makeNewFrame () = Frame.make Obj.equal

  fun executePredicate prt exp =
      let
        val env = LispRuntime.env prt
        val pred = Evaluator.eval (predicate exp) env
        val args = Obj.toList (args exp)
      in
        Obj.isTrue (Evaluator.apply pred args)
      end
end;

functor QueryDataBaseFn (structure Query : QUERY)
        : QUERY_DATA_BASE =
struct
  structure Q = Query

  (*val isUseIndex = Q.isConstantSymbol o Obj.car*)
  val isUseIndex = Q.isConstantSymbol o Q.typeOf

  fun indexKeyOf pat =
      let
        (*val key = Obj.car pat*)
        val key = Q.typeOf pat
      in
        if Q.isVar key then Q.QUESTION
        else if Q.isConstantSymbol key then key
        else Q.error ("Unexpected pattern: ~S", [pat])
      end

  (*
  fun isIndexable pat =
      (Q.isConstantSymbol o Obj.car) pat orelse
      (Q.isVar o Obj.car) pat
   *)
  fun isIndexable pat =
      let
        (*val key = Obj.car pat*)
        val key = Q.typeOf pat
      in
        Q.isConstantSymbol key orelse
        Q.isVar key
      end

  fun get ({THE_TABLE,...}:Q.qrt) (key1, key2) =
      Table.lookup THE_TABLE (key1, key2)
  fun put ({THE_TABLE,...}:Q.qrt) (key1, key2) stream =
      Table.insert THE_TABLE (key1, key2) stream

  fun getStream (qrt:Q.qrt) (key1, key2) =
      (Q.log ("getStream: ~S ~S", [key1,key2]);
      case get qrt (key1, key2) of
        SOME s => s
      | NONE => Q.NULL_OBJ_STREAM
      )

  fun storeAssertionInIndex (qrt:Q.qrt) assertion =
      (Q.log ("storeAssertionInIndex: ~S", [assertion]);
      if isIndexable assertion then
        let
          val key = indexKeyOf assertion
          val currentAssertionStream =
              getStream qrt (key, Q.ASSERTION_STREAM)
          val lazy stream =
              Stream.Cons (assertion, currentAssertionStream)
        in
          put qrt (key, Q.ASSERTION_STREAM) stream
        end
      else
        ()
      )

  fun storeRuleInIndex (qrt:Q.qrt) rule =
      (Q.log ("storeRuleInIndex: ~S", [rule]);
      let
        val pattern = Q.conclusion rule
      in
        if isIndexable pattern then
          let
            val key = indexKeyOf pattern
            val currentRuleStream =
                getStream qrt (key, Q.RULE_STREAM)
            val lazy stream =
                Stream.Cons (rule, currentRuleStream)
          in
            put qrt (key, Q.RULE_STREAM) stream
          end
        else
          ()
      end
      )

  fun addRuleOrAssertion (qrt:Q.qrt) assertion =
      (Q.log ("addRuleOrAssertion: ~S", [assertion]);
      if Q.isRule assertion then addRule qrt assertion
      else addAssertion qrt assertion
      )

  and addAssertion (qrt as {THE_ASSERTIONS,...}:Q.qrt) assertion =
      (Q.log ("addAssertion: ~S", [assertion]);
      (storeAssertionInIndex qrt assertion;
       let
         val oldAssertions = !THE_ASSERTIONS
         val lazy stream = Stream.Cons (assertion, oldAssertions)
       in
         THE_ASSERTIONS := stream
       end)
      )

  and addRule (qrt as {THE_RULES,...}:Q.qrt) rule =
      (Q.log ("addRule: ~S", [rule]);
      (storeRuleInIndex qrt rule;
       let
         val oldRules = !THE_RULES
         val lazy stream = Stream.Cons (rule, oldRules)
       in
         THE_RULES := stream
       end)
      )

  fun fetchAssertions (qrt:Q.qrt) (pattern, frame) =
      (Q.log ("fetchAssertions: ~S", [pattern]);
      if isUseIndex pattern then getIndexedAssertions qrt pattern
      else getAllAssertions qrt
      )

  and getIndexedAssertions (qrt:Q.qrt) pattern =
      getStream qrt (indexKeyOf pattern, Q.ASSERTION_STREAM)

  and getAllAssertions ({THE_ASSERTIONS,...}:Q.qrt) =
      !THE_ASSERTIONS

  fun fetchRules (qrt:Q.qrt) (pattern, frame) =
      (Q.log ("fetchRules: ~S", [pattern]);
      if isUseIndex pattern then getIndexedRules qrt pattern
      else getAllRules qrt
      )

  and getIndexedRules (qrt:Q.qrt) pattern =
      Stream.append (getStream qrt (indexKeyOf pattern, Q.RULE_STREAM),
                     getStream qrt (Q.QUESTION, Q.RULE_STREAM))

  and getAllRules ({THE_RULES,...}:Q.qrt) =
      !THE_RULES
end;

functor QueryEvaluatorFn (structure Query : QUERY
                          structure DataBase : QUERY_DATA_BASE
                          sharing Query = DataBase.Q)
        : QUERY_EVALUATOR =
struct
  structure Q = Query
  structure QDB = DataBase

  fun eval (qrt:Q.qrt) query frameStream =
      (Q.log ("eval: ~S", [query]);
      if Q.isConjunction query then
        conjoin qrt (Q.conjunctionBody query) frameStream
      else if Q.isDisjunction query then
        disjoin qrt (Q.disjunctionBody query) frameStream
      else if Q.isNegation query then
        negate qrt (Q.negatedQuery query) frameStream
      else if Q.isPredicate query then
        lispValue qrt (Q.predicateBody query) frameStream
      else if Q.isAlwaysTrue query then
        frameStream
      else
        simpleQuery qrt query frameStream
      )

  and simpleQuery (qrt:Q.qrt) queryPattern frameStream =
      (Q.log ("simpleQuery: ~S", [queryPattern]);
      Stream.flatmap
          (fn frame =>
              Stream.append (findAssertion qrt queryPattern frame,
                             (*delay*)applyRules qrt queryPattern frame))
          frameStream
      )

  and conjoin (qrt:Q.qrt) conjuncts frameStream =
      (Q.log ("conjoin: ~S", [conjuncts]);
      if Q.isEmptyConjunction conjuncts then
        frameStream
      else
        conjoin qrt
                (Q.restConjuncts conjuncts)
                (eval qrt (Q.firstConjunct conjuncts) frameStream)
      )

  and disjoin (qrt:Q.qrt) disjuncts frameStream =
      (Q.log ("disjoin: ~S", [disjuncts]);
      if Q.isEmptyDisjunction disjuncts then
        Q.NULL_FRAME_STREAM
      else
        Stream.interleave
            (eval qrt (Q.firstDisjunct disjuncts) frameStream,
             (*delay*)disjoin qrt (Q.restDisjuncts disjuncts) frameStream)
      )

  and negate (qrt:Q.qrt) query frameStream =
      (Q.log ("negate: ~S", [query]);
      Stream.flatmap
          (fn frame =>
              let
                val frameStream' = Stream.singleton frame
              in
                case eval qrt query frameStream' of
                  Stream.Nil => frameStream'
                | _ => Q.NULL_FRAME_STREAM
              end)
          frameStream
      )

  and lispValue (qrt:Q.qrt) call frameStream =
      (Q.log ("lispValue: ~S", [call]);
      Stream.flatmap
          (fn frame =>
              let
                fun handler exp frame =
                    Q.error ("Unknown pat var -- lispValue: ~S", [exp])
                val exp = Q.instantiate call frame handler
                val prt = #PRED_RUNTIME qrt
              in
                if Q.executePredicate prt exp then
                  Stream.singleton frame
                else
                  Q.NULL_FRAME_STREAM
              end)
          frameStream
      )

  and findAssertion (qrt:Q.qrt) pattern frame =
      (Q.log ("findAssertion: ~S", [pattern]);
      Stream.flatmap
          (fn datum => checkOneAssertion datum pattern frame)
          (QDB.fetchAssertions qrt (pattern, frame))
      )

  and checkOneAssertion assertion queryPat queryFrame =
      (Q.log ("checkOneAssertion: ~S ~S", [assertion, queryPat]);
      case Q.patternMatch queryPat assertion (SOME queryFrame) of
        SOME matchResult => (Q.log ("checkOneAssertion: BINGO",nil);
                             Stream.singleton matchResult)
      | NONE => (Q.log ("checkOneAssertion: NONE",nil);
                 Q.NULL_FRAME_STREAM)
      )

  and applyRules (qrt:Q.qrt) pattern frame =
      (Q.log ("applyRules: ~S", [pattern]);
      Stream.flatmap
          (fn rule => applyOneRule qrt rule pattern frame)
          (QDB.fetchRules qrt (pattern, frame))
      )

  and applyOneRule (qrt:Q.qrt) rule queryPat queryFrame =
      (Q.log ("applyOneRule: ~S ~S", [rule, queryPat]);
      let
        val cleanRule = Q.renameVariablesIn rule
      in
        case Q.unifyMatch queryPat
                              (Q.conclusion cleanRule)
                              (SOME queryFrame) of
          SOME unifyResult => (Q.log ("applyOneRule: BINGO",nil);
                               eval qrt
                                    (Q.ruleBody cleanRule)
                                    (Stream.singleton unifyResult))
        | NONE => (Q.log ("applyOneRule: NONE",nil);
                   Q.NULL_FRAME_STREAM)
      end
      )
end;

functor QueryInterpreterFn (LispRuntime : LISP_RUNTIME)
        : INTERPRETER =
struct
  structure Obj = LispRuntime.Obj
  structure Reader = LispRuntime.Reader
  structure Evaluator = LispRuntime.Evaluator
  structure Printer = LispRuntime.Printer

  structure Q
    = QueryFn (LispRuntime)
  structure QDB
    = QueryDataBaseFn (structure Query = Q)
  structure QE
    = QueryEvaluatorFn (structure Query = Q and DataBase = QDB)

  val stdIn = LispRuntime.stdIn
  val stdOut = LispRuntime.stdOut
  val stdErr = LispRuntime.stdErr
  val quit = Obj.sym ":q"
  val eval = Obj.sym ":e"
  val debug = Obj.sym ":d"

  fun hello (qrt:Q.qrt) =
      let
        val lrt = #PRED_RUNTIME qrt
      in
        ignore (Printer.format (stdOut lrt,
                                "Hello!~%"^
                                "Type '~S' to exit~%"^
                                "Type '~S' to eval lisp expression~%"^
                                "Type '~S' to toggle debug flag~%",
                                [quit, eval, debug]))
      end

  fun bye (qrt:Q.qrt) =
      let
        val lrt = #PRED_RUNTIME qrt
      in
        ignore (Printer.format (stdOut lrt,
                                "Bye!~%",
                                nil))
      end

  fun onError (qrt:Q.qrt)
              (Obj.Error (ctrlstr,args), cont) =
      let
        val lrt = #PRED_RUNTIME qrt
        val msg = "Runtime error: " ^ ctrlstr ^ "~%"
      in
        Printer.format (stdErr lrt, msg, args);
        cont ()
      end
    | onError (qrt:Q.qrt)
              (IO.Io {name,function,cause}, cont) =
      let
        val lrt = #PRED_RUNTIME qrt
        val msg = "IO error: " ^ name ^ " -- " ^ function ^
                  " (cause: " ^ exnMessage cause ^ ")~%"
      in
        Printer.format (stdErr lrt, msg, nil);
        cont ()
      end
    | onError (qrt:Q.qrt)
              (e, cont) =
      let
        val lrt = #PRED_RUNTIME qrt
        val msg = "Error: " ^ exnMessage e ^ "~%"
      in
        Printer.format (stdErr lrt, msg, nil);
        cont ()
      end

  fun repl (qrt:Q.qrt) prompt =
      let
        val lrt = #PRED_RUNTIME qrt
        val counter = ref 0
        fun inc () = (Obj.int (!counter)) before counter := !counter + 1
        fun count () = Obj.int (!counter)
        fun reset () = counter := 0
        fun loop () =
            let
              val obj = (Printer.format (stdOut lrt, prompt, nil);
                         (Q.querySyntaxProcess o Reader.read) (stdIn lrt))
              val frameStream = Stream.singleton (Q.makeNewFrame ())
              fun handler exp frame = Q.contractQuestionMark exp
            in
              if Obj.isEof obj orelse Obj.eq (obj, quit) then
                ()
              else if Obj.eq (obj, eval) then (* toggle debug flag *)
                (let
                   val obj' = Reader.read (stdIn lrt)
                   val obj'' = Evaluator.eval obj' (LispRuntime.env lrt)
                 in
                   Printer.print (stdOut lrt, obj'');
                   loop ()
                 end)
              else if Obj.eq (obj, debug) then (* eval lisp exp *)
                (Q.debug := not (!Q.debug);
                 Printer.format (stdOut lrt, "Debug: ~S.~%",
                                 [Obj.bool (!Q.debug)]);
                 loop ())
              else if Q.isAssertionToBeAdded obj then
                (QDB.addRuleOrAssertion qrt (Q.addAssertionBody obj);
                 Printer.format (stdOut lrt, "Assertion added to DB.~%", nil);
                 loop ())
              else
                (reset ();
                 Stream.app
                    (fn exp => Printer.format (stdOut lrt, "~S: ~S~%",
                                               [inc(), exp]))
                    (Stream.map
                         (fn frame => Q.instantiate obj frame handler)
                         (QE.eval qrt obj frameStream));
                 Printer.format (stdOut lrt, "~S result(s) found.~%",
                                 [count ()]);
                 loop ())
            end
            handle e => onError qrt (e, loop)
      in
        loop ()
      end

  fun load qrt file =
      let
        val lrt = #PRED_RUNTIME qrt
        val oldIn = stdIn lrt
        fun body () =
            let
              val newIn = Obj.openIn file
              fun body' () = (LispRuntime.setStdIn lrt newIn; repl qrt "~%")
              fun cleanup' () = ignore (Obj.closeIn newIn)
            in
              U.unwindProtect body' cleanup'
            end
        fun cleanup () = LispRuntime.setStdIn lrt oldIn
      in
        U.unwindProtect body cleanup
      end

  fun go () =
      let
        val qrt = Q.makeRuntime ()
        val lrt = #PRED_RUNTIME qrt
        (* load *)
        val fnLoad = (fn file => (load qrt (Obj.toString file); Obj.undef))
        val subrLoad = Obj.subr1 ("load", fnLoad)
        val symLoad = Obj.sym (Obj.subrName subrLoad)
        val _ = Obj.defineEnv (LispRuntime.env lrt) (symLoad, subrLoad)
      in
        (hello qrt; repl qrt "~%> "; bye qrt)
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
structure QI = QueryInterpreterFn (Runtime)
end;

(*
 * QI.go (); (* => activates top-level *)
 *)

(*
 * sample data base can be loaded by executing
 * the following at the lisp top-level:
 * :e (load "chap4_4_example.scm")
 *)
