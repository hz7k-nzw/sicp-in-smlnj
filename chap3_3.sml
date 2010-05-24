(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 3  Modularity, Objects, and State *)
(* 3.3  Modeling with Mutable Data *)
(* 3.3.1  Mutable List Structure *)

(*
 * Immutable version of PAIR has been defined in chap2_1.sml
 *)
signature PAIR = sig
  type 'a t
  val cons : 'a * 'a -> 'a t
  val car : 'a t -> 'a
  val cdr : 'a t -> 'a
  val setCar : 'a t -> 'a -> unit (* added *)
  val setCdr : 'a t -> 'a -> unit (* added *)
end;

signature SEXP =
sig
  type t
  val null : t
  val int : int -> t
  val str : string -> t
  val cons : t * t -> t
  val car : t -> t
  val cdr : t -> t
  val setCar : t -> t -> unit
  val setCdr : t -> t -> unit
  val isNull : t -> bool
  val list : t list -> t
  val lastPair : t -> t
  val append : t * t -> t
  val append' : t * t -> t
  val toString : t -> string
end;

functor SexpFn (Pair : PAIR) :> SEXP =
struct
  datatype t = Nil
             | Int of int
             | Str of string
             | Cons of t Pair.t

  val null = Nil

  val int = Int

  val str = Str

  fun cons (x, y) = Cons (Pair.cons (x, y))

  fun car (Cons p) = Pair.car p
    | car Nil = raise Empty
    | car _ = raise Match

  fun cdr (Cons p) = Pair.cdr p
    | cdr Nil = raise Empty
    | cdr _ = raise Match

  fun setCar (Cons p) newVal = Pair.setCar p newVal
    | setCar Nil _ = raise Empty
    | setCar _ _ = raise Match

  fun setCdr (Cons p) newVal = Pair.setCdr p newVal
    | setCdr Nil _ = raise Empty
    | setCdr _ _ = raise Match

  fun isNull Nil = true
    | isNull _ = false

  fun list nil = Nil
    | list (x::xs) = cons (x, list xs)

  fun lastPair x =
      let
        val tail = cdr x
      in
        if isNull tail then x else lastPair tail
      end

  fun append (x, y) =
      if isNull x then y
      else cons (car x, append (cdr x, y))

  fun append' (x, y) = (setCdr (lastPair x) y; x)

  fun toString x =
      let
        fun s1 Nil      = "()"
          | s1 (Int i)  = Int.toString i
          | s1 (Str s)  = s
          | s1 (Cons p) = "(" ^ s1 (Pair.car p) ^ s2 (Pair.cdr p) ^ ")"
        and s2 Nil      = ""
          | s2 (Int i)  = " . " ^ Int.toString i
          | s2 (Str s)  = " . " ^ s
          | s2 (Cons p) = " " ^ s1 (Pair.car p) ^ s2 (Pair.cdr p)
      in
        s1 x
      end
end;

structure PairAsTuple :> PAIR =
struct
  type 'a t = 'a ref * 'a ref

  fun cons (x, y) = (ref x, ref y)

  fun car (xRef, _) = !xRef

  fun cdr (_, yRef) = !yRef

  fun setCar (xRef, _) newVal = xRef := newVal

  fun setCdr (_, yRef) newVal = yRef := newVal
end;

structure S = SexpFn (PairAsTuple);

val one = S.int 1;
val two =  S.int 2;
val three = S.int 3;
val four = S.int 4;

val x = S.list [one, two];
val y = S.list [three, four];

val z = S.append (x, y);
S.toString z;
S.toString (S.cdr x);

val w = S.append' (x, y);
S.toString w;
S.toString (S.cdr x);

fun mystery x =
    let
      fun loop (x, y) =
          (U.log ("(x y)=(" ^ S.toString x ^ " " ^ S.toString y ^ ")");
           if S.isNull x then y
           else
             let
               val temp = S.cdr x
             in
               S.setCdr x y; loop (temp, x)
             end)
    in
      loop (x, S.null)
    end;

val v = S.list [one, two, three, four];
val w = mystery v;
S.toString v;
S.toString w;

val x = S.list [one, two];
val z1 = S.cons (x, x);
val z2 = S.cons (S.list [one, two], S.list [one, two]);

(*
 * (S.car z1) = (S.cdr z1);
 * (S.car z2) = (S.cdr z2);
 * => These code does not work because
 *    my sexp does not support equality test.
 *)

fun setToWow x =
    let
      val wow = S.str "wow"
    in
      S.setCar (S.car x) wow; x
    end;

S.toString z1;
S.toString (setToWow z1);

S.toString z2;
S.toString (setToWow z2);

structure PairAsFun :> PAIR  = struct
  datatype msg = Car | Cdr | SetCar | SetCdr
  datatype 'a ret = Val of 'a | Setter of 'a -> unit
  type 'a t = msg -> 'a ret

  fun cons (x, y) =
      let
        val xRef = ref x
        val yRef = ref y
        fun dispatch Car = Val (!xRef)
          | dispatch Cdr = Val (!yRef)
          | dispatch SetCar = Setter (fn newVal => xRef := newVal)
          | dispatch SetCdr = Setter (fn newVal => yRef := newVal)
      in
        dispatch
      end

  fun car z =
      case z Car
       of Val v => v
        | _     => raise Match

  fun cdr z =
      case z Cdr
       of Val v => v
        | _     => raise Match

  fun setCar z newVal =
      case z SetCar
       of Setter f => f newVal
        | _        => raise Match

  fun setCdr z newVal =
      case z SetCdr
       of Setter f => f newVal
        | _        => raise Match
end;

structure S = SexpFn (PairAsFun);

val one = S.int 1;
val two =  S.int 2;
val seventeen = S.int 17;

val x = S.cons (one, two);
val z = S.cons (x, x);
S.setCar (S.cdr z) seventeen;
S.toString z;
S.toString (S.car x);

(* 3.3.2  Representing Queues *)

(*
 * http://www.codepoetics.com/wiki/index.php?title=Topics:SICP_in_other_languages:Alice_ML:Chapter_3
*)

signature QUEUE =
sig
  type v
  type queue
  val make : unit -> queue
  val empty : queue -> bool
  val front : queue -> v
  val insert : queue -> v -> queue
  val delete : queue -> queue
  val toString : queue -> string
end;

functor QueueFn (type v; val f : v -> string) :>
        QUEUE where type v = v =
struct
  type v = v
  datatype qnode = Nil
                 | Node of v * qnode ref
  type queue = qnode ref * qnode ref (* frontPtr * rearPtr *)

  fun make () = (ref Nil, ref Nil)

  fun empty (fp, _) =
      case !fp of
        Nil => true
      | Node (_, _) => false

  fun front (fp, _) =
      case !fp of
        Nil => raise Empty
      | Node (v, _) => v

  fun insert (queue as (fp, rp)) v =
      let
        val newNode = Node (v, ref Nil)
      in
        case !fp of
          Nil => (fp := newNode;
                  rp := newNode;
                  queue)
        | _ => case !rp of
                 Nil => raise Fail "Not reached"
               | Node (_, rp') => (rp' := newNode;
                                   rp  := newNode;
                                   queue)
      end

  fun delete (queue as (fp, rp)) =
      case !fp of
        Nil => raise Empty
      | Node (_, fp') => (fp := !fp';
                          queue)

  fun toString (fp, rp) =
      let
        fun s1 Nil = "()"
          | s1 (Node (v, p')) = "(" ^ f v ^ s2 (!p')
        and s2 Nil = ")"
          | s2 (Node (v, p')) = " " ^ f v ^ s2 (!p')
      in
        "(" ^ s1 (!fp) ^ " " ^ s1 (!rp) ^ ")"
      end
end;

structure Q = QueueFn (type v = int; val f = Int.toString);

val q = Q.make ();
Q.toString q;
Q.toString (Q.insert q 1);
Q.toString (Q.insert q 2);
Q.toString (Q.insert q 3);
Q.toString (Q.delete q);
Q.toString (Q.delete q);
Q.toString (Q.delete q);

(* 3.3.3  Representing Tables *)

signature TABLE =
sig
  eqtype k
  type v
  type table
  val make : unit -> table
  val lookup : table -> k -> v option
  val insert : table -> k -> v -> unit
end;

functor TableFn1 (eqtype k1; type v) :>
        TABLE where type k = k1 and type v = v =
struct
  type k = k1
  type v = v
  type vref = v ref
  type table = (k1 * vref) list ref

  fun make () = ref nil

  fun lookup table key =
      case U.assoc key (!table) of
        SOME (_, vref) => SOME (!vref)
      | NONE => NONE

  fun insert table key newValue =
      case U.assoc key (!table) of
        SOME (_, vref) =>
        (vref := newValue;
         U.log "vref updated!")
      | NONE =>
        (table := (key, ref newValue) :: (!table);
         U.log "vref inserted!")
end;

structure T = TableFn1 (type k1 = char and v = int);
val t = T.make();
T.lookup t #"a";
T.lookup t #"b";
T.lookup t #"c";
T.insert t #"a" 1;
T.insert t #"b" 2;
T.insert t #"c" 3;
T.lookup t #"a";
T.lookup t #"b";
T.lookup t #"c";
T.insert t #"a" 11;
T.insert t #"b" 12;
T.insert t #"c" 13;
T.lookup t #"a";
T.lookup t #"b";
T.lookup t #"c";

functor TableFn2 (eqtype k1 and k2; type v) :>
        TABLE where type k = (k1 * k2) and type v = v =
struct
  type k = k1 * k2
  type v = v
  type vref = v ref
  type subtable = (k2 * vref) list ref
  type table = (k1 * subtable) list ref

  fun make () = ref nil

  fun lookup table (key1, key2) =
      case U.assoc key1 (!table) of
        SOME (_, subtable) =>
        (case U.assoc key2 (!subtable) of
          SOME (_, vref) => SOME (!vref)
        | NONE => NONE)
      | NONE => NONE

  fun insert table (key1, key2) newValue =
      case U.assoc key1 (!table) of
        SOME (_, subtable) =>
        (case U.assoc key2 (!subtable) of
           SOME (_, vref) =>
           (vref := newValue;
            U.log "vref updated!")
         | NONE =>
           (subtable := (key2, ref newValue) :: (!subtable);
            U.log "vref inserted!"))
      | NONE =>
        (table := (key1, ref [(key2, ref newValue)]) :: (!table);
         U.log "subtable inserted!")
end;

structure T = TableFn2 (type k1 = string and k2 = char and v = int);
val t = T.make();
T.lookup t ("math",#"+");
T.lookup t ("math",#"-");
T.lookup t ("math",#"*");
T.insert t ("math",#"+") 43;
T.insert t ("math",#"-") 45;
T.insert t ("math",#"*") 42;
T.lookup t ("math",#"+");
T.lookup t ("math",#"-");
T.lookup t ("math",#"*");
T.insert t ("math",#"+") 443;
T.insert t ("math",#"-") 445;
T.insert t ("math",#"*") 442;
T.lookup t ("math",#"+");
T.lookup t ("math",#"-");
T.lookup t ("math",#"*");
T.lookup t ("letters",#"a");
T.lookup t ("letters",#"b");
T.insert t ("letters",#"a") 97;
T.insert t ("letters",#"b") 98;
T.lookup t ("letters",#"a");
T.lookup t ("letters",#"b");
T.insert t ("letters",#"a") 997;
T.insert t ("letters",#"b") 998;
T.lookup t ("letters",#"a");
T.lookup t ("letters",#"b");
(*
 * functor LocalTableFn (structure T : TABLE) =
 * struct
 *   fun make () =
 *       let
 *         val table = T.make ()
 *         val lookup = T.lookup table
 *         val insert = T.insert table
 *       in
 *         { Lookup = lookup, Insert = insert }
 *       end
 * end;
 *
 * structure LT = LocalTableFn (structure T = Table');
 * val lt = LT.make();
 * val get = #Lookup lt;
 * val put = #Insert lt;
 *)
val t = T.make();
val get = T.lookup t;
val put = T.insert t;

get ("math",#"+");
get ("math",#"-");
get ("math",#"*");
put ("math",#"+") 43;
put ("math",#"-") 45;
put ("math",#"*") 42;
get ("math",#"+");
get ("math",#"-");
get ("math",#"*");
put ("math",#"+") 443;
put ("math",#"-") 445;
put ("math",#"*") 442;
get ("math",#"+");
get ("math",#"-");
get ("math",#"*");
get ("letters",#"a");
get ("letters",#"b");
put ("letters",#"a") 97;
put ("letters",#"b") 98;
get ("letters",#"a");
get ("letters",#"b");
put ("letters",#"a") 997;
put ("letters",#"b") 998;
get ("letters",#"a");
get ("letters",#"b");

functor TableFnN (eqtype k'; type v) :>
        TABLE where type k = k' list and type v = v =
struct
  type k = k' list
  type v = v
  datatype dataref
    = ValueRef of v ref
    | SubtableRef of (k' * dataref) list ref
  type table = dataref

  fun make () = SubtableRef (ref nil)

  fun valueRef (ValueRef vref) = vref
    | valueRef _ = raise Match

  fun subtableRef (SubtableRef stref) = stref
    | subtableRef _ = raise Match

  fun lookup table nil =
      let
        val vref = valueRef table
      in
        SOME (!vref)
      end
    | lookup table (key::keys) =
      let
        val stref = subtableRef table
      in
        case U.assoc key (!stref) of
          SOME (_, dref) => lookup dref keys
        | NONE => NONE
      end

  fun insert table nil newValue =
      let
        val vref = valueRef table
      in
        vref := newValue;
        U.log "vref updated!"
      end
    | insert table (key::keys) newValue =
      let
        val stref = subtableRef table
      in
        case U.assoc key (!stref) of
          SOME (_, dref) =>
          (insert dref keys newValue;
           U.log "subtable updated!")
        | NONE =>
          if null keys then
            let
              val dref = ValueRef (ref newValue)
            in
              stref := (key, dref) :: (!stref);
              U.log "vref inserted!"
            end
          else
            let
              val dref = SubtableRef (ref nil)
            in
              insert dref keys newValue;
              stref := (key, dref) :: (!stref);
              U.log "subtable inserted!"
            end
      end
end;

structure T = TableFnN (type k' = string and v = int);
val t = T.make();
T.lookup t ["a"];
T.lookup t ["b"];
T.lookup t ["c"];
T.insert t ["a"] 1;
T.insert t ["b"] 2;
T.insert t ["c"] 3;
T.lookup t ["a"];
T.lookup t ["b"];
T.lookup t ["c"];
T.insert t ["a"] 11;
T.insert t ["b"] 12;
T.insert t ["c"] 13;
T.lookup t ["a"];
T.lookup t ["b"];
T.lookup t ["c"];
T.lookup t ["letters","a"];
T.lookup t ["letters","b"];
T.insert t ["letters","a"] 97;
T.insert t ["letters","b"] 98;
T.lookup t ["letters","a"];
T.lookup t ["letters","b"];
T.insert t ["letters","a"] 997;
T.insert t ["letters","b"] 998;
T.lookup t ["letters","a"];
T.lookup t ["letters","b"];
T.lookup t ["math","arith","+"];
T.lookup t ["math","arith","-"];
T.lookup t ["math","arith","*"];
T.insert t ["math","arith","+"] 43;
T.insert t ["math","arith","-"] 45;
T.insert t ["math","arith","*"] 42;
T.lookup t ["math","arith","+"];
T.lookup t ["math","arith","-"];
T.lookup t ["math","arith","*"];
T.insert t ["math","arith","+"] 443;
T.insert t ["math","arith","-"] 445;
T.insert t ["math","arith","*"] 442;
T.lookup t ["math","arith","+"];
T.lookup t ["math","arith","-"];
T.lookup t ["math","arith","*"];

(* 3.3.4  A Simulator for Digital Circuits *)

structure Time =
struct
  type t = int
  val zero : t = 0
  val add : t * t -> t = op +
  val sub : t * t -> t = op -
  val toString : t -> string = Int.toString
end;

structure Signal =
struct
  datatype t = T | F

  fun lnot F = T
    | lnot T = F

  fun land (F,F) = F
    | land (T,F) = F
    | land (F,T) = F
    | land (T,T) = T

  fun lor (F,F) = F
    | lor (T,F) = T
    | lor (F,T) = T
    | lor (T,T) = T

  fun lxor (F,F) = F
    | lxor (T,F) = T
    | lxor (F,T) = T
    | lxor (T,T) = F

  fun toString T = "1"
    | toString F = "0"

  fun fromInt 1 = T
    | fromInt 0 = F
    | fromInt _ = raise Match

  fun toInt T = 1
    | toInt F = 0
end;

signature DIGITAL_CIRCUIT =
sig
  type wire

  val inverterDelay : Time.t
  val andGateDelay : Time.t
  val orGateDelay : Time.t
  val inverter : wire * wire -> unit
  val andGate : wire * wire * wire -> unit
  val orGate : wire * wire * wire -> unit
  val halfAdder : wire * wire * wire * wire -> unit
  val fullAdder : wire * wire * wire * wire * wire -> unit
  val rippleCarryAdder : wire list * wire list * wire list * wire -> unit
end;

signature WIRE =
sig
  type t

  val make : unit -> t
  val getSignal : t -> Signal.t
  val setSignal : t -> Signal.t -> unit
  val addAction : t -> (unit -> unit) -> unit
  val afterDelay : Time.t * (unit -> unit) -> unit
  val propagate : unit -> unit
  val probe : string * t -> unit
end;

functor DigitalCircuitFn (W : WIRE) : DIGITAL_CIRCUIT =
struct
  type wire = W.t

  val inverterDelay = 2
  val andGateDelay = 3
  val orGateDelay = 5

  fun inverter (input, output) =
      let
        fun invertInput () =
            let
              val newValue = Signal.lnot (W.getSignal input)
              fun action () = W.setSignal output newValue
            in
              W.afterDelay (inverterDelay, action)
            end
      in
        W.addAction input invertInput;
        U.log "inverter: ok"
      end

  fun andGate (a1, a2, output) =
      let
        fun andActionProcedure () =
            let
              val newValue = Signal.land (W.getSignal a1,
                                          W.getSignal a2)
              fun action () = W.setSignal output newValue
            in
              W.afterDelay (andGateDelay, action)
            end
      in
        W.addAction a1 andActionProcedure;
        W.addAction a2 andActionProcedure;
        U.log "andGate: ok"
      end

  fun orGate (o1, o2, output) =
      let
        fun orActionProcedure () =
            let
              val newValue = Signal.lor (W.getSignal o1,
                                         W.getSignal o2)
              fun action () = W.setSignal output newValue
            in
              W.afterDelay (orGateDelay, action)
            end
      in
        W.addAction o1 orActionProcedure;
        W.addAction o2 orActionProcedure;
        U.log "orGate: ok"
      end
(*
  fun xorGate (x1, x2, output) =
      let
        val x1' = W.make ()
        and x2' = W.make ()
        and a = W.make ()
        and b = W.make ()
        and 
      in
        inverter (x1, x1');
        inverter (x2, x2');
        andGate (x1, x2', a);
        andGate (x1', x2, b);
        orGate (a, b, output);
        U.log "xorGate: ok"
      end
*)
  fun halfAdder (a, b, s, c) =
      let
        val d = W.make ()
        and e = W.make ()
      in
        orGate (a, b, d);
        andGate (a, b, c);
        inverter (c, e);
        andGate (d, e, s);
        U.log "halfAdder: ok"
      end

  fun fullAdder (a, b, cIn, sum, cOut) =
      let
        val s = W.make ()
        and c1 = W.make ()
        and c2 = W.make ()
      in
        halfAdder (b, cIn, s, c1);
        halfAdder (a, s, sum, c2);
        orGate (c1, c2, cOut);
        U.log "fullAdder: ok"
      end

  fun rippleCarryAdder (a, b, s, c) =
      let
        fun build (nil, nil, nil, _) =
            U.log "rippleCarryAdder: ok"
          | build (ha::ta, hb::tb, hs::ts, cOut) =
            let
              val cIn = W.make ()
            in
              fullAdder (ha, hb, cIn, hs, cOut);
              build (ta, tb, ts, cIn)
            end
          | build _ = raise Match
      in
        build (a, b, s, c)
      end
end;

signature AGENDA =
sig
  type t

  val make : unit -> t
  val empty : t -> bool
  val firstItem : t -> (unit -> unit)
  val removeFirstItem : t -> unit
  val addTo : Time.t * (unit -> unit) * t -> unit
  val currentTime : t -> Time.t
end;

functor WireFn (A : AGENDA) :> WIRE =
struct
  type t = {GetSignal : unit -> Signal.t,
            SetSignal : Signal.t -> unit,
            AddAction : (unit -> unit) -> unit}

  val theAgenda = A.make ()

  fun make () =
      let
        val signalValue = ref Signal.F
        val actionProcedures = ref nil

        fun callEach nil = U.log "callEach: done"
          | callEach (proc::procs) = (proc (); callEach procs)
        fun getter () =
            !signalValue
        fun setter newValue =
            if not (!signalValue = newValue) then
              (signalValue := newValue; callEach (!actionProcedures))
            else
              U.log "setter: done"
        fun adder proc =
            (actionProcedures := (proc :: (!actionProcedures)); proc ())
      in
        {GetSignal=getter, SetSignal=setter, AddAction=adder}
      end

  fun getSignal ({GetSignal,...}:t) = GetSignal ()

  fun setSignal ({SetSignal,...}:t) value = SetSignal value

  fun addAction ({AddAction,...}:t) action = AddAction action

  fun afterDelay (delay, action) =
      let
        val time = Time.add (delay, A.currentTime theAgenda)
      in
        A.addTo (time, action, theAgenda)
      end

  fun propagate () =
      if A.empty theAgenda then
        U.log "propagate: done"
      else
        let
          val firstItem = A.firstItem theAgenda
        in
          firstItem ();
          A.removeFirstItem theAgenda;
          propagate ()
        end

  fun probe (name, wire) =
      let
        fun action () =
            print (name ^ ": Time = " ^
                   (Time.toString o A.currentTime) theAgenda ^
                   ", New-value = " ^
                   (Signal.toString o getSignal) wire ^ "\n")
      in
        addAction wire action
      end
end;

structure Agenda :> AGENDA =
struct
  structure Q = QueueFn (type v = (unit -> unit) (* action *)
                         val f = (fn _ => "fn")) (* actionToString *)
  type segment = Time.t * Q.queue
  datatype segments = Nil | Cons of segment * segments ref
  type t = Time.t ref * segments ref

  fun make () = (ref Time.zero, ref Nil)

  fun currentTime (timeRef, _) = !timeRef

  fun setCurrentTime (timeRef, _) time = timeRef := time

  fun segments (_, segmentsRef) = !segmentsRef

  fun setSegments (_, segmentsRef) segments =
      segmentsRef := segments

  fun firstSegment agenda =
      case segments agenda of
        Nil => raise Empty
      | Cons (first, _) => first

  fun restSegments agenda =
      case segments agenda of
        Nil => Nil
      | Cons (_, restRef) => !restRef

  fun empty agenda =
      case segments agenda of
        Nil => true
      | Cons (_, _) => false

  fun firstItem agenda =
      let
        val (time, queue) = firstSegment agenda
      in
        setCurrentTime agenda time;
        Q.front queue
      end

  fun addTo (time, action, agenda) =
      let
        fun belongsBefore Nil = true
          | belongsBefore (Cons ((t, _), _)) = time < t
        fun makeNewSegment () =
            let
              val q = Q.make()
            in
              Q.insert q action; (time, q)
            end
        fun addToSegments Nil = raise Empty
          | addToSegments ((* segments as *)
                           Cons ((t, q), restRef)) =
            (* assert: belongsBefore segments = false *)
            if time = t then
              ignore (Q.insert q action)
            else if belongsBefore (!restRef) then
              restRef := Cons (makeNewSegment (), ref (!restRef))
            else
              addToSegments (!restRef)
      in
        if belongsBefore (segments agenda) then
          setSegments agenda (Cons (makeNewSegment (),
                                    ref (segments agenda)))
        else
          addToSegments (segments agenda)
      end

  fun removeFirstItem agenda =
      let
        val (time, queue) = firstSegment agenda
      in
        Q.delete queue;
        if Q.empty queue then
          setSegments agenda (restSegments agenda)
        else
          ()
      end
end;

structure A = Agenda;
structure W = WireFn (A);
structure DC = DigitalCircuitFn (W);

fun sampleSimulation1 () =
    let
      val input1 = W.make ()
      val input2 = W.make ()
      val sum = W.make ()
      val carry = W.make ()
    in
      W.probe ("sum", sum);
      W.probe ("carry", carry);
      DC.halfAdder (input1, input2, sum, carry);
      W.setSignal input1 Signal.T;
      W.propagate ();
      W.setSignal input2 Signal.T;
      W.propagate ()
    end;

fun sampleSimulation2 (bA, bB) =
    let
      val ssA = map Signal.fromInt bA
      val ssB = map Signal.fromInt bB
      val wsA = map W.make [(),(),(),()]
      val wsB = map W.make [(),(),(),()]
      val wsS = map W.make [(),(),(),()]
      val wC = W.make ()
      val w2i : W.t -> int = Signal.toInt o W.getSignal
      fun setSignals (nil, nil) = ()
        | setSignals (w::ws, s::ss) =
          (W.setSignal w s; setSignals (ws, ss))
        | setSignals _ = raise Match
      fun probeWires (_, _, nil) = ()
        | probeWires (name, n, w::ws) =
          (W.probe (name ^ "-" ^ Int.toString n, w);
           probeWires (name, n+1, ws))
    in
      probeWires ("sum", 0, wsS);
      W.probe ("carry", wC);
      DC.rippleCarryAdder (wsA, wsB, wsS, wC);
      setSignals (wsA, ssA);
      setSignals (wsB, ssB);
      W.propagate ();
      (map w2i wsS, w2i wC)
    end;

sampleSimulation1 ();
sampleSimulation2 ([1,0,1,0],[0,1,0,1]);

(* 3.3.5  Propagation of Constraints *)

signature CONSTRAINT_SYSTEM =
sig
  type value
  type informant
  type connector
  val makeConnector : string -> connector
  val makeUser : string -> informant
  val hasValue : connector -> bool
  val getValue : connector -> value
  val setValue : connector -> value * informant -> unit
  val forgetValue : connector -> informant -> unit
  val adder : connector * connector * connector -> informant
  val multiplier : connector * connector * connector -> informant
  val constant : value * connector -> informant
  val probe : string * connector -> informant
end;

structure Value =
struct
  datatype t = Nil | Int of int

  val zero : t = Int 0
  val null : t = Nil

  fun add (Int i, Int j) = Int (i + j)
    | add _ = raise Match

  fun sub (Int i, Int j) = Int (i - j)
    | sub _ = raise Match

  fun mul (Int i, Int j) = Int (i * j)
    | mul _ = raise Match

  fun quo (Int i, Int j) = Int (i div j)
    | quo _ = raise Match

  fun eq (Int i, Int j) = i = j
    | eq (Nil, Nil) = true
    | eq _ = false

  fun toString (Int i) = Int.toString i
    | toString Nil = "?"
end;

structure Informant =
struct
  type t = unit -> {Id : int,
                    Name : string,
                    I_have_a_value : unit -> unit,
                    I_lost_my_value : unit -> unit}
  local
    val idref = ref 0;
  in
  fun newId () =
      let
        val id = !idref
      in
        idref := id + 1; id
      end
  end

  val null : t =
      let
        val id = newId ()
        val action = fn () => ()
      in
        (fn () => {Id = id,
                   Name = "null",
                   I_have_a_value  = action,
                   I_lost_my_value = action})
      end

  fun eq (x:t, y:t) =
      let
        val {Id = idX,...} = x ()
        val {Id = idY,...} = y ()
      in
        idX = idY
      end

  fun toString (x:t) =
      let
        val {Id, Name,...} = x ()
      in
        "informant:" ^ Int.toString Id ^ ":" ^ Name
      end

  fun informAboutValue (x:t) =
      let
        val {I_have_a_value,...} = x ()
      in
        I_have_a_value ()
      end

  fun informAboutNoValue (x:t) =
      let
        val {I_lost_my_value,...} = x ()
      in
        I_lost_my_value ()
      end
end;

structure Connector =
struct
  structure V = Value
  structure I = Informant

  type t = unit -> {Id : int,
                    Name : string,
                    HasValue : unit -> bool,
                    GetValue : unit -> V.t,
                    SetValue : V.t * I.t -> unit,
                    Forget   : I.t -> unit,
                    Connect  : I.t -> unit}
  local
    val idref = ref 0;
  in
  fun newId () =
      let
        val id = !idref
      in
        idref := id + 1; id
      end
  end

  val null : t =
      let
        val id = newId ()
        val pred = fn () => false
        val getter = fn () => V.null
        val action = fn _ => ()
      in
        (fn () => {Id = id,
                   Name = "null",
                   HasValue = pred,
                   GetValue = getter,
                   SetValue = action,
                   Forget   = action,
                   Connect  = action})
      end

  fun eq (x:t, y:t) =
      let
        val {Id = idX,...} = x ()
        val {Id = idY,...} = y ()
      in
        idX = idY
      end

  fun toString (x:t) =
      let
        val {Id, Name,...} = x ()
      in
        "connector:" ^ Int.toString Id ^ ":" ^ Name
      end

  fun hasValue (x:t) =
      let
        val {HasValue,...} = x ()
      in
        HasValue ()
      end

  fun getValue (x:t) =
      let
        val {GetValue,...} = x ()
      in
        GetValue ()
      end

  fun setValue (x:t) (newValue, informant) =
      let
        val {SetValue,...} = x ()
      in
        SetValue (newValue, informant)
      end

  fun forgetValue (x:t) retractor =
      let
        val {Forget,...} = x ()
      in
        Forget retractor
      end

  fun connect (x:t) newConstraint =
      let
        val {Connect,...} = x ()
      in
        Connect newConstraint
      end
end;

structure ConstraintSystem :>
          CONSTRAINT_SYSTEM where type value = Value.t =
struct
  structure V = Value
  structure I = Informant
  structure C = Connector

  type value = Value.t
  type informant = Informant.t
  type connector = Connector.t

  fun makeConnector name =
      let
        fun memq (_, nil) = false
          | memq (c, h::t) = I.eq (c, h) orelse memq (c, t)
        fun forEachExcept (except, proc, lst) =
            let
              fun loop nil = ()
                | loop (c::cs) =
                  if I.eq (c, except) then
                    (U.log ("forEachExcept: " ^
                            I.toString c ^ " -> ignored");
                     loop cs)
                   else
                     (proc c;
                      U.log ("forEachExcept: " ^
                             I.toString c ^ " -> processed");
                      loop cs)
            in
              loop lst
            end
        val value = ref V.null
        val informant = ref I.null
        val constraints = ref nil
        val id = C.newId ()
        fun me () = {Id = id,
                     Name = name,
                     HasValue = pred,
                     GetValue = getter,
                     SetValue = setter,
                     Forget   = forgetter,
                     Connect  = connector}
        and pred () = not (I.eq (!informant, I.null))
        and getter () = !value
        and setter (newValue, newInformant) =
            if not (C.hasValue me) then
              (value := newValue;
               informant := newInformant;
               forEachExcept (newInformant,
                              I.informAboutValue,
                              !constraints);
               U.log ("set: " ^ C.toString me ^ " -> done"))
            else if not (V.eq (!value, newValue)) then
              raise Fail ("Contradiction: " ^ C.toString me ^
                          "; expected = " ^ V.toString (!value) ^
                          ", found = " ^ V.toString newValue)
            else
              U.log ("set: " ^ C.toString me ^ " -> ignored")
        and forgetter retractor =
            if I.eq (retractor, !informant) then
              (informant := I.null;
               forEachExcept (retractor,
                              I.informAboutNoValue,
                              !constraints);
               U.log ("forget: " ^ C.toString me ^ " -> done"))
            else
              U.log ("forget: " ^ C.toString me ^ " -> ignored")
        and connector newConstraint =
            (if not (memq (newConstraint, !constraints)) then
               constraints := newConstraint :: (!constraints)
             else
               ();
             if C.hasValue me then
               I.informAboutValue newConstraint
             else
               ();
             U.log ("connect: " ^ C.toString me ^ " -> done"))
      in
        me
      end

  fun makeUser name =
      let
        val id = I.newId ()
        val action = fn () => ()
        fun me () = {Id = id,
                     Name = name,
                     I_have_a_value  = action,
                     I_lost_my_value = action}
      in
        me
      end

  val hasValue = C.hasValue

  val getValue = C.getValue

  val setValue = C.setValue

  val forgetValue = C.forgetValue

  fun adder (a1, a2, sum) =
      let
        val id = I.newId ()
        fun me () = {Id = id,
                     Name = "adder",
                     I_have_a_value  = processNewValue,
                     I_lost_my_value = processForgetValue}
        and processNewValue () =
            if C.hasValue a1 andalso
               C.hasValue a2 then
              C.setValue sum (V.add (C.getValue a1,
                                     C.getValue a2), me)
            else if C.hasValue a1 andalso
                    C.hasValue sum then
              C.setValue a2 (V.sub (C.getValue sum,
                                    C.getValue a1), me)
            else if C.hasValue a2 andalso
                    C.hasValue sum then
              C.setValue a1 (V.sub (C.getValue sum,
                                    C.getValue a2), me)
            else
              ()
        and processForgetValue () =
            (C.forgetValue sum me;
             C.forgetValue a1 me;
             C.forgetValue a2 me;
             processNewValue ())
      in
        C.connect a1 me;
        C.connect a2 me;
        C.connect sum me;
        me
      end

  fun multiplier (m1, m2, product) =
      let
        val id = I.newId ()
        fun me () = {Id = id,
                     Name = "multiplier",
                     I_have_a_value  = processNewValue,
                     I_lost_my_value = processForgetValue}
        and processNewValue () =
            if (C.hasValue m1 andalso
                V.eq (C.getValue m1, V.zero))
               orelse
               (C.hasValue m2 andalso
                V.eq (C.getValue m2, V.zero)) then
              C.setValue product (V.zero, me)
            else if C.hasValue m1 andalso
                    C.hasValue m2 then
              C.setValue product (V.mul (C.getValue m1,
                                         C.getValue m2), me)
            else if C.hasValue product andalso
                    C.hasValue m1 then
              C.setValue m2 (V.quo (C.getValue product,
                                    C.getValue m1), me)
            else if C.hasValue product andalso
                    C.hasValue m2 then
              C.setValue m1 (V.quo (C.getValue product,
                                    C.getValue m2), me)
            else
              ()
        and processForgetValue () =
            (C.forgetValue product me;
             C.forgetValue m1 me;
             C.forgetValue m2 me;
             processNewValue())
      in
        C.connect m1 me;
        C.connect m2 me;
        C.connect product me;
        me
      end

  fun constant (value, connector) =
      let
        val id = I.newId ()
        fun me () = {Id = id,
                     Name = "constant",
                     I_have_a_value  = unknown1,
                     I_lost_my_value = unknown2}
        and unknown1 () = raise Fail "Unknown request(1)"
        and unknown2 () = raise Fail "Unknown request(2)"
      in
        C.connect connector me;
        C.setValue connector (value, me);
        me
      end

  fun probe (name, connector) =
      let
        fun printProbe value =
            print ("probe: " ^ name ^ " = " ^ V.toString value ^ "\n")
        val id = I.newId ()
        fun me () = {Id = id,
                     Name = "probe",
                     I_have_a_value  = processNewValue,
                     I_lost_my_value = processForgetValue}
        and processNewValue () =
            printProbe (C.getValue connector)
        and processForgetValue () =
            printProbe V.null
      in
        C.connect connector me;
        me
      end
end;

structure CS = ConstraintSystem;

fun sampleConstraint () =
    let
      val user = CS.makeUser "user"
      val c = CS.makeConnector "C"
      and f = CS.makeConnector "F"
      fun celsiusFahrenheitConverter () =
          (*
           * 9C = 5(F - 32):
           * C -> c
           * F -> f
           * 9 -> w
           * 5 -> x
           * 32 -> w
           * 9C -> u = w * c
           * F - 32 -> v = f - y
           * 5(F - 32) -> u = x * v
           *)
          let
            val u = CS.makeConnector "u"
            and v = CS.makeConnector "v"
            and w = CS.makeConnector "w"
            and x = CS.makeConnector "x"
            and y = CS.makeConnector "y"
          in
            CS.multiplier (c, w, u); (* c * w = u *)
            CS.multiplier (v, x, u); (* v * x = u *)
            CS.adder (v, y, f); (* v + y = f *)
            CS.constant (Value.Int 9, w); (* w = 9 *)
            CS.constant (Value.Int 5, x); (* x = 5 *)
            CS.constant (Value.Int 32, y); (* y = 32 *)
            U.log "celsiusFahrenheitConverter: OK"
          end
    in
      celsiusFahrenheitConverter ();
      CS.probe ("Celsius temp", c);
      CS.probe ("Fahrenheit temp", f);
      CS.setValue c (Value.Int 25, user);
      CS.setValue f (Value.Int 212, user)
      handle (Fail msg) => print ("Error! " ^ msg ^ "\n");
      CS.forgetValue c user;
      CS.setValue f (Value.Int 212, user)
    end;

sampleConstraint ();
