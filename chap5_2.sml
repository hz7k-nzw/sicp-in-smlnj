(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml;
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 5  Computing with Register Machines *)
(* 5.2  A Register-Machine Simulator *)
(* 5.2.1  The Machine Model *)

(* => see REGISTER_MACHINE defined below *)

(* 5.2.2  The Assembler *)

(* => see RegisterMachineFn defined below *)

(* 5.2.3  Generating Execution Procedures for Instructions *)

(* => see RegisterMachineFn defined below *)

signature REGISTER_MACHINE =
sig
  (* type of user data *)
  type value

  (* type of instruction *)
  type inst = (unit -> unit) ref

  (* type of register contents *)
  datatype data
    = Value of value
    | Insts of inst list

  (* type of operator *)
  type ope = data list -> data

  (* type of parameters for controller-text *)
  datatype param
    = R of string (* register *)
    | C of value (* constant *)
    | L of string (* label *)

  (* type of controller-text *)
  datatype ctrl
    = Label of string
    | Assign of string * param
    | AssignOp of string * string * param list
    | Test of string * param list
    | Branch of param
    | Goto of param
    | Save of string
    | Restore of string
    | Perform of string * param list

  (* type of register machine model *)
  type mm

  (* debug flag *)
  val debug : bool ref

  (* constructs and returns a model of the machine with
   * the given registers, operations, and controller. *)
  val makeMachine : string list * (string * ope) list * ctrl list -> mm

  (* simulates the execution of the given machine, starting
   * from the beginning of the controller sequence and stopping
   * when it reaches the end of the sequence. *)
  val start : mm -> unit

  (* transforms the sequence of controller expressions for
   * a machine into a corresponding list of machine instructions,
   * each with its execution procedure. *)
  val assemble : mm -> ctrl list -> inst list

  (* allocate-register *)
  val allocateReg : mm -> string -> unit

  (* install-instruction-sequence *)
  val installInsts : mm -> inst list -> unit

  (* install-operations *)
  val installOps : mm -> (string * ope) list -> unit

  (* returns the contents of a simulated register in the given machine. *)
  val getRegisterContents : mm -> string -> data

  (* stores a value in a simulated register in the given machine. *)
  val setRegisterContents : mm -> string -> data -> unit

  (* from function to operator (0-arg) *)
  val fromFn0 : string * (unit -> value)
                -> string * ope

  (* from function to operator (1-arg) *)
  val fromFn1 : string * (value -> value)
                -> string * ope

  (* from function to operator (2-args) *)
  val fromFn2 : string * (value * value -> value)
                -> string * ope

  (* from function to operator (3-args) *)
  val fromFn3 : string * (value * value * value -> value)
                -> string * ope

  val printParam : (TextIO.outstream * param) -> unit
  val printCtrl : (TextIO.outstream * ctrl) -> unit
end;

signature REGISTER =
sig
  type 'a t
  val make : string -> 'a t
  val name : 'a t -> string
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
end;

signature STACK =
sig
  type 'a t
  val make : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val initialize : 'a t -> unit
  val printStats : 'a t -> unit
end;

signature REGISTER_MACHINE_CONF =
sig
  (* sub structure: register *)
  structure Register : REGISTER
  (* sub structure: stack *)
  structure Stack : STACK
  (* type of user data *)
  type value
  (* user value that specifies "undef" *)
  val undef: value
  (* predicate to test if the specified value is true *)
  val isTrue : value -> bool
  (* value printer *)
  val printValue : (TextIO.outstream * value) -> unit
end;

functor RegisterMachineFn (Conf: REGISTER_MACHINE_CONF)
        : REGISTER_MACHINE =
struct
  open Conf

  (* type of instruction *)
  type inst = (unit -> unit) ref

  (* type of register contents *)
  datatype data
    = Value of value
    | Insts of inst list

  (* type of operator *)
  type ope = data list -> data

  (* type of parameters for controller-text *)
  datatype param
    = R of string (* register *)
    | C of value (* constant *)
    | L of string (* label *)

  (* type of controller-text *)
  datatype ctrl
    = Label of string
    | Assign of string * param
    | AssignOp of string * string * param list
    | Test of string * param list
    | Branch of param
    | Goto of param
    | Save of string
    | Restore of string
    | Perform of string * param list

  (* type of register machine model *)
  type mm =
       {PC : inst list Register.t, (* special register: pc *)
        FLAG : bool Register.t, (* special register: flag *)
        STACK : data Stack.t,
        REG_TABLE : (string * data Register.t) list ref,
        THE_INSTS : inst list ref,
        THE_OPS : (string * ope) list ref}

  (* debug flag *)
  val debug : bool ref = ref false

  (* dummy proc used to init instruction *)
  val DUMMY_PROC =
      (fn () => raise Fail "DUMMY_PROC should not be called")

  (* print functions *)
  local
    val p = TextIO.output
    val flush = TextIO.flushOut
    val logOut = TextIO.stdOut
  in
  fun printParam (out, R r) =
      (p (out, "(reg ");
       p (out,  r);
       p (out, ")"))
    | printParam (out, C v) =
      (p (out, "(const ");
       printValue (out, v);
       p (out, ")"))
    | printParam (out, L l) =
      (p (out, "(label ");
       p (out,  l);
       p (out, ")"))

  fun printParams (out, params) =
      let
        fun p1 nil = ()
          | p1 (param::params) = (printParam (out, param);
                                  p2 params)
        and p2 nil = ()
          | p2 (param::params) = (p (out, " ");
                                  printParam (out, param);
                                  p2 params)
      in
        p1 params
      end

  fun printCtrl (out, Label l) =
      p (out, l)
    | printCtrl (out, Assign (r, param)) =
      (p (out, "(assign ");
       p (out, r);
       p (out, " ");
       printParam (out, param);
       p (out, ")"))
    | printCtrl (out, AssignOp (r, ope, params)) =
      (p (out, "(assign ");
       p (out, r);
       p (out, " (op ");
       p (out, ope);
       p (out, ") ");
       printParams (out, params);
       p (out, ")"))
    | printCtrl (out, Test (ope, params)) =
      (p (out, "(test ");
       p (out, "(op ");
       p (out, ope);
       p (out, ") ");
       printParams (out, params);
       p (out, ")"))
    | printCtrl (out, Branch param) =
      (p (out, "(branch ");
       printParam (out, param);
       p (out, ")"))
    | printCtrl (out, Goto param) =
      (p (out, "(goto ");
       printParam (out, param);
       p (out, ")"))
    | printCtrl (out, Save r) =
      (p (out, "(save ");
       p (out, r);
       p (out, ")"))
    | printCtrl (out, Restore r) =
      (p (out, "(restore ");
       p (out, r);
       p (out, ")"))
    | printCtrl (out, Perform (ope, params)) =
      (p (out, "(perform ");
       p (out, "(op ");
       p (out, ope);
       p (out, ") ");
       printParams (out, params);
       p (out, ")"))

  fun log (msg, ctrl) =
      if !debug then
        (p (logOut, ";;; "^msg^": ");
         printCtrl (logOut, ctrl);
         p (logOut, "\n");
         flush logOut)
      else
        ()
  end

  fun toValue (Value v) = v
    | toValue (Insts _) = raise Fail "Insts specified (expected: Value)"
  fun toInsts (Insts i) = i
    | toInsts (Value _) = raise Fail "Value specified (expected: Insts)"

  (* lookup-register (get-register) *)
  fun lookupReg ({REG_TABLE,...}:mm) name : data Register.t =
      case U.assoc name (!REG_TABLE) of
        SOME (_, reg) => reg
      | NONE => raise Fail ("Unknown register: "^name)

  (* lookup-prim *)
  fun lookupOpe ({THE_OPS,...}:mm) name : ope =
      case U.assoc name (!THE_OPS) of
        SOME (_, ope) => ope
      | NONE => raise Fail ("Unknown operator: "^name)

  (* allocate-register *)
  fun allocateReg ({REG_TABLE,...}:mm) name =
      case U.assoc name (!REG_TABLE) of
        SOME _ => raise Fail ("Multiply defined register: "^name)
      | NONE => REG_TABLE := ((name, Register.make name) :: (!REG_TABLE))

  (* install-instruction-sequence *)
  fun installInsts ({THE_INSTS,...}:mm) seq = THE_INSTS := seq

  (* install-operations *)
  fun installOps ({THE_OPS,...}:mm) ops = THE_OPS := (ops @ (!THE_OPS))
(*
  (* stack *)
  fun stack ({STACK,...}:mm) = STACK

  (* operations *)
  fun ops ({THE_OPS,...}:mm) = !THE_OPS
*)
  (* make-execution-procedure *)
  fun makeExecProc (m as {PC,FLAG,STACK,...}:mm) (ctrl, labels) =
      case ctrl of
        Assign (name, param) =>
        let
          val reg = lookupReg m name
          val proc = makePrimProc m (param, labels)
        in
          (fn () => (log ("exec", ctrl);
                     Register.set reg (proc ());
                     advancePc PC))
        end
      | AssignOp (name, ope, params) =>
        let
          val reg = lookupReg m name
          val proc = makeOpeProc m (ope, params, labels)
        in
          (fn () => (log ("exec", ctrl);
                     Register.set reg (proc ());
                     advancePc PC))
        end
      | Test (ope, params) =>
        let
          val proc = makeOpeProc m (ope, params, labels)
        in
          (fn () => (log ("exec", ctrl);
                     Register.set FLAG ((isTrue o toValue o proc) ());
                     advancePc PC))
        end
      | Branch param =>
        (case param of
           L name =>
           let
             val insts = lookupLabel labels name
           in
             (fn () => (log ("exec", ctrl);
                        if Register.get FLAG
                        then Register.set PC insts
                        else advancePc PC))
           end
         | R _ => raise Fail "Bad branch (register specified)"
         | C _ => raise Fail "Bad branch (constant specified)"
        )
      | Goto param =>
        (case param of
           L name =>
           let
             val insts = lookupLabel labels name
           in
             (fn () => (log ("exec", ctrl);
                        Register.set PC insts))
           end
         | R name =>
           let
             val reg = lookupReg m name
           in
             (fn () => (log ("exec", ctrl);
                        Register.set PC (toInsts (Register.get reg))))
           end
         | C _ => raise Fail "Bad goto (constant specified)"
        )
      | Save name =>
        let
          val reg = lookupReg m name
        in
          (fn () => (log ("exec", ctrl);
                     Stack.push STACK (Register.get reg);
                     advancePc PC))
        end
      | Restore name =>
        let
          val reg = lookupReg m name
        in
          (fn () => (log ("exec", ctrl);
                     Register.set reg (Stack.pop STACK);
                     advancePc PC))
        end
      | Perform (ope, params) =>
        let
          val proc = makeOpeProc m (ope, params, labels)
        in
          (fn () => (log ("exec", ctrl);
                     proc (); advancePc PC))
        end
      | Label name =>
        raise Fail ("Unexpected label: "^name)

  (* lookup-label *)
  and lookupLabel labels name : inst list =
      case U.assoc name labels of
        SOME (_, insts) => insts
      | NONE => raise Fail ("Unknown label: "^name)

  (* advance-pc *)
  and advancePc pc =
      case Register.get pc of
       (_::insts) => Register.set pc insts
     | nil => raise Fail "Empty insts: PC"

  (* make-primitive-exp *)
  and makePrimProc m (param, labels) =
      case param of
        C value =>
        (fn () => Value value)
      | L name =>
        let
          val insts = lookupLabel labels name
        in
          (fn () => Insts insts)
        end
      | R name =>
        let
          val reg = lookupReg m name
        in
          (fn () => Register.get reg)
        end

  (* make-operation-exp *)
  and makeOpeProc m (name, params, labels) =
      let
        val ope = lookupOpe m name
        val paramToProc =
            (fn param => makePrimProc m (param, labels))
        val procs = map paramToProc params
        val procToData =
            (fn proc => proc ())
      in
        (fn () => ope (map procToData procs))
      end

  (* assemble *)
  fun assemble m ctrls =
      let
        fun receive (srcs, insts, labels) =
            (updateInsts m (srcs, insts, labels); insts)
      in
        extractLabels ctrls receive
      end

  (* extract-labels *)
  and extractLabels nil receive = receive (nil, nil, nil)
    | extractLabels (ctrl::ctrls) receive =
      let
        fun receive' (srcs, insts, labels) =
            case ctrl of
              Label name =>
              receive (srcs, insts, (name, insts)::labels)
            | _ =>
              receive (ctrl::srcs, (ref DUMMY_PROC)::insts, labels)
      in
        extractLabels ctrls receive'
      end

  (* update-insts *)
  and updateInsts m (srcs, insts, labels) =
      let
        fun updator (src, procRef) =
            let
              val proc = makeExecProc m (src, labels)
            in
              procRef := proc
            end
      in
        ListPair.appEq updator (srcs, insts)
      end

  (* make-machine *)
  fun makeMachine (regNames, ops, ctrls) : mm =
      let
        val m = makeNewMachine ()
      in
        app (fn regName => allocateReg m regName) regNames;
        installOps m ops;
        installInsts m (assemble m ctrls);
        m
      end

  (* make-new-machine *)
  and makeNewMachine unit : mm =
      let
        val pc = Register.make "pc"
        val flag = Register.make "flag"
        val stack = Stack.make ()
        val reg_table = nil
        val insts = nil
        val ops = [("initialize-stack",
                    (fn _ => (Stack.initialize stack; Value undef))),
                   ("print-stack-stats",
                    (fn _ => (Stack.printStats stack; Value undef)))]
      in
        {PC = pc,
         FLAG = flag,
         STACK = stack,
         REG_TABLE = ref reg_table,
         THE_INSTS = ref insts,
         THE_OPS = ref ops}
      end

  (* start *)
  fun start (m as {PC,THE_INSTS,...}:mm) =
      (Register.set PC (!THE_INSTS); execute m)

  (* execute *)
  and execute (m as {PC,...}:mm) =
      case Register.get PC of
        nil => ()
      | (procRef::insts) =>
        let
          val proc = !procRef
        in
          (proc (); execute m)
        end

  (* get-register-contents *)
  fun getRegisterContents m name =
        let
          val reg = lookupReg m name
        in
          Register.get reg
        end

  (* set-register-contents! *)
  fun setRegisterContents m name =
        let
          val reg = lookupReg m name
        in
          (fn data => Register.set reg data)
        end

  fun fromFn0 (name: string, f: unit -> value)
      : string * ope =
      let
        fun ope [] = Value (f ())
          | ope _ = raise Fail ("Unexpected arguments: "^name)
      in
        (name, ope)
      end
  fun fromFn1 (name: string, f: value -> value)
      : string * ope =
      let
        fun ope [n1] = Value (f (toValue n1))
          | ope _ = raise Fail ("Unexpected arguments: "^name)
      in
        (name, ope)
      end
  fun fromFn2 (name:string, f: value * value -> value)
      : string * ope =
      let
        fun ope [n1,n2] = Value (f (toValue n1, toValue n2))
          | ope _ = raise Fail ("Unexpected arguments: "^name)
      in
        (name, ope)
      end
  fun fromFn3 (name:string, f: value * value * value -> value)
      : string * ope =
      let
        fun ope [n1,n2,n3] = Value (f (toValue n1, toValue n2, toValue n3))
          | ope _ = raise Fail ("Unexpected arguments: "^name)
      in
        (name, ope)
      end
end;

structure Register :> REGISTER =
struct
  type 'a t = string * 'a option ref

  fun make name = (name, ref NONE)
  fun name (name, _) = name
  fun get (name, vref) =
      case !vref of
        SOME v => v
      | NONE => raise Fail ("Empty register: "^name)
  fun set (_, vref) newVal =
      vref := (SOME newVal)
end;

structure Stack :> STACK =
struct
  type 'a t = 'a list ref

  fun make () =
      ref nil
  fun push stack newVal =
      stack := newVal :: (!stack)
  fun pop stack =
      case !stack of
        (hd::tl) => (stack := tl; hd)
      | nil => raise Fail "Empty stack"
  fun initialize stack =
      stack := nil
  fun printStats _ =
      U.log "Not supported: printStats"
end;

(* 5.2.4  Monitoring Machine Performance *)

structure Stack' :> STACK =
struct
  type 'a t = {Data : 'a list ref,
               NumberPushes : int ref,
               MaxDepth : int ref,
               CurrentDepth : int ref}

  fun make () =
      {Data = ref nil,
       NumberPushes = ref 0,
       MaxDepth = ref 0,
       CurrentDepth = ref 0}

  fun push ({Data,NumberPushes,MaxDepth,CurrentDepth}:'a t)
           newVal =
      (Data := newVal :: !Data;
       NumberPushes := (!NumberPushes) + 1;
       CurrentDepth := (!CurrentDepth) + 1;
       MaxDepth := Int.max (!CurrentDepth, !MaxDepth))

  fun pop ({Data,CurrentDepth,...}:'a t) =
      case !Data of
        (hd::tl) => (Data := tl;
                     CurrentDepth := (!CurrentDepth) - 1;
                     hd)
      | nil => raise Fail "Empty stack"

  fun initialize ({Data,NumberPushes,MaxDepth,CurrentDepth}:'a t) =
      (Data := nil;
       NumberPushes := 0;
       CurrentDepth := 0;
       MaxDepth := 0)

  fun printStats ({NumberPushes,MaxDepth,...}:'a t) =
      print ("stack-stats: " ^
             "total-pushes=" ^ Int.toString (!NumberPushes) ^ " " ^
             "max-depth=" ^ Int.toString (!MaxDepth) ^ "\n")
end;
