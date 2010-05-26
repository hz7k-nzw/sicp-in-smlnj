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

  (* type of operator *)
  type ope = value list -> value

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

  (* constructs and returns a model of the machine with
   * the given registers, operations, and controller. *)
  val make : string list * (string * ope) list * ctrl list -> mm

  (* simulates the execution of the given machine, starting
   * from the beginning of the controller sequence and stopping
   * when it reaches the end of the sequence. *)
  val start : mm -> unit

  (* returns the contents of a simulated register in the given machine. *)
  val getRegisterContents : mm -> string -> value

  (* stores a value in a simulated register in the given machine. *)
  val setRegisterContents : mm -> string -> value -> unit
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
end;

functor RegisterMachineFn (Conf: REGISTER_MACHINE_CONF)
        : REGISTER_MACHINE =
struct
  open Conf

  (* type of operator *)
  type ope = value list -> value

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

  (* type of instruction *)
  type inst = ctrl * (unit -> unit) ref

  (* type of label *)
  type label = string * inst list

  (* type of register contents *)
  datatype data
    = Value of value
    | Insts of inst list

  (* type of register machine model *)
  type mm =
       {PC : inst list Register.t, (* special register: pc *)
        FLAG : bool Register.t, (* special register: flag *)
        STACK : data Stack.t,
        REG_TABLE : (string * data Register.t) list ref,
        THE_INSTS : inst list ref,
        THE_OPS : (string * ope) list ref}

  (* dummy proc used to init instruction *)
  val DUMMY_PROC =
      (fn () => raise Fail "DUMMY_PROC should not be called")

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
  fun installOps ({THE_OPS,...}:mm) ops = THE_OPS := (!THE_OPS @ ops)
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
          (fn () => (Register.set reg (proc ());
                     advancePc PC))
        end
      | AssignOp (name, ope, params) =>
        let
          val reg = lookupReg m name
          val proc = makeOpeProc m (ope, params, labels)
        in
          (fn () => (Register.set reg (proc ());
                     advancePc PC))
        end
      | Test (ope, params) =>
        let
          val proc = makeOpeProc m (ope, params, labels)
        in
          (fn () => (Register.set FLAG ((isTrue o toValue o proc) ());
                     advancePc PC))
        end
      | Branch param =>
        (case param of
           L name =>
           let
             val insts = lookupLabel labels name
           in
             (fn () => if Register.get FLAG
                       then Register.set PC insts
                       else advancePc PC)
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
             (fn () => Register.set PC insts)
           end
         | R name =>
           let
             val reg = lookupReg m name
           in
             (fn () => Register.set PC (toInsts (Register.get reg)))
           end
         | C _ => raise Fail "Bad goto (constant specified)"
        )
      | Save name =>
        let
          val reg = lookupReg m name
        in
          (fn () => (Stack.push STACK (Register.get reg);
                     advancePc PC))
        end
      | Restore name =>
        let
          val reg = lookupReg m name
        in
          (fn () => (Register.set reg (Stack.pop STACK);
                     advancePc PC))
        end
      | Perform (ope, params) =>
        let
          val proc = makeOpeProc m (ope, params, labels)
        in
          (fn () => (proc (); advancePc PC))
        end
      | Label name =>
        raise Fail ("Unexpected label: "^name)

  and lookupLabel labels name : inst list =
      case U.assoc name labels of
        SOME (_, insts) => insts
      | NONE => raise Fail ("Unknown label: "^name)

  and advancePc pc =
      case Register.get pc of
       (_::insts) => Register.set pc insts
     | nil => raise Fail "Empty insts: PC"

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

  and makeOpeProc m (name, params, labels) =
      let
        val ope = lookupOpe m name
        val paramToProc =
            (fn param => makePrimProc m (param, labels))
        val procs = map paramToProc params
        val procToValue =
            (fn proc => toValue (proc ()))
      in
        (fn () => Value (ope (map procToValue procs)))
      end

  (* assemble *)
  fun assemble m ctrls =
      let
        fun receive (insts, labels) =
            (updateInsts m (insts, labels); insts)
      in
        extractLabels ctrls receive
      end

  (* extract-labels *)
  and extractLabels nil receive = receive (nil, nil)
    | extractLabels (ctrl::ctrls) receive =
      let
        fun receive' (insts, labels) =
            case ctrl of
              Label name =>
              receive (insts, (name, insts)::labels)
            | _ =>
              receive ((ctrl, ref DUMMY_PROC)::insts, labels)
      in
        extractLabels ctrls receive'
      end

  (* update-insts *)
  and updateInsts m (insts, labels) =
      let
        fun updator (ctrl, procRef) =
            let
              val proc = makeExecProc m (ctrl, labels)
            in
              procRef := proc
            end
      in
        app updator insts
      end

  (* make-machine *)
  fun make (regNames, ops, ctrls) : mm =
      let
        val m = makeNew ()
      in
        app (fn regName => allocateReg m regName) regNames;
        installOps m ops;
        installInsts m (assemble m ctrls);
        m
      end

  (* make-new-machine *)
  and makeNew unit : mm =
      let
        val pc = Register.make "pc"
        val flag = Register.make "flag"
        val stack = Stack.make ()
        val reg_table = nil
        val insts = nil
        val ops = [("initialize-stack",
                    (fn _ => (Stack.initialize stack; undef))),
                   ("print-stack-stats",
                    (fn _ => (Stack.printStats stack; undef)))]
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
      | ((_, procRef)::insts) =>
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
          toValue (Register.get reg)
        end

  (* set-register-contents! *)
  fun setRegisterContents m name =
        let
          val reg = lookupReg m name
        in
          (fn value => Register.set reg (Value value))
        end
end;

structure Ope =
struct
  fun fromFn0 (name: string, f: unit -> 'a)
      : string * ('a list -> 'a) =
      let
        fun ope [] = f ()
          | ope _ = raise Fail ("Unexpected arguments: "^name)
      in
        (name, ope)
      end
  fun fromFn1 (name: string, f: 'a -> 'a)
      : string * ('a list -> 'a) =
      let
        fun ope [n1] = f n1
          | ope _ = raise Fail ("Unexpected arguments: "^name)
      in
        (name, ope)
      end
  fun fromFn2 (name:string, f: 'a * 'a -> 'a)
      : string * ('a list -> 'a) =
      let
        fun ope [n1,n2] = f (n1,n2)
          | ope _ = raise Fail ("Unexpected arguments: "^name)
      in
        (name, ope)
      end
  fun fromFn3 (name:string, f: 'a * 'a * 'a -> 'a)
      : string * ('a list -> 'a) =
      let
        fun ope [n1,n2,n3] = f (n1,n2,n3)
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
