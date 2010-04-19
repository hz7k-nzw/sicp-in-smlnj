(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *)

structure Util = struct

  structure Int = struct
    fun square x:int = x * x
    fun cube x:int = x * x * x
    val abs = Int.abs
    fun positive x = x > 0
    fun negative x = x < 0
    fun divisible (x, y) = (x mod y) = 0
    fun even n = divisible (n, 2)
    fun odd n = not (even n)
    fun gcd (a, b) =
        if b = 0 then a else gcd (b, a mod b)
(*
    fun fib k =
        if k = 0 then 0
        else if k = 1 then 1
        else fib (k - 1) + fib (k - 2)
*)
  end

  structure Real = struct
    fun square x:real = x * x
    fun cube x:real = x * x * x
    val abs = Real.abs
    fun positive x = x > 0.0
    fun negative x = x < 0.0
    fun average (x, y) = (x + y) / 2.0
  end

  local
    val seed = Random.rand (0, 1)
  in
  (* Returns a nonnegative integer less than its integer input. *)
  fun random n = Random.randRange (0, n - 1) seed
  end

  (* Identity function. *)
  fun identity x = x

  local
    val debug = ref false
  in
  (* Returns the current debug flag. *)
  fun isDebug () = !debug
  (* Assigns the given value to the debug flag. *)
  fun setDebug f = debug := f
  (* Simple logger: string -> unit *)
  fun log msg = if !debug then (print msg; print "\n") else ()
  end

  (* assoc : ''a -> (''a * 'b) list -> (''a * 'b) option *)
  fun assoc key nil = NONE
    | assoc key ((p as (key', _))::ps) =
      if key = key' then SOME p
      else assoc key ps
end;
