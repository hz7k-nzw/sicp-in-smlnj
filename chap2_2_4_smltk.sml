(*
 * SICP in SML/NJ
 * by Kenji Nozawa (hz7k-nzw@asahi-net.or.jp)
 *
 * Dependency: util.sml;
 *
 * Notice: The following code runs under *sml_tk*.
 * To allow compilation (on SML/NJ) with the latest SML Basis
 * Library (v110.72), some patches neeed to be applied to the
 * sml_tk sources. See the following URL for detail:
 * http://www.asahi-net.or.jp/~hz7k-nzw/sml_tk/index.html
 *)

structure U = Util;
structure I = Util.Int;
structure R = Util.Real;

(* 2  Building Abstractions with Data *)
(* 2.2  Hierarchical Data and the Closure Property *)
(* 2.2.4  Example: A Picture Language *)

structure Vector = struct
  type t = real * real
  fun make (x:real, y:real) = (x, y)
  fun xcor ((x, _):t) = x
  fun ycor ((_, y):t) = y
  fun scale (a, v) = (a * (xcor v), a * (ycor v))
  fun add (v1, v2) = make ((xcor v1) + (xcor v2),
                           (ycor v1) + (ycor v2))
  fun sub (v1, v2) = make ((xcor v1) - (xcor v2),
                           (ycor v1) - (ycor v2))
  fun scalarProduct ((x1, y1):t, (x2, y2):t) = x1 * x2 + y1 * y2
  fun toString v = "(" ^ (Real.toString (xcor v)) ^
                   " " ^ (Real.toString (ycor v)) ^
                   ")"
end;

structure Frame = struct
  structure V = Vector
  type t = V.t * V.t * V.t
  fun make (origin:V.t, edge1:V.t, edge2:V.t) = (origin, edge1, edge2)
  fun origin ((origin, _, _):t) = origin
  fun edge1 ((_, edge1, _):t) = edge1
  fun edge2 ((_, _, edge2):t) = edge2
  fun frameCoordMap frame : V.t -> V.t =
   fn v => V.add (origin frame,
                  V.add (V.scale (V.xcor v, edge1 frame),
                         V.scale (V.ycor v, edge2 frame)))
  fun toString f = "(" ^ (V.toString (origin f)) ^
                   " " ^ (V.toString (edge1 f)) ^
                   " " ^ (V.toString (edge2 f)) ^
                   ")"
end;

structure Segment = struct
  structure V = Vector
  type t = V.t * V.t
  fun make (p1:V.t, p2:V.t) = (p1, p2)
  fun p1 ((p1, _):t) = p1 (* start-segment *)
  fun p2 ((_, p2):t) = p2 (* end-segment *)
  fun toString s = "(" ^ (V.toString (p1 s)) ^
                   " " ^ (V.toString (p2 s)) ^
                   ")"
end;

structure Gui = struct
  structure V = Vector
  structure F = Frame

  open SmlTk

  datatype context = Ctx of WinId * WidId

  fun drawLine (Ctx (_,canvasId)) (v1, v2) =
      let
        val p1 = mkCoord (Real.round (V.xcor v1),
                          Real.round (V.ycor v1))
        val p2 = mkCoord (Real.round (V.xcor v2),
                          Real.round (V.ycor v2))
        val line = CLine {citemId = newCItemId(),
                          coords = [p1, p2],
                          configs = [FillColor Black],
                          bindings = []}
      in
        addCItem canvasId line
      end

  fun makeCanvas canvasId =
      Canvas {widId = canvasId,
              scrolltype = NoneScb,
              citems = [],
              packings = [Side Top, Fill X, Expand true],
              configs = [Height 300, Width 400, Relief Groove,
                         Background (Mix{red=200, blue=240, green=240})],
              bindings = [] }

  fun makeQuitButton winId =
      Button {widId = newWidgetId(),
              packings = [Side Top, Fill X, Expand true],
              configs = [Text "Quit",
                         Command (mkSimpleAction
                                      (fn () => closeWindow winId))],
              bindings = []}

  fun makeWindow painter =
      let
        val winId = newWinId()
        val canvasId = newWidgetId()
        val ctx = Ctx (winId, canvasId)
        val frame = F.make (V.make (0.0, 300.0),
                            V.make (400.0, 0.0),
                            V.make (0.0, ~300.0))
        val action = (fn _ => (U.log "initAction called!";
                               painter ctx frame))
      in
        mkWindow {winId = winId,
                  config= [WinTitle "Painter",
                           WinAspect(4,3,4,3),
                           WinMinSize(400,300),
                           WinMaxSize(500,400)],
                  widgets = Pack [makeCanvas canvasId,
                                  makeQuitButton winId],
                  bindings = [],
                  init = action}
      end

  fun go painter =
      let
        val win = makeWindow painter
      in
        startTclExn [ win ]
      end
end;

structure Painter = struct
  structure V = Vector
  structure F = Frame
  structure S = Segment
  structure G = Gui

  (* Type of painter: My painter depends on G.context. *)
  type t = G.context -> F.t -> unit

  val go = G.go : t -> string

  fun segmentsPainter segmentList : t =
      let
        fun p ctx frame =
            let
              val m = F.frameCoordMap frame
            in
              app (fn segment =>
                      let
                        val v1 = m (S.p1 segment)
                        and v2 = m (S.p2 segment)
                      in
                        G.drawLine ctx (v1, v2)
                      end)
                  segmentList
            end
      in
        p
      end

  fun transformPainter (painter:t)
                       (origin:V.t,
                        corner1:V.t,
                        corner2:V.t) : t =
      let
        fun p ctx frame =
            let
              val m = F.frameCoordMap frame
              val newOrigin = m origin
              val newCorner1 = V.sub (m corner1, newOrigin)
              and newCorner2 = V.sub (m corner2, newOrigin)
            in
              painter ctx (F.make (newOrigin, newCorner1, newCorner2))
            end
      in
        p
      end

  fun flipVert painter =
      transformPainter painter
                       (V.make (0.0, 1.0),
                        V.make (1.0, 1.0),
                        V.make (0.0, 0.0))

  fun flipHoriz painter =
      transformPainter painter
                       (V.make (1.0, 0.0),
                        V.make (0.0, 0.0),
                        V.make (1.0, 1.0))

  fun shrinkToUpperRight painter =
      transformPainter painter
                       (V.make (0.5, 0.5),
                        V.make (1.0, 0.5),
                        V.make (0.5, 1.0))

  fun rotate90 painter =
      transformPainter painter
                       (V.make (1.0, 0.0),
                        V.make (1.0, 1.0),
                        V.make (0.0, 0.0))

  fun rotate180 painter =
      transformPainter painter
                       (V.make (1.0, 1.0),
                        V.make (0.0, 1.0),
                        V.make (1.0, 0.0))

  fun squashInwards painter =
      transformPainter painter
                       (V.make (0.0, 0.0),
                        V.make (0.65, 0.35),
                        V.make (0.35, 0.65))

  fun beside (painter1, painter2) : t =
      let
        val splitPoint = V.make (0.5, 0.0)
        val paintLeft = transformPainter painter1
                                         (V.make (0.0, 0.0),
                                          splitPoint,
                                          V.make (0.0, 1.0))
        and paintRight = transformPainter painter2
                                          (splitPoint,
                                           V.make (1.0, 0.0),
                                           V.make (0.5, 1.0))
        fun p ctx frame = (paintLeft ctx frame; paintRight ctx frame)
      in
        p
      end

  fun below (painter1, painter2) : t =
      let
        val splitPoint = V.make (0.0, 0.5)
        val paintBottom = transformPainter painter1
                                           (V.make (0.0, 0.0),
                                            V.make (1.0, 0.0),
                                            splitPoint)
        and paintTop = transformPainter painter2
                                        (splitPoint,
                                         V.make (1.0, 0.5),
                                         V.make (0.0, 1.0))
        fun p ctx frame = (paintBottom ctx frame; paintTop ctx frame)
      in
        p
      end

  fun flippedPairs painter =
      let
        val painter2 = beside (painter, flipVert painter)
      in
        below (painter2, painter2)
      end

  fun rightSplit (painter, 0) = painter
    | rightSplit (painter, n) =
      let
        val smaller = rightSplit (painter, n - 1)
      in
        beside (painter, below (smaller, smaller))
      end

  fun upSplit (painter, 0) = painter
    | upSplit (painter, n) =
      let
        val smaller = upSplit (painter, n - 1)
      in
        below (painter, beside (smaller, smaller))
      end

  fun cornerSplit (painter, 0) = painter
    | cornerSplit (painter, n) =
      let
        val up = upSplit (painter, n - 1)
        and right = rightSplit (painter, n - 1)
        val topLeft = beside (up, up)
        and bottomRight = below (right, right)
        and corner = cornerSplit (painter, n - 1)
      in
        beside (below (painter, topLeft),
                below (bottomRight, corner))
      end

  fun squareLimit (painter, n) =
      let
        val quarter = cornerSplit (painter, n)
        val half = beside (flipHoriz quarter, quarter)
      in
        below (flipVert half, half)
      end

  fun squareOfFour (tl:(t -> t), tr:(t -> t),
                    bl:(t -> t), br:(t -> t)) =
   fn painter =>
      let
        val top = beside (tl painter, tr painter)
        and bottom = beside (bl painter, br painter)
      in
        below (bottom, top)
      end

  fun flippedPairs' painter =
      let
        val combine4 = squareOfFour (U.identity, flipVert,
                                     U.identity, flipVert)
      in
        combine4 painter
      end

  fun squareLimit' (painter, n) =
      let
        val combine4 = squareOfFour (flipHoriz, U.identity,
                                     rotate180, flipVert)
      in
        combine4 (cornerSplit (painter, n))
      end


  (*
   * Vector Mapping, which maps vectors to be
   * passed to the transformPainter function.
   *)
  structure M =
  struct
    val O  = V.make (0.0, 0.0) (* origin *)
    val Ex = V.make (1.0, 0.0) (* cornerX *)
    val Ey = V.make (0.0, 1.0) (* cornerY *)

    fun mapVectors m = (m O, m Ex, m Ey)

    fun scale a v = V.scale (a, v)

    fun translate b v = V.add (v, b)

    fun linear (x, y) v =
        V.make (V.scalarProduct (x, v),
                V.scalarProduct (y, v))

    fun rotate a v =
        let
          val x = V.make (Math.cos a, Math.sin (~a))
          val y = V.make (Math.sin a, Math.cos a)
        in
          linear (x, y) v
        end
  end

  (*
   * Sierpinski gasket
   * borrowed from: http://d.hatena.ne.jp/coze/20060820#p1
   *)
  local
    val S = M.scale (1.0 / 2.0)

    (* M1 : (x, y) -> 1/2 (x, y) *)
    val M1 = S

    (* M2 : (x, y) -> 1/2 (x, y) + (1/2, 0) *)
    val b2 = V.make (1.0 / 2.0, 0.0);
    val T2 = M.translate b2
    val M2 = T2 o S

    (* M3 : (x, y) -> 1/2 (x, y) + (1/4, sqrt(3)/4) *)
    val b3 = V.make (1.0 / 4.0, (Math.sqrt 3.0) / 4.0)
    val T3 = M.translate b3
    val M3 = T3 o S

    val vectors1 = M.mapVectors M1
    val vectors2 = M.mapVectors M2
    val vectors3 = M.mapVectors M3

    fun nextOrders painter =
        let
          val p1 = transformPainter painter vectors1
          val p2 = transformPainter painter vectors2
          val p3 = transformPainter painter vectors3
          fun p ctx frame = (p1 ctx frame;
                             p2 ctx frame;
                             p3 ctx frame)
        in
          p
        end
  in
    fun gasket (painter, 0) = painter
      | gasket (painter, n) =
        let
          val smaller = gasket (painter, n - 1)
        in
          nextOrders smaller
        end
  end

  (* Koch curve *)
  local
    val S = M.scale (1.0 / 3.0)
    val R = M.rotate (Math.pi / 3.0)
    val R' = M.rotate (~ (Math.pi / 3.0))

    (* M1 : (x, y) -> 1/3 (x, y) *)
    val M1 = S

    (* M2 : (x, y) -> 1/3 R[pi/3] (x, y) + 1/3 (1, 0) *)
    val b2 = V.make (1.0 / 3.0, 0.0)
    val T2 = M.translate b2
    val M2 = T2 o (S o R)

    (* M3 : (x, y) -> 1/3 R[-pi/3] (x, y) + 1/3 (3/2, sqrt(3)/2) *)
    val b3 = V.make (1.0 / 2.0, 1.0 / (2.0 * (Math.sqrt 3.0)))
    val T3 = M.translate b3
    val M3 = T3 o (S o R')

    (* M4 : (x, y) -> 1/3 (x, y) + 1/3 (2, 0)*)
    val b4 = V.make (2.0 / 3.0, 0.0)
    val T4 = M.translate b4
    val M4 = T4 o S

    val vectors1 = M.mapVectors M1
    val vectors2 = M.mapVectors M2
    val vectors3 = M.mapVectors M3
    val vectors4 = M.mapVectors M4

    fun nextOrders painter =
        let
          val p1 = transformPainter painter vectors1
          val p2 = transformPainter painter vectors2
          val p3 = transformPainter painter vectors3
          val p4 = transformPainter painter vectors4
          fun p ctx frame = (p1 ctx frame;
                             p2 ctx frame;
                             p3 ctx frame;
                             p4 ctx frame)
        in
          p
        end
  in
    fun koch (painter, 0) = painter
      | koch (painter, n) =
        let
          val smaller = koch (painter, n - 1)
        in
          nextOrders smaller
        end
  end
end;

structure P = Painter

(* painter for FRAME *)
local
  val p01 = P.V.make (0.00, 0.00)
  and p02 = P.V.make (1.00, 0.00)
  and p03 = P.V.make (1.00, 1.00)
  and p04 = P.V.make (0.00, 1.00)
in
val framePainter
  = P.segmentsPainter [
    P.S.make (p01,p02),
    P.S.make (p02,p03),
    P.S.make (p03,p04),
    P.S.make (p04,p01)];

(* painter for X *)
val xPainter
  = P.segmentsPainter [
    P.S.make (p01,p03),
    P.S.make (p02,p04)];
end;

(* painter for DIAMOND *)
local
  val p01 = P.V.make (0.50, 0.00)
  and p02 = P.V.make (1.00, 0.50)
  and p03 = P.V.make (0.50, 1.00)
  and p04 = P.V.make (0.00, 0.50)
in
val diamondPainter
  = P.segmentsPainter [
    P.S.make (p01,p02),
    P.S.make (p02,p03),
    P.S.make (p03,p04),
    P.S.make (p04,p01)];
end;

(* painter for WAVE *)
local
  val p01 = P.V.make (0.00, 0.60)
  and p02 = P.V.make (0.25, 0.45)
  and p03 = P.V.make (0.30, 0.55)
  and p04 = P.V.make (0.40, 0.55)
  and p05 = P.V.make (0.30, 0.75)
  and p06 = P.V.make (0.40, 1.00)
  and p07 = P.V.make (0.60, 1.00)
  and p08 = P.V.make (0.70, 0.75)
  and p09 = P.V.make (0.60, 0.55)
  and p10 = P.V.make (0.80, 0.55)
  and p11 = P.V.make (1.00, 0.50)
  and p12 = P.V.make (1.00, 0.40)
  and p13 = P.V.make (0.70, 0.40)
  and p14 = P.V.make (0.60, 0.30)
  and p15 = P.V.make (0.85, 0.00)
  and p16 = P.V.make (0.60, 0.00)
  and p17 = P.V.make (0.50, 0.20)
  and p18 = P.V.make (0.40, 0.00)
  and p19 = P.V.make (0.25, 0.00)
  and p20 = P.V.make (0.40, 0.30)
  and p21 = P.V.make (0.35, 0.40)
  and p22 = P.V.make (0.25, 0.25)
  and p23 = P.V.make (0.00, 0.50)
in
val wave
  = P.segmentsPainter [
    P.S.make (p01,p02),
    P.S.make (p02,p03),
    P.S.make (p03,p04),
    P.S.make (p04,p05),
    P.S.make (p05,p06)(*,
    P.S.make (p06,p07)*),
    P.S.make (p07,p08),
    P.S.make (p08,p09),
    P.S.make (p09,p10),
    P.S.make (p10,p11)(*,
    P.S.make (p11,p12)*),
    P.S.make (p12,p13),
    P.S.make (p13,p14),
    P.S.make (p14,p15)(*,
    P.S.make (p15,p16)*),
    P.S.make (p16,p17),
    P.S.make (p17,p18)(*,
    P.S.make (p18,p19)*),
    P.S.make (p19,p20),
    P.S.make (p20,p21),
    P.S.make (p21,p22),
    P.S.make (p22,p23)(*,
    P.S.make (p23,p01)*)];

val wave2 = P.beside (wave, P.flipVert wave);
val wave4 = P.below (wave2, wave2);
val wave4' = P.flippedPairs wave;
val rsWave4 = P.rightSplit (wave, 4)
val csWave4 = P.cornerSplit (wave, 4)
val slWave3 = P.squareLimit (wave, 3)
end;

(* painter for Sierpinski gasket *)
local
  val p01 = P.V.make (0.00, 0.00)
  and p02 = P.V.make (1.00, 0.00)
  and p03 = P.V.make (0.50, Math.sqrt(3.0) / 2.0)
  val basePainter
    = P.segmentsPainter [
      P.S.make (p01,p02),
      P.S.make (p02,p03),
      P.S.make (p03,p01)];
in
val gasket1 = P.gasket (basePainter, 1);
val gasket2 = P.gasket (basePainter, 2);
val gasket4 = P.gasket (basePainter, 4);
val gasket6 = P.gasket (basePainter, 6);
val gasket8 = P.gasket (basePainter, 8);
end;

(* painter for Koch curve *)
local
  val p01 = P.V.make (0.00, 0.00)
  and p02 = P.V.make (1.00, 0.00)
  val basePainter
    = P.segmentsPainter [P.S.make (p01,p02)];
in
val koch1 = P.koch (basePainter, 1);
val koch2 = P.koch (basePainter, 2);
val koch3 = P.koch (basePainter, 3);
val koch4 = P.koch (basePainter, 4);
val koch5 = P.koch (basePainter, 5);
end;

(*
 * P.go framePainter;
 * P.go xPainter;
 * P.go diamondPainter;
 * P.go wave;
 * P.go wave2;
 * P.go wave4;
 * P.go rsWave4;
 * P.go csWave4;
 * P.go slWave3;
 * P.go gasket6;
 * P.go koch4;
 *)

