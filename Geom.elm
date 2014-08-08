import Dict
import Text
import Keyboard
import Debug
import Touch as T

import Graphics.Input as I

type NTouch = { x:Float, y:Float, id:Int, x0:Float, y0:Float, t0:Time }

type DrawState = { lines : [Line]
                 , arcs : [Arc]

                 , toolStart : Point
                 , toolAngle : Angle
                 , toolLength : Length

                 , mode : Mode

                 , gesture : Gesture
                 , drawing : Drawing }

type Point = (Float, Float)
type Angle = Float
type Length = Float

type Line = (Point, Point)
type Arc = (Point, Length, Angle, Angle)

data Drawing = NotDrawing | DrawingLine Line | DrawingArc Arc

data Gesture = NoTouches

             | Translate { p0:Point, tp0:Point, t:NTouch }

             | TouchEnd { t:NTouch } -- before we've figured out which one
             | AdjustLength { l0:Length, t:NTouch }
             | Rotate { a0:Angle, t:NTouch }

             | PencilEnd { t:NTouch } -- before we've figured out which one
             | DrawLine { t:NTouch, p1:Point }
             | DrawArc { t:NTouch, c:Point, r:Length }

-- updating state
initialDrawState : DrawState
initialDrawState = { lines = []
                   , arcs = []
                   , toolStart = (0, 0)
                   , toolAngle = 0
                   , toolLength = 300

                   , mode = Ruler -- kind of annoying that this is duplicated

                   , gesture = NoTouches
                   , drawing = NotDrawing }

norm : Point -> Float
norm (x, y) = sqrt <| x^2 + y^2

addv (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
subv (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

dist v w = norm <| subv v w

displacement : NTouch -> Point
displacement t = (t.x - t.x0, t.y - t.y0)

angularDist : NTouch -> Angle
angularDist t = atan2 (t.y - t.y0) (t.x - t.x0)

bound (lower, upper) val = max lower <| min upper val

boundPoint : ((Float, Float), (Float, Float)) -> Point -> Point
boundPoint ((minX, maxX), (minY, maxY)) (x, y) = (bound (minX, maxX) x, bound (minY, maxY) y)

toolEnd : Point -> Angle -> Length -> Point
toolEnd (sx, sy) a l = (sx + l*(cos a), sy + l*(sin a))

toToolAngle : Point -> Point -> Angle
toToolAngle (sx, sy) (ex, ey) = snd <| toPolar (ex - sx, ey - sx)

distToSegment p v w =
  let (px, py) = p
      (x1, y1) = v
      (x2, y2) = w

      l2 = (dist v w)^2

  in if l2 == 0
       then dist p v
       else let t = ((px - x1) * (x2 - x1) + (py - y1) * (y2 - y1)) / l2
            in if | t < 0 -> dist p v
                  | t > 1 -> dist p w
                  | otherwise -> dist p ( x1 + t * (x2 - x1)
                                        , y1 + t * (y2 - y1) )

insideRuler : DrawState -> NTouch -> Bool
insideRuler ds {x, y} =
  let start = ds.toolStart
      end = toolEnd start ds.toolAngle ds.toolLength

      distToRuler = Debug.log "distToRuler" <| distToSegment (x, y) start end
  in distToRuler < 50

-- used to determine whether we're lengthening ruler / drawing a line
-- or rotating ruler / drawing an arc.
-- check whether the user's gesture has been more in a parallel or perpendicular direction
isTouchParallel : Angle -> NTouch -> Bool
isTouchParallel toolAngle t =
  let theta = angularDist t - toolAngle
  in (theta < degrees 45 && theta > degrees -45) || (theta > degrees 135 || theta < degrees -135)

startEndGesture : NTouch -> DrawState -> Gesture
startEndGesture t ds =
  if isTouchParallel ds.toolAngle t
    then AdjustLength { l0 = ds.toolLength, t = t } -- pulling left or right
    else Rotate { a0 = ds.toolAngle, t = t }

-- t1 is the fixed finger (the one that went down first)
-- t2 is the finger that's moving to draw a line or an arc
startPencilEndGesture : NTouch -> DrawState -> Gesture
startPencilEndGesture t ds =
  if isTouchParallel ds.toolAngle t
    then DrawLine { t = t, p1 = ds.toolStart }
    else DrawArc { t = t, c = ds.toolStart, r = ds.toolLength }

gesture : [NTouch] -> DrawState -> Gesture
gesture ts ds = -- Debug.log "gesture state" <|
  case (ds.gesture, ts) of
    (NoTouches, t::[]) ->
      if | dist (t.x, t.y) ds.toolStart < 200 ->
             case ds.mode of
               Ruler -> Translate { p0 = ds.toolStart
                                  , tp0 = (t.x, t.y)
                                  , t = t }
               _     -> NoTouches

         | dist (t.x, t.y) (toolEnd ds.toolStart ds.toolAngle ds.toolLength) < 200 ->
             case ds.mode of
               Ruler -> TouchEnd { t = t }
               Draw  -> PencilEnd { t = t }

         | otherwise -> NoTouches

    (Translate trans, t'::[]) ->
      if t'.id == trans.t.id
        then Translate { trans | t <- t' }
        else NoTouches

    -- rotate/adj length stuff
    (TouchEnd ep, t'::[]) ->
      -- dispatch only if the distance t2 has moved is big enough
      if t'.id == ep.t.id
        then if norm (displacement t') > 50
               then startEndGesture t' ds
               else TouchEnd { ep | t <- t' }
        else NoTouches

    (AdjustLength adj, t'::[]) ->
      if t'.id == adj.t.id
        then AdjustLength { adj | t <- t' }
        else NoTouches

    (Rotate rot, t'::[]) ->
      if t'.id == rot.t.id
        then Rotate { rot | t <- t' }
        else NoTouches

    -- pencil stuff
    (PencilEnd de, t'::[]) ->
      if t'.id == de.t.id
        then if norm (displacement t') > 50
               then startPencilEndGesture t' ds
               else PencilEnd { de | t <- t' }
        else NoTouches

    (DrawLine dl, t'::[]) ->
      if t'.id == dl.t.id
        then DrawLine { dl | t <- t' }
        else NoTouches

    (DrawArc da, t'::[]) ->
      if t'.id == da.t.id
        then DrawArc { da | t <- t' }
        else NoTouches

    (_, _) -> NoTouches

update : Action -> DrawState -> DrawState
update a ds = case a of
                Touches ts -> touchesUpdate ts ds
                ChangeMode m -> { ds | mode <- m }

touchesUpdate : [NTouch] -> DrawState -> DrawState
touchesUpdate ts ds =
  let gest = gesture ts ds
      ds' = { ds | gesture <- gest }
  in case gest of
       Translate { p0, tp0, t } -> -- TODO optimize so we don't do all this bounding always
         let toolStart' = boundPoint ((-displayWidth/2, displayWidth/2), (-displayHeight/2, displayHeight/2))
                          <| p0 `addv` ((t.x, t.y) `subv` tp0)
         in { ds' | toolStart <- toolStart' }

       AdjustLength { l0, t } ->
         let toolLength' = l0 + (t.x - t.x0) * cos ds'.toolAngle +
                                (t.y - t.y0) * sin ds'.toolAngle
         in { ds' | toolLength <- bound (0, displayWidth) toolLength' }

       Rotate { a0, t } ->
         let (sx, sy) = ds'.toolStart
             (ex', ey') = boundPoint ((-displayWidth/2, displayWidth/2),
                                      (-displayHeight/2, displayHeight/2))
                                     (t.x, t.y)
         in { ds' | toolAngle <- atan2 (ey' - sy) (ex' - sx) }

       DrawLine { t, p1 } ->
         let theta = angularDist t - ds.toolAngle
             r = (norm (displacement t)) * cos theta
             lineLength = bound (0, ds.toolLength) r
         in { ds' | drawing <- DrawingLine (p1,
                                            (fst p1 + lineLength*(cos ds.toolAngle),
                                             snd p1 + lineLength*(sin ds.toolAngle))) }
       DrawArc { t, c, r } ->
         let (tx, ty) = ds'.toolStart
         in { ds' | drawing <- DrawingArc (c, r, ds'.toolAngle,
                                           atan2 (t.y - ty) (t.x - tx)) }

       otherwise ->
         case ds'.drawing of
           NotDrawing -> ds'
           DrawingLine l -> { ds' | drawing <- NotDrawing
                                  , lines <- l :: ds'.lines } -- TODO get rid of drawing earlier
           DrawingArc a -> { ds' | drawing <- NotDrawing
                                 , arcs <- a :: ds'.arcs } -- TODO get rid of drawing earlier

-- display
unitWidth = 20
markHeight = 20

rulerTopHeight = 20
rulerBottomHeight = 20

{-| An elliptical arc with the given center, radii and angle interval. -}
arc : (Float, Float) -> (Float, Float) -> (Float, Float) -> Shape
arc (cx, cy) (a, b) (startAngle, endAngle) =
  let n = 50
      t = (endAngle - startAngle) / n
      f i = (cx + a * cos (t*i + startAngle), cy + b * sin (t*i + startAngle))
  in map f [0..n-1]

toolTop : Length -> Form
toolTop len = rect len rulerTopHeight
              |> filled (rgba 0 128 128 0.5)
              |> moveX (len/2)

marks : Dict.Dict Int Form
marks =
  let mark n = group [ segment (0, 0) (0, markHeight)
                       |> traced (solid (rgb 0 64 128))
                     , toText (show n)
                       |> Text.height 10
                       |> Text.color (rgb 0 64 128)
                       |> Text.centered |> toForm
                       |> moveY -6 ]
  in Dict.fromList <| map (\n -> (n, mark n)) [0..50]

toolMarks : Length -> Form
toolMarks len =
  let mark n = Dict.getOrElse (group []) n marks |> moveX (toFloat (n*unitWidth))
  in group <| map mark [0..floor (len / unitWidth)]

toolBottom : Length -> Form
toolBottom len = rect len rulerBottomHeight
               |> filled (rgba 0 128 128 0.5)
               |> moveX (len/2)
               |> \back -> group [back, toolMarks len]

displayTool : Point -> Angle -> Length -> Form
displayTool c angle len = group [ toolTop len |> moveY 11
                                , toolBottom len |> moveY -10
                                , circle 4 |> filled black
                                , circle 4 |> filled black |> moveX len ]
                        |> move c
                        |> rotate angle

display : Int -> Int -> DrawState -> Element
display ww wh { lines, arcs, toolStart, toolAngle, toolLength, drawing } =
        let linesForms = map (traced defaultLine . uncurry segment)
                         <| case drawing of
                              NotDrawing -> lines
                              DrawingLine l -> l :: lines
                              DrawingArc _ -> lines
            arcForms = map (\(c, l, a1, a2) -> traced defaultLine <| arc c (l, l) (a1, a2))
                       <| case drawing of
                            NotDrawing -> arcs
                            DrawingLine _ -> arcs
                            DrawingArc a -> a :: arcs
        in collage ww wh <| linesForms ++ arcForms ++ [displayTool toolStart toolAngle toolLength]

-- problem display
problem : Form
problem = let triangle = group
                         <| map (outlined (solid darkGray))
                         <| [ segment (0, 0) (100, 0)
                            , segment (100, 0) (0, 115)
                            , segment (0, 115) (0, 0) ]
              labels = group
                       <| [ move (0, -10) <| toForm <| plainText "A"
                          , move (110, 0) <| toForm <| plainText "B"
                          , move (0, 125) <| toForm <| plainText "C" ]
          in moveY 100
             <| group [triangle, labels]

displayProblem : Int -> Int -> Element
displayProblem ww wh = collage ww wh [problem]

-- touch analysis
recenter : Int -> Int -> T.Touch -> NTouch
recenter cx cy t = { x = toFloat <| t.x - cx,
                     y = toFloat <| -t.y + cy,
                     x0 = toFloat <| t.x0 - cx,
                     y0 = toFloat <| -t.y0 + cy,
                     id = t.id,
                     t0 = t.t0 }

touchesR : Signal [NTouch]
touchesR = let recenterAndResortAll ts =
                 let (cx, cy) = (displayWidth `div` 2, displayHeight `div` 2)
                 in map (recenter cx cy) <| sortBy ((\x -> -x) . .t0) ts
           in recenterAndResortAll <~ T.touches

fingerSymbol : NTouch -> Form
fingerSymbol {x, y} = circle 50
                    |> filled green
                    |> move (x, y)

touchesView : Signal Element
touchesView = let viewTouches ts = collage displayWidth displayHeight
                                   <| map fingerSymbol ts
              in viewTouches <~ touchesR

-- modes
data Mode = Ruler | Draw

mode : I.Input Mode
mode = I.input Ruler

modesBox : Mode -> Element
modesBox m = flow down [ I.button mode.handle Ruler "Ruler"
                         |> case m of
                              Ruler -> color blue
                              _     -> id
                       , I.button mode.handle Draw "Draw"
                         |> case m of
                              Draw -> color blue
                              _    -> id ]

-- merging
data Action = Touches [NTouch] | ChangeMode Mode

actions = merge (lift Touches touchesR) (lift ChangeMode mode.signal)

-- overall display
displayWidth = 1371
displayHeight = 660

displayS : Signal Element
displayS = (\ds -> layers [displayProblem displayWidth displayHeight, display displayWidth displayHeight ds])
           <~ (foldp update initialDrawState <| actions)

main = (\x y z -> layers [spacer displayWidth displayHeight |> color gray, x, y, z]) <~ touchesView ~ displayS ~ lift modesBox mode.signal
