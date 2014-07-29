import Dict
import Text
import Keyboard
import Debug
import Touch as T

type NTouch = { x:Float, y:Float, id:Int, x0:Float, y0:Float, t0:Time }

type DrawState = { lines : [Line]
                 , arcs : [Arc]
                 , toolCenter : Point
                 , toolAngle : Angle
                 , toolLength : Length
                 , gesture : Gesture }

type Point = (Float, Float)
type Angle = Float
type Length = Float

type Line = (Point, Point)
type Arc = (Point, Length, Angle)

data Gesture = NoTouches

             | Translate Point NTouch

             | TwoFingers NTouch NTouch

             | AdjustLength Length NTouch NTouch
             | Rotate Angle NTouch NTouch

             | OneFinger NTouch

             | OneNTouchOneNTouch Point NTouch NTouch
             | DrawLine NTouch NTouch Line
             | DrawArc NTouch NTouch Arc

-- updating state
initialDrawState : DrawState
initialDrawState = { lines = []
                   , arcs = []
                   , toolCenter = (0, 0)
                   , toolAngle = 0
                   , toolLength = 300
                   , gesture = NoTouches }

norm : Point -> Float
norm (x, y) = sqrt <| x^2 + y^2

addv (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

displacement : NTouch -> Point
displacement t = (t.x - t.x0, t.y - t.y0)

angularDist : NTouch -> Angle
angularDist t = atan2 (t.y - t.y0) (t.x - t.x0)

-- f1 is the fixed finger (the one that went down first)
-- f2 is the finger that's moving to do rotation or length adjustment
startTwoFingerGesture : NTouch -> NTouch -> DrawState -> Gesture
startTwoFingerGesture f1 f2 ds =
  -- do some tan stuff to find the angle of the movement from f2's p0 to f2's p
  -- then return AdjustLength or Rotate
  let angle = angularDist f2 - ds.toolAngle
  in if (angle < degrees 45 && angle > degrees -45) || (angle > degrees 135 || angle < degrees -135)
       then AdjustLength ds.toolLength f1 f2 -- pulling left or right
       else Rotate ds.toolAngle f1 f2

gesture : [NTouch] -> DrawState -> Gesture
gesture ts ds = Debug.log "gesture" <|
  case (ds.gesture, ts) of
    (NoTouches, t::[]) -> Translate ds.toolCenter t

    (Translate p0 t, t'::[]) ->
      if t'.id == t.id
        then Translate p0 t'
        else NoTouches

    (Translate _ t1, t2'::t1'::[]) ->
      if t1'.id == t1.id
        then TwoFingers t1' t2'
        else NoTouches
    (OneFinger t1, t2'::t1'::[]) ->
      if t1'.id == t1.id
        then TwoFingers t1' t2'
        else NoTouches

    (TwoFingers t1 t2, t2'::t1'::[]) ->
      -- dispatch only if the distance t2 has moved is big enough
      if t1'.id == t1.id && t2'.id == t2.id
        then if norm (displacement t2') > 100 
               then startTwoFingerGesture t1' t2' ds
               else TwoFingers t1' t2'
        else NoTouches

    (AdjustLength l0 t1 t2, t2'::t1'::[]) ->
      if t1'.id == t1.id && t2'.id == t2.id
        then AdjustLength l0 t1' t2'
        else NoTouches

    (Rotate a0 t1 t2, t2'::t1'::[]) ->
      if t1'.id == t1.id && t2'.id == t2.id
        then Rotate a0 t1' t2'
        else NoTouches

    (AdjustLength _ t1 t2, t'::[]) ->
      if t'.id == t1.id || t'.id == t2.id 
        then OneFinger t'
        else NoTouches
    (Rotate _ t1 t2, t'::[]) -> 
      if t'.id == t1.id || t'.id == t2.id 
        then OneFinger t'
        else NoTouches
    (OneFinger t, t'::[]) ->
      if t'.id == t.id
        then OneFinger t'
        else NoTouches

    (_, _) -> NoTouches

update : [NTouch] -> DrawState -> DrawState
update ts ds =
  let gest = gesture ts ds
      ds' = { ds | gesture <- gest }
  in case gest of
       Translate p0 t -> { ds' | toolCenter <- p0 `addv` displacement t }
       AdjustLength l0 _ t2 -> { ds' | toolLength <- l0 + (t2.x - t2.x0) * cos ds'.toolAngle + (t2.y - t2.y0) * sin ds'.toolAngle }
       Rotate a0 t1 t2 ->
         let (tx, ty) = ds'.toolCenter
         in { ds' | toolAngle <- atan2 (t2.y - ty) (t2.x - tx) }
       otherwise -> ds'

-- display
unitWidth = 20
markHeight = 20

toolTop : Length -> Form
toolTop len = rect len 20
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
toolBottom len = rect len 20
               |> filled (rgba 0 128 128 0.5)
               |> moveX (len/2)
               |> \back -> group [back, toolMarks len]

displayTool : Point -> Angle -> Length -> Form
displayTool c angle len = group [ toolTop len |> moveY 11
                                , toolBottom len |> moveY -10 ]
                        |> move c
                        |> rotate angle

display : Int -> Int -> DrawState -> Element
display ww wh { lines, arcs, toolCenter, toolAngle, toolLength } =
        let linesForms = map (traced defaultLine . uncurry segment) <| lines
        in collage ww wh <| linesForms ++ [displayTool toolCenter toolAngle toolLength]

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

-- overall display
displayWidth = 1024
displayHeight = 768

displayS : Signal Element
displayS = (\ds -> layers [displayProblem displayWidth displayHeight, display displayWidth displayHeight ds])
           <~ (foldp update initialDrawState <| touchesR)

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

main = (\x y -> layers [spacer displayWidth displayHeight |> color gray, x, y]) <~ touchesView ~ displayS
