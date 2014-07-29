import Dict
import Text
import Keyboard
import Window
import Touch as T

type NormalizedTouch = { x:Float, y:Float, id:Int, x0:Float, y0:Float, t0:Time }
type Finger = NormalizedTouch
type Pencil = NormalizedTouch

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

             | OneFingerTranslate Point Finger

             | TwoFingers Finger Finger

             | AdjustLength Length Finger Finger
             | Rotate Angle Finger Finger

             | OneFingerOnePencil Finger Pencil
             | DrawLine Finger Pencil Line
             | DrawArc Finger Pencil Arc

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

displacement : NormalizedTouch -> Point
displacement t = (t.x - t.x0, t.y - t.y0)

angularDist : NormalizedTouch -> Angle
angularDist t = atan2 (t.y - t.y0) (t.x - t.x0)

-- f1 is the fixed finger (the one that went down first)
-- f2 is the finger that's moving to do rotation or length adjustment
startTwoFingerGesture : Finger -> Finger -> DrawState -> Gesture
startTwoFingerGesture f1 f2 ds =
  -- do some tan stuff to find the angle of the movement from f2's p0 to f2's p
  -- then return AdjustLength or Rotate
  let angle = angularDist f2
  in if (angle < degrees 45 && angle > degrees -45) || (angle > degrees 135 || angle < degrees -135)
       then AdjustLength ds.toolLength f1 f2 -- pulling left or right
       else Rotate ds.toolAngle f1 f2

gesture : ([Finger], [Pencil]) -> DrawState -> Gesture
gesture (fs, ps) ds =
  case (ds.gesture, fs, ps) of
    (NoTouches, f::[], []) -> OneFingerTranslate ds.toolCenter f

    (OneFingerTranslate p0 f, f'::[], []) ->
      if f'.id == f.id
        then OneFingerTranslate p0 f'
        else NoTouches

    (OneFingerTranslate _ f1, f2'::f1'::[], []) ->
      if f1'.id == f1.id
        then TwoFingers f1' f2'
        else NoTouches

    (TwoFingers f1 f2, f2'::f1'::[], []) ->
      -- dispatch only if the distance f2 has moved is big enough
      if f1'.id == f1.id && f2'.id == f2.id
        then if norm (displacement f2') > 100 
               then startTwoFingerGesture f1' f2' ds
               else TwoFingers f1' f2'
        else NoTouches

    (AdjustLength l0 f1 f2, f2'::f1'::[], []) ->
      if f1'.id == f1.id && f2'.id == f2.id
        then AdjustLength l0 f1' f2'
        else NoTouches

    (Rotate a0 f1 f2, f2'::f1'::[], []) ->
      if f1'.id == f1.id && f2'.id == f2.id
        then Rotate a0 f1' f2'
        else NoTouches

    (_, _, _) -> NoTouches

update : ([Finger], [Pencil]) -> DrawState -> DrawState
update fps ds =
  let gest = gesture fps ds
      ds' = { ds | gesture <- gest }
  in case gest of
       OneFingerTranslate p0 f -> { ds' | toolCenter <- p0 `addv` displacement f }
       AdjustLength l0 f1 f2 -> { ds' | toolLength <- l0 + norm (displacement f2) }
       Rotate a0 f1 f2 ->
         let (tx, ty) = ds'.toolCenter
         in { ds' | toolAngle <- atan2 (f2.y - ty) (f2.x - tx) }
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
  in Dict.fromList <| map (\n -> (n, mark n)) [0..30]

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
displayS : Signal Element
displayS = (\ds ww wh -> layers [asText ds, displayProblem ww wh, display ww wh ds])
           <~ (foldp update initialDrawState <| fingersPencils)
            ~ Window.width
            ~ Window.height

-- touch analysis
recenter : Int -> Int -> T.Touch -> NormalizedTouch
recenter cx cy t = { x = toFloat <| t.x - cx,
                     y = toFloat <| -t.y + cy,
                     x0 = toFloat <| t.x0 - cx,
                     y0 = toFloat <| -t.y0 + cy,
                     id = t.id,
                     t0 = t.t0 }

distinguish : [NormalizedTouch] -> Bool -> ([Finger], [Pencil])
distinguish ts forceFinger =
  let examineTouch t (fs, ps, used) =
        let ts' = filter (\t2 -> t /= t2) ts
            findDistance t1 t2 = sqrt <| (t2.x - t1.x)^2 + (t2.y - t1.y)^2
            byDistance ts = sortBy (findDistance t) ts

        in if | isEmpty ts' -> (fs, t :: ps, used)
              | any (\uid -> t.id == uid) used -> (fs, ps, used)
              | otherwise ->
                let nearestTouch = head <| byDistance ts'
                in if findDistance nearestTouch t < 230
                     then ({ t | x <- (nearestTouch.x + t.x) / 2,
                                 y <- (nearestTouch.y + t.y) / 2,
                                 x0 <- (nearestTouch.x0 + t.x0) / 2,
                                 y0 <- (nearestTouch.y0 + t.y0) / 2 } :: fs,
                           ps,
                           t.id :: nearestTouch.id :: used)
                     else (fs, t :: ps, used)
      (fs, ps, _) =
        if forceFinger
           then (ts, [], [])
           else foldr examineTouch ([], [], []) ts
  in (fs, ps)

touchesR : Signal [NormalizedTouch]
touchesR = let recenterAndResortAll ww wh ts =
                 let (cx, cy) = (ww `div` 2, wh `div` 2)
                 in map (recenter cx cy) <| sortBy ((\x -> -x) . .t0) ts
           in recenterAndResortAll <~ Window.width ~ Window.height ~ T.touches

fingersPencils : Signal ([Finger], [Pencil])
fingersPencils = distinguish <~ touchesR ~ Keyboard.shift

fingerSymbol : Finger -> Form
fingerSymbol {x, y} = circle 50
                    |> filled green
                    |> move (x, y)

pencilSymbol : Finger -> Form
pencilSymbol {x, y} = rect 50 50
                    |> filled black
                    |> move (x, y)

touchesView : Signal Element
touchesView = let viewTouches ww wh (fs, ps) = collage ww wh
                                               <| map fingerSymbol fs ++ map pencilSymbol ps
              in viewTouches <~ Window.width
                              ~ Window.height
                              ~ fingersPencils

main = (\x y z -> layers [x, y, plainText "\n\n" `above` z]) <~ touchesView ~ displayS ~ lift asText fingersPencils

-- TODO put dots for fingers and/or pencils
-- start working on rotation, drawing, etc
-- figure out how to normalize coordinates / get window dims into collage
