import Window
import Text
import Keyboard
import Touch as T

type Finger = T.Touch
type Pencil = T.Touch

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

             | AdjustLength Finger Finger
             | Rotate Finger Finger

             | OneFingerOnePencil Finger Pencil
             | DrawLine Finger Pencil Line
             | DrawArc Finger Pencil Arc

-- updating state
initialDrawState : DrawState
initialDrawState = { lines = []
                   , arcs = []
                   , toolCenter = (0, 0)
                   , toolAngle = 0
                   , toolLength = 10
                   , gesture = NoTouches }

toPoint : (Int, Int) -> Point
toPoint (x, y) = (toFloat x, toFloat y)

norm : Point -> Float
norm (x, y) = x^2 + y^2

addv (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

displacement : T.Touch -> Point
displacement t = toPoint (t.x - t.x0, t.y - t.y0)

-- f1 is the fixed finger (the one that went down first)
-- f2 is the finger that's moving to do rotation or length adjustment
startTwoFingerGesture : Finger -> Finger -> Gesture
startTwoFingerGesture f1 f2 =
  -- do some tan stuff to find the angle of the movement from f2's p0 to f2's p
  -- then return AdjustLength or Rotate
  let angle = atan2 (toFloat (f2.y - f2.y0)) (toFloat (f2.x - f2.x0))
  in if (angle < degrees 45 && angle > degrees -45) || (angle > degrees 135 || angle < degrees -135)
       then AdjustLength f1 f2 -- pulling left or right
       else Rotate f1 f2

stopGesture : DrawState -> DrawState
stopGesture ds = { ds | gesture <- NoTouches }

gesture : ([Finger], [Pencil]) -> DrawState -> DrawState
gesture (fs, ps) ds =
  case (ds.gesture, fs, ps) of
    (NoTouches, f::[], []) -> { ds | gesture <- OneFingerTranslate ds.toolCenter f }

    (OneFingerTranslate p0 f, f'::[], []) ->
      if f'.id == f.id
        then { ds | gesture <- OneFingerTranslate p0 f' }
        else stopGesture ds

    (OneFingerTranslate _ f1, f2'::f1'::[], []) ->
      if f1'.id == f1.id
        then { ds | gesture <- TwoFingers f1' f2' }
        else stopGesture ds

    (TwoFingers f1 f2, f2'::f1'::[], []) ->
      -- dispatch only if the distance f2 has moved is big enough
      if f1'.id == f1.id && f2'.id == f2.id && norm (displacement f2') > 10
        then { ds | gesture <- startTwoFingerGesture f1' f2' }
        else stopGesture ds

    (AdjustLength f1 f2, f2'::f1'::[], []) ->
      if f1'.id == f1.id && f2'.id == f2.id
        then { ds | gesture <- AdjustLength f1' f2' }
        else stopGesture ds

    (Rotate f1 f2, f2'::f1'::[], []) ->
      if f1'.id == f1.id && f2'.id == f2.id
        then { ds | gesture <- Rotate f1' f2' }
        else stopGesture ds

    (_, _, _) -> stopGesture ds

update : ([Finger], [Pencil]) -> DrawState -> DrawState
update fps ds =
  let ds' = gesture fps ds
  in case ds'.gesture of
       OneFingerTranslate p0 f -> { ds' | toolCenter <- p0 `addv` displacement f }
       otherwise -> ds'

-- display
unitWidth = 20
markHeight = 20

toolTop : Length -> Form
toolTop len = rect (len*unitWidth) 20
              |> filled (rgba 0 128 128 0.5)
              |> moveX (len*unitWidth/2)

toolMarks : Length -> Form
toolMarks len =
    let mark val = group [ segment (0, 0) (0, markHeight)
                           |> traced (solid (rgb 0 64 128))
                         , toText (show val)
                           |> Text.height 10
                           |> Text.color (rgb 0 64 128)
                           |> Text.centered |> toForm
                           |> moveY -6 ]
                 |> moveX (val*unitWidth)
    in group <| map (mark . toFloat) [0..floor len]

toolBottom : Length -> Form
toolBottom len = rect (len*unitWidth) 20
               |> filled (rgba 0 128 128 0.5)
               |> moveX (len*unitWidth/2)
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
displayS = (\ww wh ds -> layers [asText ds, displayProblem ww wh, display ww wh ds])
           <~ Window.width ~ Window.height ~ (foldp update initialDrawState <| fingersPencils)

-- touch analysis
recenter : Int -> Int -> T.Touch -> T.Touch
recenter cx cy t = { t | x <- t.x - cx,
                         y <- -t.y + cy,
                         x0 <- t.x0 - cx,
                         y0 <- t.y0 - cy }

distinguish : [T.Touch] -> Bool -> ([Finger], [Pencil])
distinguish ts forceFinger =
  let examineTouch t (fs, ps, used) =
        let findDistance t1 t2 = (sqrt <| toFloat (t2.x - t.x)^2 + toFloat (t2.y - t.y)^2, t2)
            distances = map (findDistance t) <| filter (\t2 -> t /= t2) ts
            nearer (d1, t1) (d2, t2) = if d1 < d2
                                         then (d1, t1)
                                         else (d2, t2)
        in if | isEmpty distances -> (fs, t :: ps, used)
              | any (\t2 -> t == t2) used -> (fs, ps, used)
              | otherwise ->
                let (nearestDist, nearestTouch) = foldr1 nearer distances
                in if nearestDist < 120
                     then ({ t | x <- (nearestTouch.x + t.x) `div` 2,
                                 y <- (nearestTouch.y + t.y) `div` 2  } :: fs,
                           ps,
                           t :: nearestTouch :: used)
                     else (fs, t :: ps, used)
      (fs, ps, _) =
        if forceFinger
           then (ts, [], [])
           else foldr examineTouch ([], [], []) ts
  in (fs, ps)

touchesR : Signal [T.Touch]
touchesR = let recenterAndResortAll ww wh ts =
                 let (cx, cy) = (ww `div` 2, wh `div` 2)
                 in map (recenter cx cy) <| sortBy ((\x -> -x) . .t0) ts
           in recenterAndResortAll <~ Window.width ~ Window.height ~ T.touches

fingersPencils : Signal ([Finger], [Pencil])
fingersPencils = distinguish <~ touchesR ~ Keyboard.shift

fingerSymbol : Finger -> Form
fingerSymbol {x, y} = circle 50
                    |> filled green
                    |> move (toFloat x, toFloat y)

pencilSymbol : Finger -> Form
pencilSymbol {x, y} = rect 50 50
                    |> filled black
                    |> move (toFloat x, toFloat y)

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
