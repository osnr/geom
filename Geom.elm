import Window
import Touch as T

type Finger = T.Touch
type Pencil = T.Touch

fingers : Signal [Finger]
fingers = T.touches

pencils : Signal [Pencil]
pencils = T.touches

type DrawState = { lines : [Line]
                 , arcs : [Arc]
                 , toolCenter : (Float, Float)
                 , toolAngle : Float
                 , toolLength : Float
                 , drawing : Drawing }

type Line = ((Float, Float), (Float, Float))
type Arc = ((Float, Float), Float)

data Drawing = NotDrawing | DrawingLine Line | DrawingArc Arc

initialDrawState : DrawState
initialDrawState = { lines = []
                   , arcs = []
                   , toolCenter = (0, 0)
                   , toolAngle = 0
                   , toolLength = 10
                   , drawing = NotDrawing }

draw : ([T.Touch], [T.Touch]) -> DrawState -> DrawState
draw (fingers, pencils) ds = ds

toolTop : Float -> Form
toolTop len = rect (len*5) 20 |> outlined defaultLine

toolBottom : Float -> Form
toolBottom len = rect (len*5) 20 |> outlined defaultLine

displayTool : (Float, Float) -> Float -> Float -> Element
displayTool c angle len = group [ toolTop len |> moveY 11
                                , toolBottom len |> moveY -10 ]
                        |> move c
                        |> rotate angle
                        |> (\x -> [x])
                        |> collage 800 600

display : DrawState -> Element
display { lines, arcs, toolCenter, toolAngle, toolLength, drawing } =
        layers [ collage 800 600 <| map (traced defaultLine . uncurry segment) lines
               , displayTool toolCenter toolAngle toolLength  ]

displayS : Signal Element
displayS = display <~ (foldp draw initialDrawState <| lift2 (,) fingers pencils)

recenter : Int -> Int -> T.Touch -> T.Touch
recenter ww wh t = { t | x <- t.x - (ww `div` 2),
                         y <- -t.y + (wh `div` 2) }

distinguish : [T.Touch] -> ([Finger], [Pencil])
distinguish ts = let examineTouch t (fs, ps) =
                       let distances = map (\t2 -> ((t2.x - t.x)^2 + (t2.y - t.y)^2, t2)) ts
                           nearer (d1, t1) (d2, t2) = if d1 < d2
                                                        then (d1, t1)
                                                        else (d2, t2)
                           (nearestDist, nearestTouch) = foldr1 nearer distances
                       in if nearestDist < 40
                            then (nearestTouch :: fs, ps)
                            else (fs, t :: ps)
                 in foldr examineTouch ([], []) ts

touchesR : Signal [T.Touch]
touchesR = let recenterAll ww wh ts = map (recenter ww wh) ts
           in recenterAll <~ Window.width ~ Window.height ~ T.touches

touchSymbol : T.Touch -> Form
touchSymbol {x, y} = circle 10
                   |> outlined defaultLine
                   |> move (toFloat x, toFloat y)

touchesView : Signal Element
touchesView = let viewTouches ww wh ts = collage ww wh <| map touchSymbol ts
              in viewTouches <~ Window.width 
                              ~ Window.height
                              ~ touchesR

main = (\x y -> layers [x, y]) <~ touchesView ~ displayS

-- TODO put dots for fingers and/or pencils
-- start working on rotation, drawing, etc
-- figure out how to normalize coordinates / get window dims into collage
