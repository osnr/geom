import Window
import Touch as T

type Finger = T.Touch
type Pencil = T.Touch

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

draw : ([Finger], [Pencil]) -> DrawState -> DrawState
draw (fs, ps) ds = ds

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
        layers [ collage 400 300 <| map (traced defaultLine . uncurry segment) lines
               , displayTool toolCenter toolAngle toolLength  ]

displayS : Signal Element
displayS = display <~ (foldp draw initialDrawState <| fingersPencils)

recenter : Int -> Int -> T.Touch -> T.Touch
recenter ww wh t = { t | x <- t.x - (ww `div` 2),
                         y <- -t.y + (wh `div` 2) }

distinguish : [T.Touch] -> ([Finger], [Pencil])
distinguish ts = let examineTouch t (fs, ps) =
                       let findDistance t1 t2 = (sqrt <| (t2.x - t.x)^2 + (t2.y - t.y)^2, t2)
                           distances = map (findDistance t) <| filter (\t2 -> t /= t2) ts
                           nearer (d1, t1) (d2, t2) = if d1 < d2
                                                        then (d1, t1)
                                                        else (d2, t2)
                       in if isEmpty distances
                            then (fs, t :: ps)
                            else let (nearestDist, nearestTouch) = foldr1 nearer distances
                                 in if nearestDist < 100
                                      then (nearestTouch :: fs, ps)
                                      else (fs, t :: ps)
                 in foldr examineTouch ([], []) ts

touchesR : Signal [T.Touch]
touchesR = let recenterAll ww wh ts = map (recenter ww wh) ts
           in recenterAll <~ Window.width ~ Window.height ~ T.touches

fingersPencils : Signal ([Finger], [Pencil])
fingersPencils = distinguish <~ touchesR

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

main = (\x y z -> layers [x, y, z]) <~ touchesView ~ displayS ~ lift asText fingersPencils

-- TODO put dots for fingers and/or pencils
-- start working on rotation, drawing, etc
-- figure out how to normalize coordinates / get window dims into collage
