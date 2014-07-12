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
draw (fs, ps) ds =
  case (ps, ds.drawing) of
    ([], NotDrawing) -> ds
    ([], DrawingLine l) -> { ds | drawing <- NotDrawing,
                                  lines <- l :: ds.lines }
    ([], DrawingArc a) -> { ds | drawing <- NotDrawing,
                                 arcs <- a :: ds.arcs }
    (p::_, NotDrawing) -> { ds | drawing <- DrawingLine ((toFloat p.x, toFloat p.y), (toFloat p.x, toFloat p.y)) }
    (p::_, DrawingLine (pt1, _)) -> { ds | drawing <- DrawingLine (pt1, (toFloat p.x, toFloat p.y)) }

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

drawingLines : Drawing -> [Line]
drawingLines drawing = case drawing of
                         NotDrawing -> []
                         DrawingLine l -> [l]
                         DrawingArc _ -> []

display : Int -> Int -> DrawState -> Element
display ww wh { lines, arcs, toolCenter, toolAngle, toolLength, drawing } =
        layers [ collage ww wh <| map (traced defaultLine . uncurry segment) <| drawingLines drawing ++ lines
               , displayTool toolCenter toolAngle toolLength  ]

displayS : Signal Element
displayS = display <~ Window.width ~ Window.height ~ (foldp draw initialDrawState <| fingersPencils)

recenter : Int -> Int -> T.Touch -> T.Touch
recenter ww wh t = { t | x <- t.x - (ww `div` 2),
                         y <- -t.y + (wh `div` 2) }

distinguish : [T.Touch] -> ([Finger], [Pencil])
distinguish ts =
  let examineTouch t (fs, ps, used) =
        let findDistance t1 t2 = (sqrt <| (t2.x - t.x)^2 + (t2.y - t.y)^2, t2)
            distances = map (findDistance t) <| filter (\t2 -> t /= t2) ts
            nearer (d1, t1) (d2, t2) = if d1 < d2
                                         then (d1, t1)
                                         else (d2, t2)
        in if | isEmpty distances -> (fs, t :: ps, used)
              | any (\t2 -> t == t2) used -> (fs, ps, used)
              | otherwise ->
                let (nearestDist, nearestTouch) = foldr1 nearer distances
                in if nearestDist < 80
                     then ({ t | x <- (nearestTouch.x + t.x) `div` 2,
                                 y <- (nearestTouch.y + t.y) `div` 2  } :: fs,
                           ps,
                           t :: nearestTouch :: used)
                     else (fs, t :: ps, used)
      (fs, ps, _) = foldr examineTouch ([], [], []) ts
  in (fs, ps)

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
touchesView = let viewTouches ww wh (fs, ps) = color blue
                                               <| collage ww wh
                                               <| map fingerSymbol fs ++ map pencilSymbol ps
              in viewTouches <~ Window.width 
                              ~ Window.height
                              ~ fingersPencils

main = (\x y z -> layers [x, y, z]) <~ touchesView ~ displayS ~ lift asText fingersPencils

-- TODO put dots for fingers and/or pencils
-- start working on rotation, drawing, etc
-- figure out how to normalize coordinates / get window dims into collage
