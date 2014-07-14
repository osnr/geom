import Window
import Text
import Touch as T

type Finger = T.Touch
type Pencil = T.Touch

type DrawState = { lines : [Line]
                 , arcs : [Arc]
                 , toolCenter : Point
                 , toolAngle : Angle
                 , toolLength : Length
                 , toolMoving : ToolMoving
                 , drawing : Drawing }

type Point = (Float, Float)
type Angle = Float
type Length = Float

type Line = (Point, Point)
type Arc = (Point, Length, Angle)

data Drawing = NotDrawing | DrawingLine Line | DrawingArc Arc

data ToolMoving = NotMoving
                | Translating { initialOffset : Point }
                | Stretching { initialLength : Length }

-- updating state
initialDrawState : DrawState
initialDrawState = { lines = []
                   , arcs = []
                   , toolCenter = (0, 0)
                   , toolAngle = 0
                   , toolLength = 10
                   , toolMoving = NotMoving
                   , drawing = NotDrawing }

toPoint : (Int, Int) -> Point
toPoint (x, y) = (toFloat x, toFloat y)

addv : Point -> Point -> Point
addv (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subv : Point -> Point -> Point
subv (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

dist : Point -> Point -> Float
dist (x1, y1) (x2, y2) = sqrt <| (x1 - x2)^2 + (y1 - y2)^2

moveTool : [Finger] -> DrawState -> DrawState
moveTool fs ds =
  case (fs, ds.toolMoving) of
    ([], _) -> { ds | toolMoving <- NotMoving }

    (f::[], NotMoving) ->
      { ds | toolMoving <- Translating { initialOffset = ds.toolCenter `subv` toPoint (f.x, f.y) } }
    (f::[], Translating {initialOffset}) ->
      { ds | toolCenter <- initialOffset `addv` toPoint (f.x, f.y) }
    (f::[], Stretching _) -> { ds | toolMoving <- NotMoving }

    (f1::f2::[], NotMoving) ->
      { ds | toolMoving <- Stretching { initialLength = ds.toolLength - (toPoint (f1.x, f1.y) `dist` toPoint (f2.x, f2.y)) } }
    (f1::f2::[], Stretching {initialLength}) ->
      { ds | toolLength <- initialLength + (toPoint (f1.x, f1.y) `dist` toPoint (f2.x, f2.y)) }

draw : [Pencil] -> DrawState -> DrawState
draw ps ds =
  case (ps, ds.drawing) of
    ([], NotDrawing) -> ds
    ([], DrawingLine l) -> { ds | drawing <- NotDrawing,
                                  lines <- l :: ds.lines }
    ([], DrawingArc a) -> { ds | drawing <- NotDrawing,
                                 arcs <- a :: ds.arcs }
    (p::_, NotDrawing) -> { ds | drawing <- DrawingLine (toPoint (p.x, p.y), toPoint (p.x, p.y)) }
    (p::_, DrawingLine (pt1, _)) -> { ds | drawing <- DrawingLine (pt1, (toFloat p.x, toFloat p.y)) }

update : ([Finger], [Pencil]) -> DrawState -> DrawState
update (fs, ps) = draw ps . moveTool fs

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

drawingLines : Drawing -> [Line]
drawingLines drawing = case drawing of
                         NotDrawing -> []
                         DrawingLine l -> [l]
                         DrawingArc _ -> []

display : Int -> Int -> DrawState -> Element
display ww wh { lines, arcs, toolCenter, toolAngle, toolLength, drawing } =
        let linesForms = map (traced defaultLine . uncurry segment) <| drawingLines drawing ++ lines
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
displayS = (\ww wh ds -> layers [displayProblem ww wh, display ww wh ds])
           <~ Window.width ~ Window.height ~ (foldp update initialDrawState <| fingersPencils)

-- touch analysis
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
touchesView = let viewTouches ww wh (fs, ps) = collage ww wh
                                               <| map fingerSymbol fs ++ map pencilSymbol ps
              in viewTouches <~ Window.width 
                              ~ Window.height
                              ~ fingersPencils

main = (\x y z -> layers [x, y, z]) <~ touchesView ~ displayS ~ lift asText fingersPencils

-- TODO put dots for fingers and/or pencils
-- start working on rotation, drawing, etc
-- figure out how to normalize coordinates / get window dims into collage
