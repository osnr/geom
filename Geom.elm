import Window
import Touch as T

fingers : Signal [T.Touch]
fingers = T.touches

pencils : Signal [T.Touch]
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

main = display <~ (foldp draw initialDrawState <| lift2 (,) fingers pencils)
