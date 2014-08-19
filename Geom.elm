import Dict
import Text
import Keyboard
import Debug
import Window
import Touch as T

type NTouch = { x:Float, y:Float, id:Int, x0:Float, y0:Float, t0:Time }

type DrawState = { lines : [Line]
                 , arcs : [Arc]
                 , points : [Point]

                 , toolStart : Point
                 , toolAngle : Angle
                 , toolLength : Length

                 , displayWidth : Float
                 , displayHeight : Float
                 , mode : Mode

                 , gesture : Gesture
                 , drawing : Drawing }

type Point = (Float, Float)
type Angle = Float
type Length = Float

type Line = (Point, Point)
type Arc = (Point, Length, Angle, Angle)

data Drawing = NotDrawing | DrawingLine Line | DrawingArc (Maybe Angle) Arc

data Gesture = NoTouches

             | Translate { toolP0:Point, touchP0:Point, t:NTouch }

             | TouchEnd { t:NTouch } -- before we've figured out which one
             | AdjustLength { l0:Length, t:NTouch }
             | Rotate { angleOffset0:Angle, t:NTouch }

             | PencilEnd { t:NTouch } -- before we've figured out which one
             | DrawLine { t:NTouch, p1:Point }
             | DrawArc { t:NTouch, c:Point, r:Length }

-- updating state
initialDrawState : DrawState
initialDrawState = { lines = []
                   , arcs = []
                   , points = []

                   , toolStart = (0, 0)
                   , toolAngle = 0
                   , toolLength = 300

                   , displayWidth = 800
                   , displayHeight = 600
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

      distToRuler = distToSegment (x, y) start end
  in distToRuler < 50

-- used to determine whether we're lengthening ruler / drawing a line
-- or rotating ruler / drawing an arc.
-- check whether the user's gesture has been more in a parallel or perpendicular direction
isTouchParallel : Angle -> NTouch -> Bool
isTouchParallel toolAngle t =
  let theta = angularDist t - toolAngle
  in abs (cos theta) > abs (sin theta)

initEndGesture : NTouch -> DrawState -> Gesture
initEndGesture t ds =
  if isTouchParallel ds.toolAngle t
    then AdjustLength { l0 = ds.toolLength, t = t } -- pulling left or right
    else let (sx, sy) = ds.toolStart
         in Rotate { angleOffset0 = ds.toolAngle - atan2 (t.y - sy) (t.x - sx), t = t }

-- t1 is the fixed finger (the one that went down first)
-- t2 is the finger that's moving to draw a line or an arc
initPencilEndGesture : NTouch -> DrawState -> Gesture
initPencilEndGesture t ds =
  if isTouchParallel ds.toolAngle t
    then DrawLine { t = t, p1 = ds.toolStart }
    else DrawArc { t = t, c = ds.toolStart, r = ds.toolLength }

onStartOrEnd : DrawState -> Point -> (Point -> a) -> (Point -> a) -> (() -> a) -> a
onStartOrEnd ds p onStart onEnd onNeither =
  let sDist = dist p ds.toolStart
      end   = toolEnd ds.toolStart ds.toolAngle ds.toolLength
      eDist = dist p end
  in if | sDist < eDist && sDist < 200  -> onStart ds.toolStart
        | eDist <= sDist && eDist < 200 -> onEnd end
        | otherwise                     -> onNeither ()

gesture : [NTouch] -> DrawState -> Gesture
gesture ts ds = -- Debug.log "gesture state" <|
  case (ds.gesture, ts) of
    (NoTouches, t::[]) ->
      onStartOrEnd ds (t.x, t.y)
        (\_ ->
           case ds.mode of
             Ruler -> Translate { toolP0 = ds.toolStart
                                , touchP0 = (t.x, t.y)
                                , t = t }
             _     -> NoTouches)

        (\_ ->
           case ds.mode of
             Ruler -> TouchEnd { t = t }
             Draw  -> PencilEnd { t = t }
             _     -> NoTouches)

        (\_ -> NoTouches)

    (Translate trans, t'::[]) ->
      if t'.id == trans.t.id
        then Translate { trans | t <- t' }
        else NoTouches

    -- rotate/adj length stuff
    (TouchEnd ep, t'::[]) ->
      -- dispatch only if the distance t2 has moved is big enough
      if t'.id == ep.t.id
        then if norm (displacement t') > 5
               then initEndGesture t' ds
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
        then if norm (displacement t') > 5
               then initPencilEndGesture t' ds
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
                Touches ts      -> touchesUpdate ts ds
                ChangeMode m    -> { ds | mode <- m }
                Tap p           -> tapUpdate p ds
                Resize (dw, dh) -> Debug.log "hi" { ds | displayWidth <- toFloat dw, displayHeight <- toFloat dh }

removeIndex : Int -> [(Int, a)] -> [a]
removeIndex i ixs = snd <| unzip <| filter (\(i', _) -> i' /= i) ixs

erase : DrawState -> Point -> DrawState
erase ds p =
    let iLines = zip [0..length ds.lines] ds.lines
        lineErasers = map (\(i, (p1, p2)) ->
                               ( (\ds -> { ds | lines <- removeIndex i iLines })
                               , min (dist p1 p) (dist p2 p)))
                      iLines
        iArcs = zip [0..length ds.arcs] ds.arcs
        arcErasers = map (\(i, ((cx, cy), r, a1, a2)) ->
                              let p1 = ( cx + r * cos a1
                                       , cy + r * sin a1 )
                                  p2 = ( cx + r * cos a2
                                       , cy + r * sin a2 )
                              in ( (\ds -> { ds | arcs <- removeIndex i iArcs })
                                 , min (dist p1 p) (dist p2 p) ))
                     iArcs
        iPoints = zip [0..length ds.points] ds.points
        pointErasers = map (\(i, p') ->
                                ( (\ds -> { ds | points <- removeIndex i iPoints })
                                , dist p' p ))
                       iPoints

        erasers = pointErasers ++ arcErasers ++ lineErasers
    in case sortBy snd erasers of
         (eraser, _) :: _ -> eraser ds
         []               -> ds

tapUpdate : Point -> DrawState -> DrawState
tapUpdate p ds =
    case ds.mode of
      Draw ->
          onStartOrEnd ds p
            (\_   -> { ds | points <- ds.toolStart :: ds.points })
            (\end -> { ds | points <- end :: ds.points })
            (\_   -> ds)
      Erase -> erase ds p
      Ruler -> ds

touchesUpdate : [NTouch] -> DrawState -> DrawState
touchesUpdate ts ds =
  let gest = gesture ts ds
      ds' = { ds | gesture <- gest }
  in case gest of
       Translate { toolP0, touchP0, t } -> -- TODO optimize so we don't do all this bounding always
         let toolStart' = boundPoint ((-ds.displayWidth/2, ds.displayWidth/2), (-ds.displayHeight/2, ds.displayHeight/2))
                          <| toolP0 `addv` ((t.x, t.y) `subv` touchP0)
         in { ds' | toolStart <- toolStart' }

       AdjustLength { l0, t } ->
         let toolLength' = l0 + (t.x - t.x0) * cos ds'.toolAngle +
                                (t.y - t.y0) * sin ds'.toolAngle
         in { ds' | toolLength <- bound (0, ds.displayWidth) toolLength' }

       Rotate { angleOffset0, t } ->
         let (sx, sy) = ds'.toolStart
             touchAngle = atan2 (t.y - sy) (t.x - sx)
             toolAngle' = angleOffset0 + touchAngle
             (ex', ey') = ( sx + ds.toolLength * cos toolAngle'
                          , sy + ds.toolLength * sin toolAngle' )
             (ex'', ey'') = boundPoint ((-ds.displayWidth/2, ds.displayWidth/2),
                                        (-ds.displayHeight/2, ds.displayHeight/2))
                                       (ex', ey')
             toolAngle'' = atan2 (ey'' - sy) (ex'' - sx)
         in { ds' | toolAngle <- toolAngle'' }

       DrawLine { t, p1 } ->
         let theta = angularDist t - ds.toolAngle
             r = (norm (displacement t)) * cos theta
             lineLength = bound (0, ds.toolLength) r
         in { ds' | drawing <- DrawingLine (p1,
                                            (fst p1 + lineLength*(cos ds.toolAngle),
                                             snd p1 + lineLength*(sin ds.toolAngle))) }

       DrawArc { t, c, r } ->
         let (sx, sy) = ds'.toolStart
             arcAngle' = atan2 (t.y - sy) (t.x - sx)
         in { ds' | drawing <-
               case ds'.drawing of
                 DrawingArc counter (_, _, _, arcAngle) ->
                   let counter1 =
                       case counter of
                         Nothing -> if abs (arcAngle - arcAngle') > pi
                                    then Just arcAngle
                                    else Nothing
                         Just oldAngle ->
                             let gap = oldAngle - arcAngle'
                             in if | gap > pi  -> Just (2*pi + arcAngle')
                                   | gap < -pi -> Just (arcAngle' - 2*pi)
                                   | otherwise -> Nothing
                   in DrawingArc counter1 <|
                                 case counter1 of
                                   Nothing ->
                                     (c, r, ds'.toolAngle, arcAngle')
                                   Just oldAngle ->
                                     (c, r, ds'.toolAngle, oldAngle)
                 _ -> DrawingArc Nothing (c, r, ds'.toolAngle, arcAngle') }

       otherwise ->
         case ds'.drawing of
           NotDrawing -> ds'
           DrawingLine l -> { ds' | drawing <- NotDrawing
                                  , lines <- l :: ds'.lines } -- TODO get rid of drawing earlier
           DrawingArc _ a -> { ds' | drawing <- NotDrawing
                                   , arcs <- a :: ds'.arcs } -- TODO get rid of drawing earlier

-- display
unitWidth = 20
markHeight = 20

rulerTopHeight = 20
rulerBottomHeight = 20

{-| An elliptical arc with the given center, radii and angle interval. -}
arc : (Float, Float) -> (Float, Float) -> (Float, Float) -> Shape
arc (cx, cy) (a, b) (startAngle, endAngle) =
  let n = 720
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
                                , circle 4 |> filled (rgba 255 0 0 0.5)
                                , circle 4 |> filled (rgba 0 255 0 0.5) |> moveX len ]
                        |> move c
                        |> rotate angle

display : DrawState -> Element
display { displayWidth, displayHeight,
          lines, arcs, points, toolStart, toolAngle, toolLength, drawing } =
        let linesForms = map (traced defaultLine . uncurry segment)
                         <| case drawing of
                              NotDrawing -> lines
                              DrawingLine l -> l :: lines
                              DrawingArc _ _ -> lines
            arcForms = map (\(c, l, a1, a2) -> traced defaultLine <| arc c (l, l) (a1, a2))
                       <| case drawing of
                            NotDrawing -> arcs
                            DrawingLine _ -> arcs
                            DrawingArc _ a -> a :: arcs
            pointForms = map (\p -> move p <| filled black <| circle 5) points
        in collage (ceiling displayWidth) (ceiling displayHeight)
           <| pointForms ++ linesForms ++ arcForms ++ [displayTool toolStart toolAngle toolLength]

-- problem display
problem : Form
problem = let triangle = group
                         <| map (outlined (solid darkGray))
                         <| [ segment (0, 50) (200, 0)
                            , segment (200, 0) (0, 195)
                            , segment (0, 195) (0, 50) ]
              labels = group
                       <| [ move (0, 30) <| toForm <| plainText "A"
                          , move (210, 0) <| toForm <| plainText "B"
                          , move (0, 210) <| toForm <| plainText "C" ]
          in moveY 100
             <| group [triangle, labels]

displayProblem : Float -> Float -> Element
displayProblem ww wh = collage (ceiling ww) (ceiling wh) [problem]

-- touch analysis
recenter : Int -> Int -> T.Touch -> NTouch
recenter cx cy t = { x = toFloat <| t.x - cx,
                     y = toFloat <| -t.y + cy,
                     x0 = toFloat <| t.x0 - cx,
                     y0 = toFloat <| -t.y0 + cy,
                     id = t.id,
                     t0 = t.t0 }

touchesR : Signal [NTouch]
touchesR = let recenterAndResortAll (dw, dh) ts =
                   let (cx, cy) = (dw `div` 2, dh `div` 2)
                   in map (recenter cx cy) <| sortBy ((\x -> -x) . .t0) ts
           in recenterAndResortAll <~ Window.dimensions ~ T.touches

fingerSymbol : NTouch -> Form
fingerSymbol {x, y} = circle 50
                    |> filled green
                    |> move (x, y)

touchesView : Signal Element
touchesView = let viewTouches (dw, dh) ts = collage dw dh
                                         <| map fingerSymbol ts
              in viewTouches <~ Window.dimensions ~ touchesR

-- modes
data Mode = Ruler | Draw | Erase

port modeString : Signal String

mode : Signal Mode
mode = (\ms -> case ms of
                 "ruler" -> Ruler
                 "draw" -> Draw
                 "erase" -> Erase) <~ modeString

-- taps
tapsR : Signal Point
tapsR =
  let cx = displayWidth `div` 2
      cy = displayHeight `div` 2
  in (\t -> (toFloat (t.x - cx), toFloat (-t.y + cy))) <~ T.taps

-- merging
data Action = Touches [NTouch] | Tap Point | ChangeMode Mode | Resize (Int, Int)

-- this is a hack: https://groups.google.com/d/topic/elm-discuss/pevppMaHyyA/discussion
-- and https://groups.google.com/d/topic/elm-discuss/X4wmckEtMyg/discussion
port initialDimensions : Signal (Int, Int)

withInitialDimensions : Signal (Int, Int)
withInitialDimensions = merge Window.dimensions initialDimensions

actions : Signal Action
actions = merges [ lift Resize withInitialDimensions
                 , lift Tap tapsR
                 , lift Touches touchesR 
                 , lift ChangeMode mode ]

-- overall display
displayWidth = 1371
displayHeight = 660

displayS : Signal Element
displayS = (\ds -> layers [ displayProblem ds.displayWidth ds.displayHeight
                          , display ds ])
           <~ (foldp update initialDrawState <| actions)

main = (\(dw, dh) x y -> layers [spacer dw dh |> color gray, x, y]) <~ Window.dimensions ~ touchesView ~ displayS
