module DrawState where

import Gesture as G
import Tool as TL
import Util (..)
import Types (..)

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

update : Action -> DrawState -> DrawState
update a ds = case a of
                Touches ts      -> touchesUpdate ts ds
                ChangeMode m    -> { ds | mode <- m }
                Tap p           -> tapUpdate p ds
                Resize (dw, dh) -> { ds | displayWidth <- toFloat dw, displayHeight <- toFloat dh }

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
          G.onStartOrEnd ds p
            (\_   -> { ds | points <- ds.toolStart :: ds.points })
            (\end -> { ds | points <- end :: ds.points })
            (\_   -> ds)
      Erase -> erase ds p
      Ruler -> ds

touchesUpdate : [NTouch] -> DrawState -> DrawState
touchesUpdate ts ds =
  let gest = G.gesture ts ds
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
           <| pointForms ++ linesForms ++ arcForms ++ [TL.displayTool toolStart toolAngle toolLength]

drawStateS : Signal Action -> Signal DrawState
drawStateS actions = foldp update initialDrawState actions
