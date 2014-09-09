module DrawState where

import Debug
import Dict

import Gesture as G
import Problem

import Tool as TL
import Util (..)
import Types (..)

initialDrawState : DrawState
initialDrawState = { problem = Dict.empty

                   , lines = []
                   , arcs = []
                   , points = []

                   , tool = {
                       pos = (0, 0)
                     , angle = 0
                     , child = { length = 300 }
                     }

                   , displayWidth = 800
                   , displayHeight = 600
                   , mode = Ruler -- kind of annoying that this is duplicated

                   , gesture = NoTouches
                   , drawing = NotDrawing
                   }

update : Action -> DrawState -> DrawState
update a ds = case a of
                ChangeProblem p -> { ds | problem <- p }
                Touches ts      -> touchesUpdate ts ds
                ChangeMode m    -> { ds | mode <- m }
                Tap p           -> tapUpdate p ds
                Resize (dw, dh) -> { ds | displayWidth <- toFloat dw
                                        , displayHeight <- toFloat dh }

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
      Draw -> -- draw point on tap
          G.onStartOrEnd ds p
            (\_   -> { ds | points <- ds.tool.pos :: ds.points })
            (\end -> { ds | points <- end :: ds.points })
            (\_   -> ds)
      Erase -> erase ds p
      Object -> ds
      Ruler -> ds

touchesUpdate : [NTouch] -> DrawState -> DrawState
touchesUpdate ts ds =
  let gest = G.gesture ts ds
      ds' = { ds | gesture <- gest }
  in case gest of
       TranslateTool { toolP0, touchP0, t } -> -- TODO optimize so we don't do all this bounding always
         let toolStart' = boundPoint ((-ds'.displayWidth/2, ds'.displayWidth/2), (-ds'.displayHeight/2, ds'.displayHeight/2))
                          <| toolP0 `addv` ((t.x, t.y) `subv` touchP0)
             tool = ds'.tool
         in { ds' | tool <- { tool | pos <- toolStart' } }

       ScaleTool { l0, t } ->
         let toolLength' = l0 + (t.x - t.x0) * cos ds'.tool.angle +
                                (t.y - t.y0) * sin ds'.tool.angle
             tool = ds'.tool
             child = ds'.tool.child
         in { ds' | tool <- { tool | child <- { child | length <- bound (0, ds.displayWidth) toolLength' } } }

       RotateTool { angleOffset0, t } ->
         let tool = ds'.tool
         in { ds' | tool <-
              { tool | angle <- G.rotate ds'.displayWidth ds'.displayHeight
                                         angleOffset0 t
                                         ds'.tool.pos ds'.tool.child.length } }

       -- OBJECT MODE
       --------------
       TranslateObj { ok, oP0, touchP0, t } -> -- TODO optimize so we don't do all this bounding always
         let pos' = boundPoint ( (-ds'.displayWidth/2, ds'.displayWidth/2)
                               , (-ds'.displayHeight/2, ds'.displayHeight/2) )
                    <| oP0 `addv` ((t.x, t.y) `subv` touchP0)
             tran (Just obj) = Just { obj | pos <- pos' }
         in { ds' | problem <- Dict.update ok tran ds'.problem }

       ScaleObj { ok, l0, t } ->
         let scal (Just obj) =
               let len' = l0 + (t.x - t.x0) * cos obj.angle +
                               (t.y - t.y0) * sin obj.angle
                        |> bound (0, ds.displayWidth)
               in Just { obj | child <- Problem.setObjLength obj.child len' }
         in { ds' | problem <- Dict.update ok scal ds'.problem }

       RotateObj { ok, angleOffset0, t } ->
         let obj = Dict.getOrFail ok ds'.problem
             angle' = G.rotate ds'.displayWidth ds'.displayHeight
                               angleOffset0 t
                               obj.pos (Problem.lengthOf obj)
             obj' = { obj | angle <- angle' }
         in { ds' | problem <- Dict.insert ok obj' ds'.problem }

       -- DRAW MODE
       ------------
       DrawLine { t, p1 } ->
         let theta = angularDist t - ds.tool.angle
             r = (norm (displacement t)) * cos theta
             lineLength = bound (0, ds.tool.child.length) r
         in { ds' | drawing <- DrawingLine (p1,
                                            (fst p1 + lineLength*(cos ds.tool.angle),
                                             snd p1 + lineLength*(sin ds.tool.angle))) }

       DrawArc { t, c, r } ->
         let (sx, sy) = ds'.tool.pos
             arcAngle' = atan2 (t.y - sy) (t.x - sx)
         in { ds' | drawing <-
               case ds'.drawing of
                 DrawingArc counter minAngle maxAngle (_, _, _, arcAngle) ->
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

                       arcAngle'' = case counter1 of
                                      Nothing       -> arcAngle'
                                      Just oldAngle -> oldAngle
                   in DrawingArc counter1 (min minAngle arcAngle'') (max maxAngle arcAngle'')
                        (c, r, minAngle, maxAngle)

                 _ -> DrawingArc Nothing (min ds'.tool.angle arcAngle') (max ds'.tool.angle arcAngle')
                        (c, r, ds'.tool.angle, arcAngle') }

       otherwise ->
         case ds'.drawing of
           NotDrawing -> ds'
           DrawingLine l -> { ds' | drawing <- NotDrawing
                                  , lines <- l :: ds'.lines } -- TODO get rid of drawing earlier
           DrawingArc _ _ _ a -> { ds' | drawing <- NotDrawing
                                       , arcs <- a :: ds'.arcs } -- TODO get rid of drawing earlier

display : DrawState -> Element
display { displayWidth, displayHeight, mode,
          problem,
          lines, arcs, points, tool, drawing } =
        let linesForms = map (traced defaultLine . uncurry segment)
                         <| case drawing of
                              NotDrawing -> lines
                              DrawingLine l -> l :: lines
                              DrawingArc _ _ _ _ -> lines
            arcForms = map (\(c, l, a1, a2) -> traced defaultLine <| arc c (l, l) (a1, a2))
                       <| case drawing of
                            NotDrawing -> arcs
                            DrawingLine _ -> arcs
                            DrawingArc _ _ _ a -> a :: arcs
            pointForms = map (\p -> move p <| filled black <| circle 5) points
        in collage (ceiling displayWidth) (ceiling displayHeight)
           <| Problem.display (mode == Object) problem ::
              pointForms ++
              linesForms ++
              arcForms ++
              [ TL.displayTool (mode == Ruler) tool.pos tool.angle tool.child.length ]

drawStateS : Signal Action -> Signal DrawState
drawStateS actions = foldp update initialDrawState actions
