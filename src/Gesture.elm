module Gesture where

import Dict

import Debug
import Problem (lengthOf)

import Util (..)
import Types (..)

insideRuler : DrawState -> NTouch -> Bool
insideRuler ds {x, y} =
  let start = ds.tool.pos
      end = toolEnd ds.tool

      distToRuler = distToSegment (x, y) start end
  in distToRuler < 50

-- used to determine whether we're lengthening ruler / drawing a line
-- or rotating ruler / drawing an arc.
-- check whether the user's gesture has been more in a parallel or perpendicular direction
isTouchParallel : Angle -> NTouch -> Bool
isTouchParallel toolAngle t =
  let theta = angularDist t - toolAngle
  in abs (cos theta) > abs (sin theta)

initToolEndGesture : NTouch -> DrawState -> Gesture
initToolEndGesture t ds =
  if isTouchParallel ds.tool.angle t
    then ScaleTool { l0 = ds.tool.child.length, t = t } -- pulling left or right
    else let (sx, sy) = ds.tool.pos
         in RotateTool { angleOffset0 = ds.tool.angle - atan2 (t.y - sy) (t.x - sx), t = t }

-- FIXME huge code duplication btwn tool / object
initObjEndGesture : NTouch -> ObjectKey -> DrawState -> Gesture
initObjEndGesture t ok ds =
  let obj = Dict.getOrFail ok ds.problem
  in if isTouchParallel obj.angle t
       then ScaleObj { l0 = lengthOf obj, ok = ok, t = t } -- pulling left or right
       else let (sx, sy) = obj.pos
            in RotateObj { angleOffset0 = obj.angle - atan2 (t.y - sy) (t.x - sx)
                         , ok = ok
                         , t = t }

rotate : Float -> Float -> Angle -> NTouch -> Point -> Length -> Angle
rotate dw dh angleOffset0 t (sx, sy) len =
  let touchAngle = atan2 (t.y - sy) (t.x - sx)
      angle' = angleOffset0 + touchAngle
      (ex', ey') = ( sx + len * cos angle'
                   , sy + len * sin angle' )
      (ex'', ey'') = boundPoint ((-dw/2, dw/2), (-dh/2, dh/2)) (ex', ey')

  in atan2 (ey'' - sy) (ex'' - sx)

-- t1 is the fixed finger (the one that went down first)
-- t2 is the finger that's moving to draw a line or an arc
initPencilEndGesture : NTouch -> DrawState -> Gesture
initPencilEndGesture t ds =
  if isTouchParallel ds.tool.angle t
    then DrawLine { t = t, p1 = ds.tool.pos }
    else DrawArc { t = t, c = ds.tool.pos, r = ds.tool.child.length }

onStartOrEnd : DrawState -> Point -> (Point -> a) -> (Point -> a) -> (() -> a) -> a
onStartOrEnd ds p onStart onEnd onNeither =
  let sDist = dist p ds.tool.pos
      end   = toolEnd ds.tool
      eDist = dist p end
  in if | sDist < eDist && sDist < 200  -> onStart ds.tool.pos
        | eDist <= sDist && eDist < 200 -> onEnd end
        | otherwise                     -> onNeither ()

data CloserTo = Start | End
findObject : DrawState -> Point -> Maybe (Int, Context Object, CloserTo)
findObject {problem} p =
  let toStart (idx, obj) = (idx, dist p obj.pos)
      toEnd (idx, obj) = ( idx
                         , dist p ( fst obj.pos + (lengthOf obj)*(cos obj.angle)
                                  , snd obj.pos + (lengthOf obj)*(sin obj.angle) ) )

      indexedObjs = Dict.toList problem
      indexedStarts = map toStart indexedObjs
      indexedEnds = map toEnd indexedObjs
  in case indexedObjs of
       []   -> Nothing
       _::_ -> let (startK, startDist) = head . sortBy snd <| indexedStarts
                   (endK, endDist) = head . sortBy snd <| indexedEnds

                   (ok, ct) = if startDist >= endDist
                                 then (endK, End)
                                 else (startK, Start) -- end should take precedence
               in Just <| (ok, Dict.getOrFail ok problem, ct)

gesture : [NTouch] -> DrawState -> Gesture
gesture ts ds = -- Debug.log "gesture state" <|
  case (ds.gesture, ts) of
    (NoTouches, t::[]) ->
      case ds.mode of
        Object ->
          case findObject ds (t.x, t.y) of
            Nothing -> NoTouches
            Just (ok, obj, Start) ->
              TranslateObj { ok = ok
                           , oP0 = obj.pos
                           , touchP0 = (t.x, t.y)
                           , t = t }

            Just (ok, obj, End) ->
              TouchEndObj { ok = ok
                          , t = t }

        _ ->
          onStartOrEnd ds (t.x, t.y)
            (\_ ->
               case ds.mode of
                 Ruler  -> TranslateTool { toolP0 = ds.tool.pos
                                         , touchP0 = (t.x, t.y)
                                         , t = t }
                 _      -> NoTouches)

            (\_ ->
               case ds.mode of
                 Ruler  -> TouchEndTool { t = t }
                 Draw   -> PencilEnd { t = t }
                 _      -> NoTouches)

            (\_ -> NoTouches)

    -- TOOL MODE
    ------------
    (TranslateTool trans, t'::[]) ->
      if t'.id == trans.t.id
        then TranslateTool { trans | t <- t' }
        else NoTouches

    -- rotate/adj length stuff
    (TouchEndTool ep, t'::[]) ->
      -- dispatch only if the distance t2 has moved is big enough
      if t'.id == ep.t.id
        then if norm (displacement t') > 5
               then initToolEndGesture t' ds
               else TouchEndTool { ep | t <- t' }
        else NoTouches

    (ScaleTool adj, t'::[]) ->
      if t'.id == adj.t.id
        then ScaleTool { adj | t <- t' }
        else NoTouches

    (RotateTool rot, t'::[]) ->
      if t'.id == rot.t.id
        then RotateTool { rot | t <- t' }
        else NoTouches

    -- OBJECT MODE
    --------------
    (TranslateObj trans, t'::[]) ->
      if t'.id == trans.t.id
        then TranslateObj { trans | t <- t' }
        else NoTouches

    (TouchEndObj ep, t'::[]) ->
      -- dispatch only if the distance t2 has moved is big enough
      if t'.id == ep.t.id
        then if norm (displacement t') > 5
               then initObjEndGesture t' ep.ok ds
               else TouchEndObj { ep | t <- t' }
        else NoTouches

    (ScaleObj adj, t'::[]) ->
      if t'.id == adj.t.id
        then ScaleObj { adj | t <- t' }
        else NoTouches

    (RotateObj rot, t'::[]) ->
      if t'.id == rot.t.id
        then RotateObj { rot | t <- t' }
        else NoTouches

    -- PENCIL MODE
    --------------
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
