module Gesture where

import Util (..)
import Types (..)

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
