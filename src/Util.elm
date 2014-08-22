module Util where

import Types (..)

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

{-| An elliptical arc with the given center, radii and angle interval. -}
arc : (Float, Float) -> (Float, Float) -> (Float, Float) -> Shape
arc (cx, cy) (a, b) (startAngle, endAngle) =
  let n = 720
      t = (endAngle - startAngle) / n
      f i = (cx + a * cos (t*i + startAngle), cy + b * sin (t*i + startAngle))
  in map f [0..n-1]

removeIndex : Int -> [(Int, a)] -> [a]
removeIndex i ixs = snd <| unzip <| filter (\(i', _) -> i' /= i) ixs
