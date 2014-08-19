module Touches where

import Touch as T
import Window
import Types (NTouch, Point)

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

tapsR : Signal Point
tapsR =
  let recenter (dw, dh) t =
        let cx = dw `div` 2
            cy = dh `div` 2
        in (toFloat (t.x - cx), toFloat (-t.y + cy))
  in recenter <~ Window.dimensions ~ T.taps
