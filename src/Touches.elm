module Touches where

import Touch as T
import Mode
import Window
import Types (NTouch, Point)

recenter : Int -> Int -> T.Touch -> NTouch
recenter cx cy t = { x = toFloat <| t.x - cx,
                     y = toFloat <| -t.y + cy,
                     x0 = toFloat <| t.x0 - cx,
                     y0 = toFloat <| -t.y0 + cy,
                     id = t.id,
                     t0 = t.t0 }

isModeSwitch : (Int, Int) -> (Int, Int) -> Bool
isModeSwitch (dw, dh) (x, y) = x > dw - Mode.boxWidth &&
                               y < Mode.boxHeight

touchesR : Signal [NTouch]
touchesR = let recenterAndResortAll (dw, dh) ts =
                   let (cx, cy) = (dw // 2, dh // 2)
                       touchIsModeSwitch t = isModeSwitch (dw, dh) (t.x, t.y)
                   -- TODO we're expecting a sort here atm but don't really need it
                   in map (recenter cx cy) <| filter (not . touchIsModeSwitch) <| ts
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
  let toRecenteredPoint ((dw, dh), {x, y}) =
        let cx = dw // 2
            cy = dh // 2
        in (toFloat (x - cx), toFloat (-y + cy))

      tapIsModeSwitch (dims, {x, y}) = isModeSwitch dims (x, y)
  in lift toRecenteredPoint
       <| dropIf tapIsModeSwitch ((1, 1), { x = -99, y = -99 })
       <| (,) <~ Window.dimensions ~ T.taps
