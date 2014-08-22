module Mode where

import Graphics.Input as I
import Types (Mode (..))

modes : [Mode]
modes = [Ruler, Draw, Erase]

modeI : I.Input Mode
modeI = I.input Ruler

mode : Signal Mode
mode = modeI.signal

boxWidth = 150
boxHeight = 300

modeButton : Mode -> Mode -> Element
modeButton selectedMode m =
  I.button modeI.handle m (show m)
  |> size boxWidth (boxHeight // length modes)
  |> if selectedMode == m
       then color white
       else color gray

modeButtons : Mode -> Element
modeButtons selectedMode = size boxWidth boxHeight
                           <| flow down
                           <| map (modeButton selectedMode) modes

modeButtonsS : Signal Element
modeButtonsS = modeButtons <~ mode
