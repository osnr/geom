module Mode where

import Graphics.Input as I
import Types (Mode, Ruler, Draw, Erase)

import Window
import Touch

modes : [Mode]
modes = [Ruler, Draw, Erase]

modeI : I.Input Mode
modeI = I.input Ruler

boxWidth = 150
boxHeight = 300

dimTaps : Signal ((Int, Int), { x : Int, y : Int })
dimTaps = lift2 (,) Window.dimensions Touch.taps

inBox : ((Int, Int), { x : Int, y : Int }) -> Bool
inBox ((ww, wh), {x, y}) = x > ww - boxWidth && y < boxHeight

tapToMode : { x : Int, y : Int } -> Mode
tapToMode {x, y} =
  let idx = y `div` (boxHeight `div` length modes)
  in head <| drop idx modes

tappedMode : Signal Mode
tappedMode = tapToMode . snd <~ keepIf inBox ((0,0),{x=0,y=0}) dimTaps

mode : Signal Mode
mode = merge tappedMode modeI.signal

modeButton : Mode -> Mode -> Element
modeButton selectedMode m =
  I.button modeI.handle m (show m)
  |> size boxWidth (boxHeight `div` length modes)
  |> if selectedMode == m
       then color white
       else color gray

modeButtons : Mode -> Element
modeButtons selectedMode = size boxWidth boxHeight
                           <| flow down
                           <| map (modeButton selectedMode) modes

modeButtonsS : Signal Element
modeButtonsS = modeButtons <~ mode
