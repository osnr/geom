module Mode where

import Graphics.Input as I
import Types (Mode (..))

modeI : I.Input Mode
modeI = I.input Ruler

mode : Signal Mode
mode = modeI.signal

modeButtons : Element
modeButtons = flow down [ I.button modeI.handle Ruler "Ruler"
                        , I.button modeI.handle Draw "Draw"
                        , I.button modeI.handle Erase "Erase" ]
