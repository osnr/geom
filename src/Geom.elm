import Window

import Problem
import ProblemParser
import Touches
import Mode
import DrawState as DS

import Types (Action, Resize, Tap, Touches, ChangeMode)

-- this is a hack: https://groups.google.com/d/topic/elm-discuss/pevppMaHyyA/discussion
-- and https://groups.google.com/d/topic/elm-discuss/X4wmckEtMyg/discussion
port initialDimensions : Signal (Int, Int)

withInitialDimensions : Signal (Int, Int)
withInitialDimensions = merge Window.dimensions initialDimensions

actions : Signal Action
actions = merges [ lift Resize withInitialDimensions
                 , lift Tap Touches.tapsR
                 , lift Touches Touches.touchesR 
                 , lift ChangeMode Mode.mode ]

port problemText : Signal String

problemDisplayS : Signal Element
problemDisplayS = width 300 . asText . ProblemParser.parseProblem <~ problemText

displayS : Signal Element
displayS = DS.display <~ DS.drawStateS actions

main = (\(dw, dh) tv disp problemDisp modeBtns ->
            layers [ spacer dw dh |> color gray
                   , tv
                   , disp
                   , problemDisp
                   , container dw dh topRight modeBtns ])
       <~ Window.dimensions
        ~ Touches.touchesView
        ~ displayS
        ~ problemDisplayS
        ~ Mode.modeButtonsS
