import Window

import Problem
import ProblemParser
import Touches
import Mode
import DrawState as DS

import Types (Action (..))

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

displayS : Signal Element
displayS = (\ds -> layers [ Problem.displayProblem ds.displayWidth ds.displayHeight
                          , DS.display ds ])
           <~ DS.drawStateS actions

main = (\(dw, dh) tv disp modeBtns ->
            layers [ spacer dw dh |> color gray
                   , tv
                   , disp
                   , container dw dh topRight modeBtns ])
       <~ Window.dimensions
        ~ Touches.touchesView
        ~ displayS
        ~ Mode.modeButtonsS
