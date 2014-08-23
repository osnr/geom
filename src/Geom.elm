import Window

import Problem
import ProblemParser
import Touches
import Mode
import DrawState as DS

import Either (Either, Left, Right)
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

problemS : Signal (Either String Problem.Problem)
problemS = ProblemParser.parseProblem <~ problemText

problemDisplayS : Signal Element
problemDisplayS =
  let problemDisplay (dw, dh) mp =
        case mp of
          Left err -> plainText err
          Right p  -> Problem.display dw dh p
  in problemDisplay <~ withInitialDimensions ~ problemS

displayS : Signal Element
displayS = DS.display <~ DS.drawStateS actions

main = (\(dw, dh) tv problemDisp disp modeBtns ->
            layers [ spacer dw dh |> color gray
                   , tv

                   , problemDisp
                   , disp

                   , container dw dh topRight modeBtns ])
       <~ withInitialDimensions
        ~ Touches.touchesView
        ~ problemDisplayS
        ~ displayS
        ~ Mode.modeButtonsS
