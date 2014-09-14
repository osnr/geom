import Window
import Either (isRight)
import Dict
import Debug

import Problem
import ProblemParser
import Touches
import Mode
import DrawState as DS

import Either (Either, Left, Right)
import Types (..)

-- this is a hack: https://groups.google.com/d/topic/elm-discuss/pevppMaHyyA/discussion
-- and https://groups.google.com/d/topic/elm-discuss/X4wmckEtMyg/discussion
port initialDimensions : Signal (Int, Int)

withInitialDimensions : Signal (Int, Int)
withInitialDimensions = merge Window.dimensions initialDimensions

-- action pattern so we can use one foldp to store the state of the whole world
-- every change that happens to DrawState comes from here
actions : Signal Action
actions = merges [ lift (ChangeProblem . toProblemDict) <| keepIf isRight (Right []) problemS
                 , lift ChangeMode Mode.mode
                 , lift Resize withInitialDimensions
                 , lift Tap Touches.tapsR
                 , lift Touches Touches.touchesR ]

port problemText : Signal String -- get the problem text from the outside world

problemS : Signal (Either String Problem)
problemS = ProblemParser.parseProblem <~ problemText

toProblemDict : Either String Problem -> ProblemDict
toProblemDict (Right p) = Dict.fromList . zip [0..length p] <| p

problemErrorDisplayS : Signal Element
problemErrorDisplayS =
  let problemDisplay mp =
        case mp of
          Left err -> plainText err
          Right _  -> empty
  in problemDisplay <~ problemS

displayS : Signal Element
displayS = DS.display <~ DS.drawStateS actions

-- overlay all of the pieces of the GUI
main = (\(dw, dh) tv problemErrorDisp disp modeBtns ->
            layers [ spacer dw dh |> color gray -- gray background of screen
                   , tv -- green circles where you're touching

                   , problemErrorDisp -- parse and semantic errors loading problem

                   , disp -- the important one
                          -- displaying everything that depends on DrawState object
                          -- (problem shapes, drawings, ruler)

                   , container dw dh topRight modeBtns -- mode buttons
                   ])
       <~ withInitialDimensions
        ~ Touches.touchesView
        ~ problemErrorDisplayS
        ~ displayS
        ~ Mode.modeButtonsS
