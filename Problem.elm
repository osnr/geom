module Problem where

problem : Form
problem = let triangle = group
                         <| map (outlined (solid darkGray))
                         <| [ segment (0, 50) (200, 0)
                            , segment (200, 0) (0, 195)
                            , segment (0, 195) (0, 50) ]
              labels = group
                       <| [ move (0, 30) <| toForm <| plainText "A"
                          , move (210, 0) <| toForm <| plainText "B"
                          , move (0, 210) <| toForm <| plainText "C" ]
          in moveY 100
             <| group [triangle, labels]

displayProblem : Float -> Float -> Element
displayProblem ww wh = collage (ceiling ww) (ceiling wh) [problem]
