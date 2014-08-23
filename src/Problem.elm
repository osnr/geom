module Problem where

import Types (Length, Angle)

type Context a = { pos : (Float, Float) -- FIXME not Point?
                 , angle : Angle

                 , child : a }

data Shape = Point

           | Line { length : Length }

           | Circle { r : Length }

           | Triangle { b : Length
                      , s : Length
                      , theta : Angle }

           | Parallelogram { b : Length
                           , s : Length
                           , theta : Angle }

           | Quadrilateral { b : Length
                           , s1 : Length
                           , s2 : Length
                           , theta : Angle
                           , delta : Angle }

type Problem = [Context Shape]

display : Int -> Int -> Problem -> Element
display dw dh = collage dw dh . map displayCShape

displayCShape : Context Shape -> Form
displayCShape { pos, angle, child } = displayShape child
                                    |> rotate (degrees angle)
                                    |> move pos

displayShape : Shape -> Form
displayShape sh =
  case sh of
    Point ->
        circle 2 |> filled black

    Line { length } ->
        segment (0, 0) (length, 0) |> traced defaultLine

    Circle { r } ->
        circle r |> outlined defaultLine

    Triangle { b, s, theta } ->
        path [ (0, 0)
             , (b, 0)
             , (b - s*cos (degrees theta), s*cos (degrees theta))
             , (0, 0) ]
          |> outlined defaultLine

    -- Parallelogram { b, s, theta } ->
    -- Quadrilateral { b, s1, s2, theta, delta } ->

-- problem : Form
-- problem = let triangle = group
--                          <| map (outlined (solid darkGray))
--                          <| [ segment (0, 50) (200, 0)
--                             , segment (200, 0) (0, 195)
--                             , segment (0, 195) (0, 50) ]
--               labels = group
--                        <| [ move (0, 30) <| toForm <| plainText "A"
--                           , move (210, 0) <| toForm <| plainText "B"
--                           , move (0, 210) <| toForm <| plainText "C" ]
--           in moveY 100
--              <| group [triangle, labels]

-- displayProblem : Float -> Float -> Element
-- displayProblem ww wh = collage (ceiling ww) (ceiling wh) [problem]
