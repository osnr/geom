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
                           , phi : Angle }

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
             , (b - s*cos (degrees theta), s*sin (degrees theta))
             , (0, 0) ]
          |> outlined defaultLine

    Parallelogram { b, s, theta } ->
        path [ (0, 0)
             , (b, 0)
             , (b - s*cos (degrees theta), s*sin (degrees theta))
             , (-s*cos (degrees theta), s*sin (degrees theta))
             , (0, 0) ]
          |> outlined defaultLine

    Quadrilateral { b, s1, s2, theta, phi } ->
        path [ (0, 0)
             , (b, 0)
             , (b - s1*cos (degrees theta), s1*sin (degrees theta))
             , (-s2*cos (degrees phi), s2*sin (degrees phi))
             , (0, 0) ]
          |> outlined defaultLine
