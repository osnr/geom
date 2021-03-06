module Problem where
import Debug
import Dict
import Types (..)

display : Bool -> ProblemDict -> Form
display objMode = group . map (displayCShape objMode) . Dict.values

styleCShape : Bool -> Bool -> Shape -> Form
styleCShape manipulable scalable =
  case (manipulable, scalable) of
    (False, False) -> outlined (solid brown)
    (True, False) -> filled (rgba 255 255 0 0.5)
    (True, True) -> filled (rgba 0 255 255 0.5)

    -- this really shouldn't happen
    (False, True) -> outlined (solid brown)

displayCShape : Bool -> Context Object -> Form
displayCShape objMode { pos, angle, child, manipulable, scalable } =
  group ([displayShape child |> styleCShape manipulable scalable] ++
         if (objMode && manipulable) then [displayStartEnd child] else [])
  |> rotate angle
  |> move (fst pos, snd pos)

displayShape : Object -> Shape
displayShape obj =
  case obj of
    Point ->
        circle 2

    Line { length } ->
        segment (0, 0) (length, 0)

    Circle { r } ->
        circle r

    Triangle { b, s, theta } ->
        path [ (0, 0)
             , (b, 0)
             , (b - s*cos theta, s*sin theta)
             , (0, 0) ]

    Parallelogram { b, s, theta } ->
        path [ (0, 0)
             , (b, 0)
             , (b - s*cos theta, s*sin theta)
             , (-s*cos theta, s*sin theta)
             , (0, 0) ]

    Quadrilateral { b, s1, s2, theta, phi } ->
        path [ (0, 0)
             , (b, 0)
             , (b - s1*cos theta, s1*sin theta)
             , (-s2*cos phi, s2*sin phi)
             , (0, 0) ]

displayStartEnd : Object -> Form
displayStartEnd obj =
  let end = circle 4
          |> filled (rgba 0 255 0 0.5)
          |> case obj of
               Point -> id
               Line {length} -> moveX length
               Circle {r} -> moveX r
               Triangle {b} -> moveX b
               Parallelogram {b} -> moveX b
               Quadrilateral {b} -> moveX b

  in group [ circle 4 |> filled (rgba 255 0 0 0.5)
           , end ]

lengthOf : Context Object -> Length
lengthOf { child } =
  case child of
    Point -> 0
    Line {length} -> length
    Circle {r} -> r
    Triangle {b} -> b
    Parallelogram {b} -> b
    Quadrilateral {b} -> b

setObjLength : Object -> Length -> Object
setObjLength child l =
  case child of
    Point -> Point
    Line params -> Line { params | length <- l }
    Circle params -> Circle { params | r <- l }
    Triangle params -> Triangle { params | b <- l, s <- (l/params.b)*params.s }
    Parallelogram params -> Parallelogram { params | b <- l, s <- (l/params.b)*params.s }
    Quadrilateral params -> Quadrilateral { params | b <- l, s1 <- (l/params.b)*params.s1, s2 <- (l/params.b)*params.s2 }

flip : Context Object -> Context Object
flip obj =
  case obj.child of
    Point -> obj
    Line params -> obj
    Circle params -> obj

    Triangle params ->
      { obj | child <- Triangle { b = params.b
                                , s = params.s
                                , theta = -params.theta } }

    Parallelogram params ->
      { obj | child <- Parallelogram { b = params.b
                                     , s = params.s
                                     , theta = -params.theta } }

    Quadrilateral params ->
      { obj | child <- Quadrilateral { b = params.b
                                     , s1 = params.s1
                                     , s2 = params.s2
                                     , theta = -params.theta
                                     , phi = -params.phi } }
