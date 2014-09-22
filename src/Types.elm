module Types where

import Dict

type Context a = { pos : Point
                 , angle : Angle

                 , manipulable : Bool
                 , scalable : Bool

                 , child : a }

data Object = Point

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

type Problem = [Context Object]
type ProblemDict = Dict.Dict ObjectKey (Context Object)

type NTouch = { x:Float, y:Float, id:Int, x0:Float, y0:Float, t0:Time }

type Tool = { length : Length }

type DrawState = { problem : ProblemDict

                 , lines : [Line]
                 , arcs : [Arc]
                 , points : [Point]

                 , tool : Context Tool

                 , displayWidth : Float
                 , displayHeight : Float
                 , mode : Mode

                 , gesture : Gesture
                 , drawing : Drawing }

data Mode = Ruler | Object | Draw | Erase

type Point = (Float, Float)
type Angle = Float
type Length = Float

type Line = (Point, Point)
type Arc = (Point, Length, Angle, Angle)

data Drawing = NotDrawing | DrawingLine Line | DrawingArc (Maybe Angle) Angle Angle Arc

type ObjectKey = Int

data Gesture = NoTouches

             | TranslateTool { toolP0:Point, touchP0:Point, t:NTouch }
             | TouchEndTool { t:NTouch } -- before we've figured out which one
             | ScaleTool { l0:Length, t:NTouch }
             | RotateTool { angleOffset0:Angle, t:NTouch }

             -- ok is a key to find the object at issue in the Dict of objects
             | TranslateObj { ok:ObjectKey, oP0:Point, touchP0:Point, t:NTouch }
             | TouchEndObj { ok:ObjectKey, t:NTouch }
             | ScaleObj { ok:ObjectKey, l0:Length, t:NTouch }
             | RotateObj { ok:ObjectKey, angleOffset0:Angle, t:NTouch }
             | FlipObj (Maybe ObjectKey)

             | PencilEnd { t:NTouch } -- before we've figured out which one
             | DrawLine { t:NTouch, p1:Point }
             | DrawArc { t:NTouch, c:Point, r:Length }

data Action = ChangeProblem ProblemDict | Touches [NTouch] | Tap Point | ChangeMode Mode | Resize (Int, Int)

unitWidth = 20
