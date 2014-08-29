module Types where

type NTouch = { x:Float, y:Float, id:Int, x0:Float, y0:Float, t0:Time }

type DrawState = { lines : [Line]
                 , arcs : [Arc]
                 , points : [Point]

                 , toolStart : Point
                 , toolAngle : Angle
                 , toolLength : Length

                 , displayWidth : Float
                 , displayHeight : Float
                 , mode : Mode

                 , gesture : Gesture
                 , drawing : Drawing }

data Mode = Ruler | Draw | Erase

type Point = (Float, Float)
type Angle = Float
type Length = Float

type Line = (Point, Point)
type Arc = (Point, Length, Angle, Angle)

data Drawing = NotDrawing | DrawingLine Line | DrawingArc (Maybe Angle) Angle Angle Arc

data Gesture = NoTouches

             | Translate { toolP0:Point, touchP0:Point, t:NTouch }

             | TouchEnd { t:NTouch } -- before we've figured out which one
             | AdjustLength { l0:Length, t:NTouch }
             | Rotate { angleOffset0:Angle, t:NTouch }

             | PencilEnd { t:NTouch } -- before we've figured out which one
             | DrawLine { t:NTouch, p1:Point }
             | DrawArc { t:NTouch, c:Point, r:Length }

data Action = Touches [NTouch] | Tap Point | ChangeMode Mode | Resize (Int, Int)

unitWidth = 20
