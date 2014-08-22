module ProblemParser where

import Either (Either (..), either)
import Dict
import String
import Char

import Parser (..)
import Parser.Char (..)

type Ast = [AstShape]

type DebugString = String

data AstShape = AstShape AstShapeType [(AstIdentifier, AstValue)]
data AstShapeType = AstPoint | AstLine | AstCircle | AstTriangle | AstParallelogram | AstQuadrilateral

type AstIdentifier = String
data AstValue = AstNumber Float | AstCoords (Float, Float) | AstBoolean Bool

parseGeom : String -> Either String Ast
parseGeom = parseString ast

ast : Parser Char Ast
ast = spaces *> astShape `seperatedBy` spaces <* spaces

astShape : Parser Char AstShape
astShape = AstShape <$> astShapeType <*> (spaces *> astParams)

astShapeType : Parser Char AstShapeType
astShapeType = (AstPoint <$ string "Point")
               <|> (AstLine <$ string "Line")
               <|> (AstCircle <$ string "Circle")
               <|> (AstTriangle <$ string "Triangle")
               <|> (AstParallelogram <$ string "Parallelogram")
               <|> (AstQuadrilateral <$ string "Quadrilateral")

astParams : Parser Char [(AstIdentifier, AstValue)]
astParams = braced (spaces *>
                    astParam `seperatedBy` (maySpaces *>
                                            symbol ','
                                            <* maySpaces)
                    <* spaces)

astParam : Parser Char (AstIdentifier, AstValue)
astParam = (,) <$> identifier
               <*> (maySpaces *>
                    symbol '=' *>
                    maySpaces *>
                    astValue)

astValue : Parser Char AstValue
astValue = (AstNumber <$> float) <|> (AstCoords <$> coords) <|> (AstBoolean <$> bool)

type Context a = { pos : (Float, Float)
                 , angle : Float

                 , child : a }

data Shape = Point

           | Line { length : Float }

           | Circle { r : Float }

           | Triangle { b : Float
                      , s : Float
                      , theta : Float }

           | Parallelogram { b : Float
                           , s : Float
                           , theta : Float }

           | Quadrilateral { b : Float
                           , s1 : Float
                           , s2 : Float
                           , theta : Float
                           , delta : Float }

type Problem = [Context Shape]

toShape : AstShape -> Either String (Context Shape)
toShape shp =
  case shp of
    AstShape typ params ->
      let ps = Dict.fromList params
          lookupCoords' = lookupCoords ps (show shp)
          lookupFloat' = lookupFloat ps (show shp)

      in lookupCoords' "pos" >>= \pos ->
         lookupFloat' "angle" >>= \angle ->
         Right { pos = pos, angle = angle } >>= \ctx ->
         either Left (Right . child ctx) <|
         case typ of
           AstPoint -> Right Point

           AstLine ->
             lookupFloat' "length" >>= \length ->
             Right <| Line { length = length }

lookupCoords : Dict.Dict AstIdentifier AstValue -> DebugString -> AstIdentifier -> Either String (Float, Float)
lookupCoords d dbg i =
  case Dict.get i d of
    Just (AstCoords val) -> Right val
    Just _               -> Left <| String.join "" ["Need (x, y) for parameter '", i, "' in shape '", dbg, "'."]
    Nothing              -> Left <| String.join "" ["Missing parameter '", i, "' in shape '", dbg, "'."]

lookupFloat : Dict.Dict AstIdentifier AstValue -> DebugString -> AstIdentifier -> Either String Float
lookupFloat d dbg i =
  case Dict.get i d of
    Just (AstNumber val) -> Right val
    Just _               -> Left <| String.join "" ["Need number for parameter '", i, "' in shape '", dbg, "'."]
    Nothing              -> Left <| String.join "" ["Missing parameter '", i, "' in shape '", dbg, "'."]

child : a -> b -> {a | child : b}
child ctx s = { ctx | child = s }

{- utility functions -}
isSpace : Char -> Bool
isSpace c = c == ' ' || c == '\n'

maySpaces : Parser Char [Char]
maySpaces = many <| satisfy isSpace

spaces : Parser Char [Char]
spaces = some <| satisfy isSpace

string : String -> Parser Char String
string s = String.fromList <$> token (String.toList s)

isIdentifier : Char -> Bool
isIdentifier c = Char.isUpper c || Char.isLower c || Char.isDigit c || c == '_'

identifier : Parser Char String
identifier = String.fromList <$> some (satisfy isIdentifier)

coords : Parser Char (Float, Float)
coords = parenthesized <|
         (,) <$> (maySpaces *> float)
             <*> (maySpaces *> symbol ',' *>
                  float
                  <* maySpaces)

bool : Parser Char Bool
bool = (True <$ string "true") <|> (False <$ string "false")

(>>=) : Either a b -> (b -> Either a c) -> Either a c
x >>= f = case x of
            Left l  -> Left l
            Right r -> f r

infixl 1 >>=
