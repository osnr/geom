module ProblemParser where

import Either (Either, Left, Right, either)
import Dict
import List
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

parseAst : String -> Either String Ast
parseAst = parseString ast

ast : Parser Char Ast
ast = spacesOpt *> astShape `separatedByOpt` spaces <* spacesOpt <* end

astShape : Parser Char AstShape
astShape = AstShape <$> astShapeType <*> (spacesOpt *> astParams)

astShapeType : Parser Char AstShapeType
astShapeType = (AstPoint <$ string "Point")
               <|> (AstLine <$ string "Line")
               <|> (AstCircle <$ string "Circle")
               <|> (AstTriangle <$ string "Triangle")
               <|> (AstParallelogram <$ string "Parallelogram")
               <|> (AstQuadrilateral <$ string "Quadrilateral")

astParams : Parser Char [(AstIdentifier, AstValue)]
astParams = braced (spacesOpt *>
                    astParam `separatedByOpt` (spacesOpt *>
                                               symbol ','
                                               <* spacesOpt)
                    <* spacesOpt)

astParam : Parser Char (AstIdentifier, AstValue)
astParam = (,) <$> identifier
               <*> (spacesOpt *>
                    symbol '=' *>
                    spacesOpt *>
                    astValue)

astValue : Parser Char AstValue
astValue = (AstNumber <$> number) <|> (AstCoords <$> coords) <|> (AstBoolean <$> bool)

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

toProblem : Ast -> Either String Problem
toProblem shps = mapM toShape shps

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


parseProblem : String -> Either String Problem
parseProblem s = parseAst s >>= toProblem

{- utility functions -}
isSpace : Char -> Bool
isSpace c = c == ' ' || c == '\n'

spacesOpt : Parser Char [Char]
spacesOpt = many <| satisfy isSpace

spaces : Parser Char [Char]
spaces = some <| satisfy isSpace

string : String -> Parser Char String
string s = String.fromList <$> token (String.toList s)

number : Parser Char Float
number = (toFloat <$> integer) <|> float

isIdentifier : Char -> Bool
isIdentifier c = Char.isUpper c || Char.isLower c || Char.isDigit c || c == '_'

identifier : Parser Char String
identifier = String.fromList <$> some (satisfy isIdentifier)

coords : Parser Char (Float, Float)
coords = parenthesized <|
         (,) <$> (spacesOpt *> number)
             <*> (spacesOpt *> symbol ',' *> spacesOpt *>
                  number
                  <* spacesOpt)

bool : Parser Char Bool
bool = (True <$ string "true") <|> (False <$ string "false")

-- Zero or more, rather than one or more
separatedByOpt : Parser a r -> Parser a s -> Parser a [r]
separatedByOpt p s = optional (seperatedBy p s) []

(>>=) : Either a b -> (b -> Either a c) -> Either a c
x >>= f = case x of
            Left l  -> Left l
            Right r -> f r

infixl 1 >>=

sequence : [Either a b] -> Either a [b]
sequence ms = let k m m' = m >>= \x ->
                           m' >>= \xs ->
                           Right (x::xs)
              in foldr k (Right []) ms

mapM : (b -> Either a c) -> [b] -> Either a [c]
mapM f xs = sequence (List.map f xs)
