module Matcher.Arguments exposing (..)

import Dict exposing (Dict)
import Combine exposing (Parser, many1, (<$>), (<*))
import Combine.Char exposing (noneOf)
import Combine.Num

type alias Name = String

{-| Dynamic route parameter name -}
type alias Argument = String

{-| A map of route param names and values -}
type alias Arguments = Dict Name Argument

{-| A constraint of route parameter type -}
type Constraint = Int Name | String Name | Enum Name (List String) | Regex Name String

int : Name -> Constraint
int = Int

string : Name -> Constraint
string = String

enum : Name -> List String -> Constraint
enum = Enum

regex : Name -> String -> Constraint
regex = Regex

slash : Char
slash = '/'

hash : Char
hash = '#'

query : Char
query = '?'

stringParser : Parser s String
stringParser = String.fromList <$> many1 (noneOf [ slash, hash, query ])

constraintToString : Arguments -> Constraint -> String
constraintToString args constraint =
  let argument = case constraint of
    Int name -> Dict.get name args
    String name -> Dict.get name args
    Enum name _ -> Dict.get name args
    Regex name _ -> Dict.get name args
  in case argument of
    Nothing -> Debug.crash "toString:Constraint no arguments"
    Just value -> case Combine.parse (getParser constraint <* Combine.end) value of
      Ok _ -> value
      Err _ -> Debug.crash "toString:Constraint no arguments"

getParser : Constraint -> Parser s Arguments
getParser constraint = case constraint of
  Int name -> Dict.singleton name << toString <$> Combine.Num.int
  String name -> Dict.singleton name <$> stringParser
  Enum name options -> Dict.singleton name <$> (Combine.choice <| List.map Combine.string options)
  Regex name reg -> Dict.singleton name <$> Combine.regex reg
