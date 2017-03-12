module Matcher.Arguments exposing (..)

import Dict exposing (Dict)
import Combine exposing (Parser, many1, parse, many, while, between, end, manyTill, (<$>), (*>), (<*), (<*>), (<|>))
import Combine.Char exposing (noneOf)
import Combine.Num

type alias Name = String

{-| A constraint of route parameter type -}
type Constraint = Int Name | String Name | Enum Name (List String) | Regex Name String

{-| Dynamic route parameter name -}
type alias Argument = String

-- type Argument = IntArgument | StringArgument

{-| A map of route param names and values -}
type alias Arguments = Dict Name Argument

int : Name -> Constraint
int = Int

string : Name -> Constraint
string = String

enum : Name -> List String -> Constraint
enum = Enum

regex : Name -> String -> Constraint
regex = Regex

constraintToString : Arguments -> Constraint -> String
constraintToString args constraint =
  let argument = case constraint of
    Int name -> Dict.get name args
    String name -> Dict.get name args
    Enum name _ -> Dict.get name args
    Regex name _ -> Dict.get name args
  in case argument of
    Just value -> value
    Nothing -> Debug.crash "toString:Constraint no arguments"

slash : Char
slash = '/'

stringParser : Parser s String
stringParser = String.fromList <$> many1 (noneOf [ slash, '#', '?' ])

getParser : Constraint -> Parser s String
getParser constraint = case constraint of
  Int name -> toString <$> Combine.Num.int
  String name -> stringParser
  Enum name options -> Combine.choice <| List.map Combine.string options
  Regex name reg -> Combine.regex reg
