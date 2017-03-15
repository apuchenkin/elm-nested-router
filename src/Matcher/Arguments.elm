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

getName : Constraint -> String
getName constraint = case constraint of
    Int name -> name
    String name -> name
    Enum name _ -> name
    Regex name _ -> name

toString : Arguments -> Constraint -> Result String String
toString arguments constraint =
  let
    name = getName constraint
    argument = Dict.get name arguments
  in case argument of
    Nothing -> Err <| "missed argument: " ++ name
    Just value -> case Combine.parse (getParser constraint <* Combine.end) value of
      Ok _ -> Ok value
      Err _ -> Err <| "wrong argument: " ++ name

getParser : Constraint -> Parser s Arguments
getParser constraint = case constraint of
  Int name -> Dict.singleton name << Basics.toString <$> Combine.Num.int
  String name -> Dict.singleton name <$> stringParser
  Enum name options -> Dict.singleton name <$> (Combine.choice <| List.map Combine.string options)
  Regex name reg -> Dict.singleton name <$> Combine.regex reg
