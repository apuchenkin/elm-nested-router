module Matcher.Segments exposing (
    Segment,
    end, static, int, string, enum, regex, maybe, combine, (</>),
    getConstraints, toString, parse
  )

{-| URL segments

# Types
@docs Segment

# constructors
@docs end, static, int, string, enum, regex

#combinators
@docs maybe, combine, (</>),

# Functions
@docs getConstraints, toString, parse
-}

import Matcher.Arguments as Arguments exposing (Name, Arguments)

import Dict
import Combine exposing (Parser, (<$>), (*>), (<*), (<*>), (<|>), (<$))
import Combine.Char

type Segment = Terminator | Static String | Argument Arguments.Constraint | Sequence Segment Segment | Optional Segment

end : Segment
end = Terminator

static : String -> Segment
static = Static

int : Name -> Segment
int = Argument << Arguments.int

string : Name -> Segment
string = Argument << Arguments.string

enum : Name -> List String -> Segment
enum name options = Argument <| Arguments.enum name options

regex : Name -> String -> Segment
regex name reg = Argument <| Arguments.regex name reg

maybe : Segment -> Segment
maybe segment = Optional segment

combine : Segment -> Segment -> Segment
combine s1 s2 = case s1 of
  Terminator -> s2
  Sequence s Terminator -> Sequence s s2
  _ -> Sequence s1 s2

(</>) : Segment -> Segment -> Segment
(</>) = combine
infixl 8 </>

joinStrings : List String -> String
joinStrings strings = String.concat
  <| List.intersperse "/"
  <| List.filter (not << String.isEmpty) strings

getConstraints : Segment -> List Arguments.Constraint
getConstraints segment = case segment of
  Terminator -> []
  Static string -> []
  Argument constraint -> [constraint]
  Optional segment -> getConstraints segment
  Sequence s1 s2 -> getConstraints s1 ++ getConstraints s2

toString : Arguments -> Segment -> Result String String
toString arguments segment =
  case segment of
  Terminator -> Ok ""
  Static string -> Ok string
  Argument constraint -> Arguments.toString arguments constraint
  Optional segment -> Ok <| Result.withDefault "" <| toString arguments segment
  Sequence s1 s2 -> Result.map joinStrings
    <| Result.mapError (String.concat << List.intersperse "/")
    <| combineResults
    <| List.map (toString arguments) [s1, s2]

slashParser : Parser s ()
slashParser = Combine.skip <| Combine.Char.char Arguments.slash

terminatorParser : Parser s ()
terminatorParser = slashParser <|> Combine.end

combineResults : List (Result error value) -> Result (List error) (List value)
combineResults =
  let
    step result acc = case result of
      Err err -> Err [err]
      Ok value -> Result.map ((::) value) acc
  in List.foldr step (Ok [])

getParser : Segment -> List (Parser error Arguments)
getParser segment = case segment of
  Terminator -> [Dict.empty <$ terminatorParser]
  Static string -> [Dict.empty <$ Combine.string string]
  Argument constraint -> [Arguments.getParser constraint]
  Optional segment -> Combine.succeed Dict.empty :: getParser segment
  Sequence s1 s2 -> let starters = case s2 of
      Optional s -> Combine.succeed Dict.empty :: List.map ((*>) terminatorParser) (getParser s2)
      _ -> List.map ((*>) terminatorParser) (getParser s2)
    in List.concat <| List.map (\parser -> List.map ((<*>) (Dict.union <$> parser)) starters) (getParser s1)

parse : String -> Segment -> Result (Combine.ParseErr ()) (Combine.ParseOk () Arguments)
parse = flip <| Combine.parse << Combine.choice << getParser
