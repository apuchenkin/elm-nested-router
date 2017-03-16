module Matcher.Segments exposing (..)

import Matcher.Arguments as Arguments exposing (Name, Arguments)

import Dict
import Combine exposing (Parser, (<$>), (*>), (<*), (<*>), (<|>))
import Combine.Char

type Segment = Terminator | Static String | Argument Arguments.Constraint | Sequence (List Segment) | Optional Segment

-- TODO: Use `end` as bypass?

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
combine s1 s2 = Sequence [s1, s2]

(</>) : Segment -> Segment -> Segment
(</>) = combine
infixl 8 </>

joinStrings : List String -> String
joinStrings strings = String.concat
  <| List.intersperse "/"
  <| List.filter (not << String.isEmpty) strings

toString : Arguments -> Segment -> Result String String
toString arguments segment =
  case segment of
  Terminator -> Ok ""
  Static string -> Ok string
  Argument constraint -> Arguments.toString arguments constraint
  Optional segment -> Ok <| Result.withDefault "" <| toString arguments segment
  Sequence list -> Result.map joinStrings
    <| Result.mapError (String.concat << List.intersperse "/")
    <| combineResults
    <| List.map (toString arguments) list

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

getParser : Segment -> Parser s Arguments
getParser segment = case segment of
  Terminator -> always Dict.empty <$> terminatorParser
  Static string -> always Dict.empty <$> Combine.string string
  Argument constraint -> Arguments.getParser constraint
  Optional segment -> getParser segment
  Sequence [] -> Combine.fail "empty sequence"
  Sequence (head::tail) -> List.foldl (\parser2 parser ->
      Dict.union <$> parser <* terminatorParser <*> parser2
    )
    (getParser head)
    (List.map getParser tail)

parse : Segment -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () Arguments)
parse segment = Combine.parse
  <| getParser segment <* Combine.optional () slashParser
