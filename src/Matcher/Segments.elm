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

toString : Arguments -> Segment -> Maybe String
toString arguments segment =
  case segment of
  Terminator -> Just ""
  Static string -> Just string
  Argument constraint -> Result.toMaybe <| Arguments.toString arguments constraint
  Optional segment -> Just <| Maybe.withDefault "" <| toString arguments segment
  Sequence list -> Maybe.map joinStrings
    <| combineMaybes <| List.map (toString arguments) list

slashParser : Parser s ()
slashParser = Combine.skip <| Combine.Char.char Arguments.slash

terminatorParser : Parser s ()
terminatorParser = slashParser <|> Combine.end

combineMaybes : List (Maybe a) -> Maybe (List a)
combineMaybes =
  let
    step maybe acc = case maybe of
      Nothing -> Nothing
      Just value -> Maybe.map ((::) value) acc
  in List.foldr step (Just [])

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
