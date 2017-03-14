module Matcher.Segments exposing (..)

import Matcher.Arguments as Arguments exposing (Name, Arguments)

import Dict
import Combine exposing (Parser, (<$>), (*>), (<*), (<*>), (<|>))
import Combine.Char

type Segment = Terminator | Static String | Argument Arguments.Constraint | Sequence (List Segment)

-- TODO: Add Optional?
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

combine : Segment -> Segment -> Segment
combine s1 s2 = Sequence [s1, s2]

(</>) : Segment -> Segment -> Segment
(</>) = combine
infixl 8 </>

toString : Arguments -> Segment -> String
toString arguments segment = case segment of
  Terminator -> ""
  Static string -> string
  Argument constraint -> Arguments.constraintToString arguments constraint
  Sequence list -> List.foldl (++) "" (List.map (toString arguments) list)

slashParser : Parser s ()
slashParser = Combine.skip <| Combine.Char.char Arguments.slash

terminatorParser : Parser s ()
terminatorParser = slashParser <|> Combine.end

getParser : Segment -> Parser s Arguments
getParser segment = case segment of
  Terminator -> always Dict.empty <$> terminatorParser
  Static string -> always Dict.empty <$> Combine.string string
  Argument constraint -> Arguments.getParser constraint
  Sequence [] -> Combine.fail "empty sequence"
  Sequence (head::tail) -> List.foldl (\parser2 parser ->
      Dict.union <$> parser <* terminatorParser <*> parser2
    )
    (getParser head)
    (List.map getParser tail)

parse : Segment -> String -> Result (Combine.ParseErr ()) (Combine.ParseOk () Arguments)
parse segment = Combine.parse
  <| getParser segment <* Combine.optional () slashParser
