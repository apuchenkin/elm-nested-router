module Matcher.Utils exposing (..)

import Dict
import Matcher.Matcher as Matcher exposing (Route, GetConfig)
import Matcher.Arguments as Arguments exposing (Arguments)
import Matcher.Segments as Segments

{-| @Private
  Combine route wit a provided params -}
combineArguments : Arguments -> Route route -> Route route
combineArguments params {route, arguments} = Matcher.route route <| Dict.union params arguments

-- Maps a list of route params over list of routes
mapArguments : GetConfig route -> List route -> Arguments -> List (Route route)
mapArguments getConfig routes arguments = flip List.map routes
  <| \route -> Matcher.route route
  <| Dict.filter (\name _ -> List.member name (getArgumentNames getConfig route)) arguments

getArgumentNames : GetConfig route -> route -> List (Arguments.Name)
getArgumentNames getConfig = List.map Arguments.getName
    << Segments.getConstraints << .segment << getConfig

traverse : GetConfig route -> List route -> route -> List route
traverse getConfig routes route = Matcher.parents getConfig routes route ++ [route]

{-| @Private
  Returns a set of handlers applicable to transtition between "from" and "to" routes.
-}
routeDiff : GetConfig route -> List route -> Maybe (Route route) -> Route route -> List route
routeDiff getConfig routes from to =
  let
    getParent = .parent << getConfig

    fromRoute = Maybe.map .route from
    fromParams = Maybe.withDefault Dict.empty <| Maybe.map .arguments from

    fromPath = Maybe.withDefault [] <| Maybe.map (traverse getConfig routes) fromRoute
    toPath = (traverse getConfig routes) to.route

    commons = List.map2 (==)
        (mapArguments getConfig fromPath fromParams)
        (mapArguments getConfig toPath to.arguments)

    idx = Maybe.withDefault (List.length commons) <| Maybe.map Tuple.second
      <| List.head <| List.filter (not << Tuple.first)
      <| List.map2 (,) commons <| List.range 0 (List.length commons)

  in List.drop idx toPath
