module URL.Utils exposing (..)

import Dict

import URL.Route as Route exposing (Route, GetConfig)
import URL.Arguments as Arguments exposing (Arguments)
import URL.Segments as Segments

{-| @Private
  Combine route wit a provided params -}
combineArguments : Arguments -> Route route -> Route route
combineArguments params {route, arguments} = Route.route route <| Dict.union params arguments

getArgumentNames : GetConfig route -> route -> List (Arguments.Name)
getArgumentNames getConfig = List.map Arguments.getName
    << Segments.getConstraints << .segment << getConfig

-- Maps a list of route params over list of routes
mapArguments : GetConfig route -> List route -> Arguments -> List (Route route)
mapArguments getConfig routes arguments = flip List.map routes
  <| \route -> Route.route route
  <| Dict.filter (\name _ -> List.member name (getArgumentNames getConfig route)) arguments

{-| @Private
  Returns a set of handlers applicable to transtition between "from" and "to" routes.
-}
routeDiff : GetConfig route -> List route -> Maybe (Route route) -> Route route -> List route
routeDiff getConfig routes from to =
  let
    getParent = .parent << getConfig
    traverse = Route.traverse getConfig routes

    fromRoute = Maybe.map .route from
    fromParams = Maybe.withDefault Dict.empty <| Maybe.map .arguments from

    fromPath = Maybe.withDefault [] <| Maybe.map traverse fromRoute
    toPath = traverse to.route

    commons = List.map2 (==)
        (mapArguments getConfig fromPath fromParams)
        (mapArguments getConfig toPath to.arguments)

    idx = Maybe.withDefault (List.length commons) <| Maybe.map Tuple.second
      <| List.head <| List.filter (not << Tuple.first)
      <| List.map2 (,) commons <| List.range 0 (List.length commons)

  in List.drop idx toPath
