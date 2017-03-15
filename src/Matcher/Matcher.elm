module Matcher.Matcher exposing (..)

import Dict exposing (Dict)

import Matcher.Arguments as Arguments exposing (Arguments)
import Matcher.Segments as Segments exposing ((</>))

type alias URL = String

{-| combined abstract route type with params -}
type alias Route route = {
    route: route
  , arguments: Arguments
  -- , query: Arguments
  }

type alias RouteConfig route = {
    segment: Segments.Segment
  , parent: Maybe route
  }

type alias Sitemap route = List route
type alias GetConfig route = route -> RouteConfig route

isChild : GetConfig route -> Maybe route -> route -> Bool
isChild getConfig parent child = parent == (.parent << getConfig) child

childs : GetConfig route ->  List route -> Maybe route -> List route
childs getConfig routes route = List.filter (isChild getConfig route) routes

matchOne : GetConfig route -> List route -> URL -> route -> Maybe (Route route)
matchOne getConfig routes url route =
  let
    config = getConfig route
    result = Segments.parse config.segment url
  in case result of
    Err _ -> Nothing
    Ok (_, stream, arguments) ->
      let
        childMatch = matchChilds getConfig routes (Just route) stream.input
      in case childMatch of
        Just match -> Just { route = match.route, arguments = Dict.union arguments match.arguments }
        Nothing -> case stream.input of
          "" -> Just {
            route = route
          , arguments = arguments
          }
          _ -> Nothing

matchChilds : GetConfig route -> Sitemap route -> Maybe route -> URL -> Maybe (Route route)
matchChilds getConfig sitemap parent url = List.head
  <| List.filterMap identity
  <| List.map (matchOne getConfig sitemap url)
  <| childs getConfig sitemap parent

match : GetConfig route -> Sitemap route -> URL -> Maybe (Route route)
match getConfig sitemap url = matchChilds getConfig sitemap Nothing url

composeURL : GetConfig route -> Route route -> URL
composeURL getConfig {route, arguments} = let
    config = getConfig route
    url = Maybe.withDefault "/" <| Segments.toString arguments config.segment
  in case config.parent of
    Nothing -> url
    Just parent -> String.concat
      <| List.intersperse "/"
      <| List.filter (not << String.isEmpty)
      <| [composeURL getConfig { route = parent, arguments = arguments }, url]

buildURL : GetConfig route -> Route route -> URL
buildURL getConfig route = String.cons Arguments.slash <| composeURL getConfig route

route : route -> Arguments -> Route route
route route arguments = { route = route, arguments = arguments }
