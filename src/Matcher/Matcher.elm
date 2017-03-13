module Matcher.Matcher exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
-- import Msg exposing (Msg (..))

import Matcher.Arguments as Arguments
import Matcher.Segments as Segments

type alias URL = String

type alias View msg = Dict String (Html msg)

{-| combined abstract route type with params -}
type alias Route route = {
    route: route
  , arguments: Arguments.Arguments
  -- , query: Arguments.Arguments
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
    result = Segments.parse url config.segment
    -- _ = Debug.crash <| toString <| result
  in case result of
    Err _ -> Nothing
    Ok (_, stream, arguments) ->
      let
        childMatch = matchChilds getConfig routes (Just route) stream.input
        -- _ = Debug.crash <| toString <| childMatch
        _ = Debug.log <| toString childMatch
      in case childMatch of
        Just match -> childMatch
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

buildURL : GetConfig route -> Route route -> URL
buildURL getConfig route = ""
-- buildURL getConfig (route, arguments) = (.segment << getConfig) route
