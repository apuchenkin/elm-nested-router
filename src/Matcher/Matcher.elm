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

type alias RouteConfig route state msg = {
    route: route
  , segment: Segments.Segment
  , parent: Maybe route
  , render: state -> View msg -> View msg
  }

type alias Sitemap route = List route

matchOne : (route -> RouteConfig route state msg) -> route -> URL -> Maybe (Route route)
matchOne getConfig route url =
  let
    conf = getConfig route
    result = Segments.parse url conf.segment
  in case result of
    Err _ -> Nothing
    Ok _ -> Just {
      route = route
    , arguments = Dict.empty

    }

match : (route -> RouteConfig route state msg) -> Sitemap route -> URL -> Maybe (Route route)
match config sitemap url = Nothing

buildURL : Route route -> URL
buildURL route = ""
