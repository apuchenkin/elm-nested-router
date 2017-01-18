module Router.Navigation exposing (..)

{-|  @Private

Navigation module

A set of functions that is not testable due to "Navigation" import
see:
  - https://github.com/elm-lang/navigation/issues/7
  - https://github.com/elm-lang/dom/issues/2
-}

import String
import Navigation       exposing (Location)
import Html             exposing (Html)
import Html.Events      exposing (onWithOptions)
import Html.Attributes  as Attr
import Json.Decode      as Json

import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)

{-| @Private
  Preforms attempt to match provided url to a route by a given routes configuration
  -}
matchRoute : Matcher route state msg -> String -> Maybe (Route route)
matchRoute matcher url = matcher.match url

getPath : RouterConfig route state msg -> Location -> URL
getPath config location =
    let
      (RouterConfig c) = config
      urlPath = if c.html5
        then location.pathname
        else Maybe.withDefault "/" <| Maybe.map Tuple.second <| String.uncons location.hash
    in
      if c.removeTrailingSlash then Matcher.removeTrailingSlash urlPath else urlPath

{-| Decomposes Route to string. Exposed by `Router` -}
buildUrl : RouterConfig route state msg -> Matcher route state msg -> Route route -> String
buildUrl routerConfig matcher route =
  let
    (RouterConfig config) = routerConfig
    url = matcher.buildUrl route
    url_new = if config.removeTrailingSlash then Matcher.removeTrailingSlash url else url
  in if config.html5 then url_new else String.cons Matcher.hash url_new

{-| binds forward action to existing HTML attributes. Exposed by `Router` -}
bindForward : RouterConfig route state msg -> Matcher route state msg -> Route route -> List (Html.Attribute (Msg route msg)) -> List (Html.Attribute (Msg route msg))
bindForward config matcher route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    -- action = forward config matcher route
  in
    Attr.href (buildUrl config matcher route)
    :: onWithOptions "click" options (Json.succeed <| Forward route)
    :: attrs

{-| Preforms a transition to provided `Route`. Exposed by `Router` -}
forward : RouterConfig route state msg -> Matcher route state msg -> Route route -> Cmd (Msg route msg)
forward routerConfig matcher route = Navigation.newUrl <| buildUrl routerConfig matcher route

{-| Redirects to provided `Route`. Exposed by `Router` -}
redirect : RouterConfig route state msg -> Matcher route state msg -> Route route -> Cmd (Msg route msg)
redirect routerConfig matcher route = Navigation.modifyUrl <| buildUrl routerConfig matcher route
