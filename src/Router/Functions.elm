module Router.Functions exposing (..)

import String
import Navigation       exposing (Location)
import Html             exposing (Html)
import Html.Events      exposing (onWithOptions)
import Html.Attributes  as Attr
import Json.Decode      as Json

import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)

{-| binds forward action to existing HTML attributes. Exposed by `Router` -}
bindForward : RouterConfig route state -> Matcher route state -> Route route -> List (Html.Attribute (Action state)) -> List (Html.Attribute (Action state))
bindForward config matcher route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    action = forward config matcher route
  in
    Attr.href (buildUrl config matcher route)
    :: onWithOptions "click" options (Json.succeed action)
    :: attrs

{-| Decomposes Route to string. Exposed by `Router` -}
buildUrl : RouterConfig route state -> Matcher route state -> Route route -> String
buildUrl routerConfig matcher route =
  let
    (RouterConfig config) = routerConfig
    url = matcher.buildUrl route
    url' = if config.removeTrailingSlash then Matcher.removeTrailingSlash url else url
    url'' = if config.html5 then url' else String.cons Matcher.hash url'
  in url''

{-| Preforms a transition to provided `Route`. Exposed by `Router` -}
forward : RouterConfig route state -> Matcher route state -> Route route -> Action state
forward routerConfig matcher route state =
  let
    -- (RouterConfig config) = routerConfig
    url = buildUrl routerConfig matcher route
    msg = Navigation.newUrl url
  in Response (state, msg)

{-| Redirects to provided `Route`. Exposed by `Router` -}
redirect : RouterConfig route state -> Matcher route state -> Route route -> Action state
redirect routerConfig matcher route state =
  let
    -- (RouterConfig config) = routerConfig
    url = buildUrl routerConfig matcher route
    msg = Navigation.modifyUrl url
  in Response (state, msg)

{-| @Private
  Preforms attempt to match provided url to a route by a given routes configuration
  -}
matchRoute : Matcher route state -> String -> Maybe (Route route)
matchRoute matcher url = matcher.match url
