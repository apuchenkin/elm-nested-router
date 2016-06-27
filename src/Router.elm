module Router exposing ( dispatch, initialState )
{-| A simple nested router for single page applications.

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example) ([Live demo](http://apuchenkin.github.io/elm-nested-router/example))
and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details

@docs dispatch, initialState
-}

import Dict
import String
import Navigation       exposing (Location)
import Html             exposing (Html)
import Html.Events      exposing (onWithOptions)
import Html.Attributes  as Attr
import Json.Decode      as Json

import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)
import Router.Functions    exposing (..)

hash : Char
hash = '#'

{-| Initial state for router. Fed this into your application state -}
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  }

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
buildUrl routerConfig matcher (route, params) =
  let
    (RouterConfig config) = routerConfig
    url = matcher.buildUrl (route, params)
    url' = if config.removeTrailingSlash then Matcher.removeTrailingSlash url else url
    url'' = if config.html5 then url' else String.cons hash url'
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

{-| Router constructor -}
constructor : RouterConfig route state -> Matcher route state -> Router route state
constructor config matcher =
  let
    (RouterConfig c) = config
    config' = RouterConfig <| { c | routeConfig = matcher.getConfig}
  in {
    config = config'
  , bindForward = bindForward config' matcher
  , buildUrl = buildUrl config' matcher
  , forward = forward config' matcher
  , redirect = redirect config' matcher
  , match = matchRoute matcher
  }

update : (Action state) -> state -> (state, Cmd (Action state))
update action state = let
    (Response state') = action state
  in state'

getPath : RouterConfig route state -> Location -> URL
getPath config location =
    let
      (RouterConfig c) = config
      urlPath = if c.html5
        then location.pathname
        else Maybe.withDefault "/" <| Maybe.map snd <| String.uncons location.hash
    in
      if c.removeTrailingSlash then Matcher.removeTrailingSlash urlPath else urlPath

{-| Launches the router -}
dispatch : (flags -> (WithRouter route state)) -> RouterConfig route (WithRouter route state) -> Program flags -- flags
dispatch init config =
  let
    (RouterConfig c) = config
    matcher = Matcher.matcher config
    router = constructor config matcher

    getHandlers = createHandlers router matcher
    render' state = render router (List.map getHandlers << matcher.traverse) state

    -- urlUpdate : Maybe route -> (WithRouter route state) -> ((WithRouter route state), Cmd (Action (WithRouter route state)))
    urlUpdate route state = update (transition router matcher getHandlers route) state

    parser = Navigation.makeParser (matcher.match << getPath config)
    init' flags route = urlUpdate route (init flags)
  in
    Navigation.programWithFlags parser
    {
      init = init'
    , update = update
    , urlUpdate = urlUpdate
    , view = render'
    , subscriptions = c.subscriptions
    }
