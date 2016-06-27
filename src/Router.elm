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

import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)
import Router.Functions    exposing (..)
import Router.Helpers      exposing (combineActions)

{-| Initial state for router. Fed this into your application state -}
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  }

runAction : Action state -> state -> (state, ActionEffects state)
runAction action state = let (Response res) = action state in res

getPath : RouterConfig route state -> Location -> URL
getPath config location =
    let
      (RouterConfig c) = config
      urlPath = if c.html5
        then location.pathname
        else Maybe.withDefault "/" <| Maybe.map snd <| String.uncons location.hash
    in
      if c.removeTrailingSlash then Matcher.removeTrailingSlash urlPath else urlPath

{-| @Private
  Renders handlers for current route
 -}
render :
  Router route (WithRouter route state)
  -> (route -> List (Handler (WithRouter route state)))
  -> WithRouter route state -> Html (Action (WithRouter route state))
render router getHandlers state =
    let
      (RouterConfig config) = router.config
      route       = state.router.route
      handlers    = Maybe.withDefault [] <| Maybe.map getHandlers route
      views       = List.map .view handlers
      htmlParts   = List.foldr (\view parsed -> Dict.union parsed <| view state parsed) Dict.empty views
    in config.layout router state htmlParts

{-| @Private
  Sets provided route ro the state and return state transition from previous route to new one
-}
transition :
  Router route (WithRouter route state) ->
  Matcher route (WithRouter route state) ->
  (route -> Handler (WithRouter route state)) ->
  Maybe (Route route) -> Action (WithRouter route state)
transition router matcher getHandlers to state =
  let
    (RouterConfig config) = router.config
    rs = state.router
    toRoute = Maybe.map fst to
    toParams = Maybe.withDefault Dict.empty <| Maybe.map snd to
    from  = Maybe.map (\r -> (r, rs.params)) rs.route
    state' = { state | router = { rs | route = toRoute, params = toParams }}

    diff = Maybe.withDefault [] <| Maybe.map (Matcher.routeDiff matcher from) to
    handlers = List.map getHandlers diff
    onTransition = config.transition router from to
    actions  = List.map (combineActions << .actions) handlers
  in
    combineActions (onTransition :: actions) state'

createHandlers :
    Router route (WithRouter route state) ->
    Matcher route (WithRouter route state) ->
    (route -> Handler (WithRouter route state))
createHandlers router matcher =
    let getHandlers = Matcher.memoFallback (\sid -> ((\h -> h router) << .handler << matcher.getConfig) (matcher.stringToRoute sid)) matcher.sids
    in getHandlers << toString

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

{-| Launches the router -}
dispatch : (flags -> (WithRouter route state)) -> RouterConfig route (WithRouter route state) -> Program flags -- flags
dispatch init config =
  let
    (RouterConfig c) = config
    matcher = Matcher.matcher config
    router = constructor config matcher

    getHandlers = createHandlers router matcher
    render' state = render router (List.map getHandlers << matcher.traverse) state

    update action state = runAction action state
    urlUpdate route state = runAction (transition router matcher getHandlers route) state

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
