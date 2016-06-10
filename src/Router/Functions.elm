module Router.Functions exposing (..)

import Dict
import Html             exposing (Html)

import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)
import Router.Helpers      exposing (..)

{-| @Private
  Runs the action for the specified state and initial effects
 -}
runAction : Action state -> (state, ActionEffects state) -> (state, ActionEffects state)
runAction action (state, effects) =
    let
      (Response (state', effects')) = action state
    in
      (state', Cmd.batch [effects, effects'])

{-| @Private
  Folds actions for a handlers into a single action
-}
combineActions : List (Action state) -> Action state
combineActions actions = \state -> Response <| List.foldl runAction (noFx state) actions

{-| @Private
  Renders handlers for current route
 -}
render : Router flags route (WithRouter route state) -> (route -> List (Handler (WithRouter route state))) -> WithRouter route state -> Html (Action (WithRouter route state))
render router getHandlers state =
    let
      (RouterConfig config) = router.config
      route       = state.router.route
      handlers    = Maybe.withDefault [] <| Maybe.map getHandlers route
      views       = List.map .view handlers
      htmlParts   = List.foldr (\view parsed -> Dict.union parsed <| view state parsed) Dict.empty views
    in config.layout router state htmlParts

{-| @Private
  Performs attempt to match provided url, returns fallback action on fail
  -}
setUrl : Dependencies flags route (WithRouter route state) -> String -> Action (WithRouter route state)
setUrl deps url =
  let
    (RouterConfig config) = deps.router.config
  in case deps.router.match url of
    Nothing               -> config.fallbackAction deps.router
    Just route            -> setRoute deps route

{-| @Private
  Sets provided route ro the state and return state transition from previous route to new one
-}
setRoute : Dependencies flags route (WithRouter route state) -> Route route -> Action (WithRouter route state)
setRoute deps route state =
  let
    rs = state.router
    (toRoute, toParams) = route
    from  = Maybe.map (\r -> (r, rs.params)) rs.route
    state' = { state | router = { rs | route = Just toRoute, params = toParams }}
  in
    transition deps from route state'

{-| @Private
  A composite transition action between "from" and "to" routes
  Resulting action is composed from handlers, applicable for transistion
-}
transition : Dependencies flags route (WithRouter route state) -> Transition route (WithRouter route state)
transition deps from to state =
  let
    (RouterConfig config) = deps.router.config
    diff = Matcher.routeDiff deps.matcher from to
    handlers = List.map deps.getHandlers diff
    actions  =
      (config.onTransition deps.router from to)
      :: List.map (combineActions << .actions) handlers

  in Response <| List.foldl runAction (noFx state) actions

-- An ulitity record. Required mostly for caching of common fucntions output
type alias Dependencies flags route state = {
    router: Router flags route state
  , matcher: Matcher flags route state
  , getHandlers: (route -> Handler state)
  }

dependencies : Router flags route state -> Matcher flags route state -> Dependencies flags route state
dependencies router matcher =
  let
    getHandler = memoFallback (\sid -> ((\h -> h router) << .handler << matcher.getConfig) (matcher.stringToRoute sid)) matcher.sids
    getHandler' = getHandler << toString
  in {
    router = router,
    matcher = matcher,
    getHandlers = getHandler'
  }
