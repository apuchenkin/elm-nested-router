module Router.Helpers where

import Effects
import Dict
import Router.Types exposing (ActionEffects, Response (..), Action, RouteParams, Route)

singleton : a -> List a
singleton action = [ action ]

noFx : state -> (state, ActionEffects state)
noFx state = (state, Effects.none)

combineParams : RouteParams -> Route route -> Route route
combineParams dict (route, params) = (route, Dict.union params dict)

chainAction : Action state -> Action state -> Action state
chainAction action1 action2 state =
  let
    (Response (state', effects)) = action1 state
    (Response (state'', effects')) = action2 state'
  in Response (state'', Effects.batch [effects, effects'])
