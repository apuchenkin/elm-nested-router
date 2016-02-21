module Router.Helpers where

{-| A set of utility functions
@docs singleton, noFx, combineParams, chainAction
-}

import Effects
import Dict
import Router.Types exposing (ActionEffects, Response (..), Action, RouteParams, Route)

{-| Wraps something in a list -}
singleton : a -> List a
singleton action = [ action ]

{-| An action without effects -}
noFx : state -> (state, ActionEffects state)
noFx state = (state, Effects.none)

{-| Combine route wit a provided params -}
combineParams : RouteParams -> Route route -> Route route
combineParams dict (route, params) = (route, Dict.union params dict)

{-| Combines two action together -}
chainAction : Action state -> Action state -> Action state
chainAction action1 action2 state =
  let
    (Response (state', effects)) = action1 state
    (Response (state'', effects')) = action2 state'
  in Response (state'', Effects.batch [effects, effects'])
