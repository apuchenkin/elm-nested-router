module Router.Helpers exposing (
  noFx,
  doNothing,
  chainAction,
  combineActions
  )

{-| A set of utility functions
@docs noFx, doNothing, chainAction, combineActions
-}

import Router.Types exposing (ActionEffects, Response (Response), Action)

{-| An action without effects -}
noFx : state -> (state, ActionEffects state)
noFx state = (state, Cmd.none)

{-| An empty action -}
doNothing : Action state
doNothing state = Response <| noFx state

{-| Combines two action together -}
chainAction : Action state -> Action state -> Action state
chainAction action1 action2 state =
  let
    (Response (state', effects)) = action1 state
    (Response (state'', effects')) = action2 state'
  in Response (state'', Cmd.batch [effects, effects'])

{-| @Private
  Runs the action for the specified state and initial effects
 -}
runAction : Action state -> Response state -> Response state -- (state, ActionEffects state)
runAction action response =
    let
      (Response (state, effects)) = response
      (Response (state', effects')) = action state
    in
      Response (state', Cmd.batch [effects, effects'])

{-| @Private
  Folds actions for a handlers into a single action
-}
combineActions : List (Action state) -> Action state
combineActions actions state = List.foldl runAction (Response <| noFx state) actions
