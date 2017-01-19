module Router.Helpers exposing (
  noFx,
  chainAction,
  foldActions
  )

{-| A set of utility functions
@docs noFx, chainAction, foldActions
-}

-- import Task exposing (Task)
import Router.Types exposing (Action)

{-| An action without side effects -}
noFx : Action state msg
noFx state = (state, Cmd.none)

{-| Combines two action together -}
chainAction : Action state msg -> Action state msg -> Action state msg
chainAction action1 action2 state =
  let
    (state1, cmd1) = action1 state
    (state2, cmd2) = action2 state1
  in (state2, Cmd.batch [cmd1, cmd2])

{-| @Private
  Folds actions for a handlers into a single action
-}
foldActions : List (Action state msg) -> Action state msg
foldActions actions = List.foldr chainAction noFx actions
