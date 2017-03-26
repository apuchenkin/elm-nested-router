module Router.Actions exposing (..)

import Navigation     exposing (Location)
import URL.Route as Route exposing (Route)

{-| `Action` represents function that prforms something with application state, and might contain side efects -}
type alias Action state msg = state -> (state, Cmd msg)

{-| A state of router -}
type Msg route msg = AppMsg msg | Transition Location | Forward (Route route) | Redirect (Route route)

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
