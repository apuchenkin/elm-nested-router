module Router.Helpers
    exposing
        ( noFx
        , doNothing
        , performTask
        , chainAction
        , combineActions
        )

{-| A set of utility functions
@docs noFx, doNothing, performTask, chainAction, combineActions
-}

import Task exposing (Task)
import Router.Types exposing (Commands, Response(Response), Action)


{-| An action without side effects
-}
noFx : state -> ( state, Commands state )
noFx state =
    ( state, Cmd.none )


{-| An empty action
-}
doNothing : Action state
doNothing state =
    Response <| noFx state


{-| Creates a commnd to perform the task
-}
performTask : Task Never (Action state) -> Cmd (Action state)
performTask task =
    Task.perform (always doNothing) identity task


{-| Combines two action together
-}
chainAction : Action state -> Action state -> Action state
chainAction action1 action2 state =
    let
        (Response ( state_, cmd )) =
            action1 state

        (Response ( state__, cmd_ )) =
            action2 state_
    in
        Response ( state__, Cmd.batch [ cmd_, cmd ] )


{-| @Private
  Folds actions for a handlers into a single action
-}
combineActions : List (Action state) -> Action state
combineActions actions =
    List.foldr chainAction doNothing actions
