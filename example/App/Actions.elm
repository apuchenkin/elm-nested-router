module App.Actions exposing (..)

import Http
import Dict exposing (Dict)
import Task exposing (Task)

import Json.Decode  as Json exposing (field)
import Router.Types exposing (WithRouter, Action, Response (..), Router)
import Router.Helpers exposing (noFx, chainAction, doNothing, performTask)

import App.Routes as Routes exposing (Route)

type alias State = WithRouter Route
  {
    categories: List Category,
    posts: List Post,
    post: Maybe Post
  }

type alias Category = {
    id: String,
    title: String
  }

type alias Post = {
  id: Int,
  title: String,
  text: Maybe String
}

getCategory : State -> Maybe String
getCategory state =
  let
    param = case Dict.get "subcategory" state.router.params of
      Nothing -> Dict.get "category"    state.router.params
      category -> category
  in param

decodeCategories : Json.Decoder (List Category)
decodeCategories = Json.list <| Json.map2 Category
  (field "id"    Json.string)
  (field "title" Json.string)

decodePost : Json.Decoder Post
decodePost = Json.map3 Post
  (field "id"     Json.int)
  (field "title"  Json.string)
  (Json.maybe <| field "text" Json.string)

decodePosts : Json.Decoder (List Post)
decodePosts = Json.list decodePost

loadCategories : Router Route State -> Action State
loadCategories router state =
  let
    fetch = Task.onError (\_ -> Task.succeed []) <| Http.toTask <| Http.get "data/categories.json" decodeCategories
    task = fetch |> Task.andThen (\categories ->
      let
        -- categories = Maybe.withDefault [] mcategories
        update = updateCategories categories
        categoryParam = getCategory state
        action = Maybe.withDefault update <| flip Maybe.map categoryParam <| \category ->
          update |> chainAction (loadPosts router)
      in Task.succeed <| action)

  in Response (state, performTask task)

loadPosts : Router Route State -> Action State
loadPosts router state =
  let
    category = getCategory state
    fetchTask = flip Maybe.map category <| \c ->
      let fetch = Task.onError (\_ -> Task.succeed []) <| Http.toTask <| Http.get ("data/category/" ++ c ++ ".json") decodePosts
      in fetch |> Task.andThen (\posts -> Task.succeed <| updatePosts posts)

  in Response (state, Maybe.withDefault Cmd.none <| Maybe.map performTask fetchTask)

loadPost : Action State
loadPost state =
  let
    postId = Dict.get "postId" state.router.params
    task = flip Maybe.map postId <| \pid ->
      let fetch = Task.onError (\_ -> Task.succeed Nothing) <| Http.toTask <| Http.get ("data/post/" ++ pid ++ ".json") (Json.maybe <| decodePost)
      in fetch |> Task.andThen (\post -> Task.succeed <| updatePost post)

  in Response (state, Maybe.withDefault Cmd.none <| Maybe.map performTask task)

updatePosts : List Post -> Action State
updatePosts posts state = Response <| noFx {state | posts = posts}

updatePost : Maybe Post -> Action State
updatePost post state = Response <| noFx {state | post = post}

updateCategories : List Category -> Action State
updateCategories categories state = Response <| noFx {state | categories = categories}
