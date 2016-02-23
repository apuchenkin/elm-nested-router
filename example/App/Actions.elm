module App.Actions where

import Http
import Dict exposing (Dict)
import Task exposing (Task)
import Effects exposing (Never)

import Json.Decode  as Json exposing ((:=))
import Router.Types exposing (WithRouter, Action, Response (..), Router (..))
import Router.Helpers exposing (noFx, chainAction)

import App.Routes as Routes exposing (Route)

(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) = Maybe.andThen

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
decodeCategories = Json.list <| Json.object2 Category
  ("id"     := Json.string)
  ("title"  := Json.string)

decodePost : Json.Decoder Post
decodePost = Json.object3 Post
  ("id"     := Json.int)
  ("title"  := Json.string)
  (Json.maybe ("text"   := Json.string))

decodePosts : Json.Decoder (List Post)
decodePosts = Json.list decodePost

loadCategories : Router Route State -> Action State
loadCategories router state =
  let
    fetch = Task.toMaybe <| Http.get decodeCategories "/data/categories.json"
    task = fetch `Task.andThen` \mcategories ->
      let
        categories = Maybe.withDefault [] mcategories
        update = updateCategories categories
        categoryParam = getCategory state
        action = Maybe.withDefault update <| flip Maybe.map categoryParam <| \category ->
          update `chainAction` (loadPosts router)
      in Task.succeed <| action

  in Response (state, Effects.task task)

loadPosts : Router Route State -> Action State
loadPosts (Router router) state =
  let
    category = getCategory state
    fetch = flip Maybe.map category <| \c ->
      let fetch = Task.toMaybe <| Http.get decodePosts ("/data/category/" ++ c ++ ".json")
      in fetch `Task.andThen` \posts -> Task.succeed <| updatePosts <| Maybe.withDefault [] posts

  in Response (state, Maybe.withDefault Effects.none <| Maybe.map Effects.task fetch)

loadPost : Action State
loadPost state =
  let
    postId = Dict.get "postId" state.router.params
    task = flip Maybe.map postId <| \pid ->
      let fetch = Task.toMaybe <| Http.get decodePost ("/data/post/" ++ pid ++ ".json")
      in fetch `Task.andThen` \post -> Task.succeed <| updatePost post

  in Response (state, Maybe.withDefault Effects.none <| Maybe.map Effects.task task)

updatePosts : List Post -> Action State
updatePosts posts state = Response <| noFx {state | posts = posts}

updatePost : Maybe Post -> Action State
updatePost post state = Response <| noFx {state | post = post}

updateCategories : List Category -> Action State
updateCategories categories state = Response <| noFx {state | categories = categories}
