module App.Actions exposing (..)

import Http
import Dict exposing (Dict)
import Task exposing (Task)

import Json.Decode  as Json exposing (field)
import Router.Types exposing (WithRouter, Router)
import Router.Helpers exposing (noFx)
import Router.Types as Router

import App.Routes as Routes exposing (Route)

type alias State = WithRouter Route {
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

type Msg = LoadCategories | LoadPosts | LoadPost | UpdateCategories (List Category) | UpdatePosts (List Post) | UpdatePost (Maybe Post)

getCategory : State -> Maybe String
getCategory state =
  let
    param = case Dict.get "subcategory" state.router.arguments of
      Nothing -> Dict.get "category"    state.router.arguments
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

loadCategories : State -> (State, Cmd (Router.Msg Route Msg))
loadCategories state =
  let
    fetch = Task.onError (\_ -> Task.succeed []) <| Http.toTask <| Http.get "data/categories.json" decodeCategories
  in (state, Task.perform (Router.AppMsg << UpdateCategories) fetch)

loadPosts : State -> (State, Cmd (Router.Msg Route Msg))
loadPosts state =
  let
    fetchTask = flip Maybe.map (getCategory state)
      <| \category -> Task.onError (\_ -> Task.succeed []) <| Http.toTask <| Http.get ("data/category/" ++ category ++ ".json") decodePosts

  in (state, Maybe.withDefault Cmd.none <| Maybe.map (Task.perform (Router.AppMsg << UpdatePosts)) fetchTask)

loadPost :  State -> (State, Cmd (Router.Msg Route Msg))
loadPost state =
  let
    postId = Dict.get "postId" state.router.arguments
    task = flip Maybe.map postId <| \pid ->
      Task.onError (\_ -> Task.succeed Nothing) <| Http.toTask <| Http.get ("data/post/" ++ pid ++ ".json") (Json.maybe <| decodePost)

  in (state, Maybe.withDefault Cmd.none <| Maybe.map (Task.perform (Router.AppMsg << UpdatePost)) task)

updatePosts : List Post -> State -> (State, Cmd (Router.Msg Route Msg))
updatePosts posts state = noFx {state | posts = posts}

updatePost : Maybe Post -> State -> (State, Cmd (Router.Msg Route Msg))
updatePost post state = noFx {state | post = post}

updateCategories : List Category -> State -> (State, Cmd (Router.Msg Route Msg))
updateCategories categories state = noFx {state | categories = categories}
