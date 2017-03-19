module Tests.Mock.Routes exposing (..)

type Route = Home | Category String | Post | Article String

routes : List Route
routes = [
    Home
  , Category "bear"
  , Category "tiger"
  , Category "animal"
  , Post
  , Article "animal"
  ]
