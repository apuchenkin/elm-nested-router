module App.Routes exposing (..)

type Route = Home | NotFound | Static String | Category | Post
--
-- homeR : Rute Int
-- homeR a = Nothing <$> "home" </> int


routes : List Route
routes = [
    NotFound
  , Static "about"
  , Static "contacts"
  , Home
  , Category
  , Post
  ]
