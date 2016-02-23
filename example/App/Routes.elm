module App.Routes where

import MultiwayTree exposing (Tree (..), Forest)

type Route = Home | NotFound | Static String | Category | Post

routes : Forest Route
routes = [
    Tree NotFound [],
    Tree (Static "about") [],
    Tree (Static "contacts") [],
    Tree Home [
      Tree Category [
        Tree Post []
      ]
    ]
  ]
