module Requests.Idea exposing (..)

import Http
import Json.Decode as Decode
import Data.Idea as Idea exposing (Idea)


list : Http.Request (List Idea)
list =
    Http.get "/api/ideas" Idea.decodeList
