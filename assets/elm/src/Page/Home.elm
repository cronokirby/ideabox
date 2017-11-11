module Page.Home exposing (..)

import Data.Idea as Idea exposing (Idea)
import Requests.Idea as Idea
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)


-- Model --


type alias Model =
    { ideas : List Idea }


init : Task PageLoadError Model
init =
    let
        handleLoadError _ =
            pageLoadError "Failure to load ideas"
    in
        Idea.list
            |> Http.toTask
            |> Task.map Model
            |> Task.mapError handleLoadError
