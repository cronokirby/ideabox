module Page.Errored exposing (..)

{-| The page that renders whenever an error loading another page happens.
-}

import Html exposing (Html, div, h1, p, text)


-- Model --


type alias PageLoadError =
    { errorMessage : String }


pageLoadError : String -> PageLoadError
pageLoadError =
    PageLoadError


view : PageLoadError -> Html msg
view model =
    div []
        [ h1 [] [ text "Woops, error loading the page:" ]
        , p [] [ text model.errorMessage ]
        ]
