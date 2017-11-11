module Main exposing (..)

import Html exposing (..)
import Data.Idea exposing (Idea)
import Navigation exposing (Location)
import Route exposing (Route(..))


-- Model --


type alias Model =
    { ideas : List Idea
    , route : Route.Route
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initModel =
            Model [ Idea 1 "foo" "bar" ] Home
    in
        setRoute (Route.fromLocation location) initModel



-- Update --


type Msg
    = NoOp
    | SetRoute (Maybe Route)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just r ->
            ( { model | route = r }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetRoute route ->
            setRoute route model



-- View --


view : Model -> Html Msg
view model =
    div []
        [ text (toString model.route)
        ]



-- Main --


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
