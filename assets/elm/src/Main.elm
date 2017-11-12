module Main exposing (..)

import Html exposing (..)
import Data.Idea exposing (Idea)
import Navigation exposing (Location)
import Route exposing (Route(..))
import Task
import Page.Errored as Errored exposing (PageLoadError)
import Page.Home as Home


-- Model --


type alias Model =
    { ideas : List Idea
    , pageState : PageState
    }


type Page
    = Blank
    | NotFound
    | Home Home.Model
    | Errored PageLoadError


type PageState
    = Loaded Page
    | TransitioningFrom Page


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded p ->
            p

        TransitioningFrom p ->
            p


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initModel =
            Model [ Idea 1 "foo" "bar" ] (Loaded Blank)
    in
        setRoute (Route.fromLocation location) initModel



-- Update --


type Msg
    = NoOp
    | SetRoute (Maybe Route)
    | HomeLoaded (Result PageLoadError Home.Model)
    | HomeMsg Home.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            ( { model | pageState = TransitioningFrom (getPage model.pageState) }, Task.attempt toMsg task )
    in
        case maybeRoute of
            Nothing ->
                ( { model | pageState = Loaded NotFound }, Cmd.none )

            Just Route.Home ->
                transition HomeLoaded Home.init

            Just Route.NewIdea ->
                ( { model | pageState = Loaded NotFound }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, getPage model.pageState ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( SetRoute route, _ ) ->
            setRoute route model

        ( HomeLoaded (Ok subModel), _ ) ->
            ( { model | pageState = Loaded (Home subModel) }, Cmd.none )

        ( HomeLoaded (Err error), _ ) ->
            ( { model | pageState = Loaded (Errored error) }, Cmd.none )

        ( HomeMsg subMsg, Home subModel ) ->
            let
                ( pageModel, cmd ) =
                    Home.update subMsg subModel
            in
                ( { model | pageState = Loaded (Home pageModel) }, Cmd.map HomeMsg cmd )

        ( _, NotFound ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



-- View --


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage False page

        TransitioningFrom page ->
            viewPage True page


viewPage : Bool -> Page -> Html Msg
viewPage _ page =
    case page of
        NotFound ->
            Html.text "Not Found"
                |> frameNoMsg

        Blank ->
            Html.text "Blank"
                |> frameNoMsg

        Errored why ->
            Errored.view why
                |> frameNoMsg

        Home subModel ->
            Home.view subModel
                |> frame HomeMsg


frame : (msg -> Msg) -> Html msg -> Html Msg
frame wrapper subView =
    div []
        [ Html.map wrapper subView
        ]


frameNoMsg : Html msg -> Html Msg
frameNoMsg =
    frame (\_ -> NoOp)



-- Main --


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
