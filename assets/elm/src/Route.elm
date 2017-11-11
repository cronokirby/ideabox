module Route exposing (Route(..), modifyUrl, fromLocation)

import Navigation exposing (Location)
import UrlParser as Url exposing (Parser, oneOf, parseHash, s)


-- Routing --


type Route
    = Home
    | NewIdea


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map NewIdea (s "new")
        ]


{-| Convert a route to a valid URL string
-}
routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    [ "login" ]

                NewIdea ->
                    [ "new" ]
    in
        "#/" ++ String.join ("/") pieces



-- Public Helpers --


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
