module Data.Idea exposing (Idea, decoder, decodeList)

import Json.Decode as Decode exposing (Decoder, list)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type alias Idea =
    { id : Int
    , title : String
    , body : String
    }


decoder : Decoder Idea
decoder =
    decode Idea
        |> required "id" Decode.int
        |> required "title" Decode.string
        |> required "body" Decode.string


decodeList : Decoder (List Idea)
decodeList =
    list decoder
