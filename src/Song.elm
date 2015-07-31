module Song
    ( Song
    , view
    , decoder
    )
    where

import Html exposing (..)
import Json.Decode as JD
import Json.Decode exposing ((:=))


type alias Song =
    { title : String
    , url : String
    , startTime : String
    , endTime : String
    }


view : Song -> Html
view song =
    div
        []
        [ h3 [] [ text song.title ]
        , p [] [ text song.url ]
        , p [] [ text song.startTime ]
        , p [] [ text song.endTime ]
        ]


decoder : JD.Decoder Song
decoder =
    JD.object4
        Song
        ("title" := JD.string)
        ("url" := JD.string)
        ("startTime" := JD.string)
        ("endTime" := JD.string)

