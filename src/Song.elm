module Song
    ( Song
    , songToHtml
    , songDecoder
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


songToHtml : Song -> Html
songToHtml entry =
    div []
        [ h3 [] [ text entry.title ]
        , p [] [ text entry.url ]
        , p [] [ text entry.startTime ]
        , p [] [ text entry.endTime ]
        ]


songDecoder : JD.Decoder Song
songDecoder =
    JD.object4
        Song
        ("title" := JD.string)
        ("url" := JD.string)
        ("startTime" := JD.string)
        ("endTime" := JD.string)


