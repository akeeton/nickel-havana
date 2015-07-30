module Song
    ( Song
    , Action
    , view
    , decoder
    )
    where

import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Decode exposing ((:=))


type alias Song =
    { title : String
    , url : String
    , startTime : String
    , endTime : String
    }


type Action
    = MoveUp
    | MoveDown
    | DoNothing


view : Signal.Address Action -> Song -> Html
view address song =
    div []
        [ button [ onClick address MoveUp ] [ text "Up" ]
        , button [ onClick address MoveDown ] [ text "Down" ]
        , h3 [] [ text song.title ]
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

