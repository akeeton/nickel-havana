module Playlist
    ( Playlist
    , Action
    , init
    , view
    )
    where

import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Decode exposing ((:=))
import List
import Song exposing (Song)


type alias Playlist =
    { name: String
    , songs: List Song
    }


type SongDestination
    = Up
    | Down
    | Top
    | Bottom


type Action
    = MoveSong Int SongDestination
    | DoNothing


songDestinationToString : SongDestination -> String
songDestinationToString destination =
    case destination of
        Up ->
            "Up"

        Down ->
            "Down"

        Top ->
            "Top"

        Bottom ->
            "Bottom"


songNToHtml : Signal.Address Action -> Int -> Song -> Html
songNToHtml address n song =
    let
        makeMoveButton : SongDestination -> Html
        makeMoveButton destination =
            button
                [ onClick address <| MoveSong n destination ]
                [ text <| songDestinationToString destination ]
    in
        div
            []
            [ makeMoveButton Up
            , makeMoveButton Down
            , makeMoveButton Top
            , makeMoveButton Bottom
            , Song.view song
            ]


view : Signal.Address Action -> Playlist -> Html
view address playlist =
    let
        headerHtml = h2 [] [ text playlist.name ]
        songsHtmls = List.indexedMap (songNToHtml address) playlist.songs
    in
        div [] (headerHtml :: songsHtmls)


decoder : JD.Decoder Playlist
decoder =
    JD.object2
        (\name songs -> { name = name, songs = songs })
        ("name" := JD.string)
        ("songs" := JD.list Song.decoder)


init : String -> Result String Playlist
init = JD.decodeString decoder
{-
    List.map Youtube ["vR5HJp_xXRs", "RwpjyLUj0XU"]


toHtml : Maybe Source -> Html
toHtml maybeSource =
  case maybeSource of
    Nothing -> iframe [] []

    Just (Youtube videoId) ->
      let
        srcUrl = "https://www.youtube.com/embed/" ++ videoId ++ "?autoplay=1&controls=0&disablekb=1&enablejsapi=1&fs=0&rel=0&iv_load_policy=3"
      in
        iframe
          [ id "ytplayer"
          , type' "text/html"
          , width 720
          , height 405
          , src srcUrl
          , style [("border", "0")]
          ] []
-}

