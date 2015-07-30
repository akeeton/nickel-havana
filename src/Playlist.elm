module Playlist
    ( Playlist
    , initWithJson
    , playlistToHtml
    )
    where

import Html exposing (..)
import Json.Decode as JD
import Json.Decode exposing ((:=))
import List


type alias Song =
    { title : String
    , url : String
    , startTime : String
    , endTime : String
    }


type alias Playlist =
    { name: String
    , songs: List Song
    }


songToHtml : Song -> Html
songToHtml entry =
    div []
        [ h3 [] [ text entry.title ]
        , p [] [ text entry.url ]
        , p [] [ text entry.startTime ]
        , p [] [ text entry.endTime ]
        ]


playlistToHtml : Playlist -> Html
playlistToHtml playlist =
    let
        headerHtml = h2 [] [ text playlist.name ]
        songsHtmls = List.map songToHtml playlist.songs
    in
        div [] (headerHtml :: songsHtmls)


songDecoder : JD.Decoder Song
songDecoder =
    JD.object4
        Song
        ("title" := JD.string)
        ("url" := JD.string)
        ("startTime" := JD.string)
        ("endTime" := JD.string)


playlistDecoder : JD.Decoder Playlist
playlistDecoder =
    JD.object2
        (\name songs -> { name = name, songs = songs })
        ("name" := JD.string)
        ("songs" := JD.list songDecoder)


initWithJson : String -> Result String Playlist
initWithJson = JD.decodeString playlistDecoder
{-
type Source = Youtube String
videos =
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

