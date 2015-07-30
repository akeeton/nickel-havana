module Playlist
    ( Playlist
    , init
    , view
    )
    where

import Html exposing (..)
import Json.Decode as JD
import Json.Decode exposing ((:=))
import List
import Song exposing (Song)


type alias Playlist =
    { name: String
    , songs: List Song
    }


view : Playlist -> Html
view playlist =
    let
        headerHtml = h2 [] [ text playlist.name ]
        songsHtmls = List.map Song.view playlist.songs
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

