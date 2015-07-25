module Playlist
    ( Playlist
    )
    where

import Json.Decode as JD
import Json.Decode exposing ((:=))


type alias PlaylistEntry =
    { title : String
    , url : String
    , startTime : String
    , endTime : String
    }


type alias Playlist =
    List PlaylistEntry


playlistEntryDecoder : JD.Decoder PlaylistEntry
playlistEntryDecoder =
    JD.object4
        PlaylistEntry
        ("title" := JD.string)
        ("url" := JD.string)
        ("startTime" := JD.string)
        ("endTime" := JD.string)


playlistDecoder : JD.Decoder Playlist
playlistDecoder =
    JD.list playlistEntryDecoder


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

