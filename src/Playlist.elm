module Playlist where

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
