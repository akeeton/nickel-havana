module Playlist
    ( PlaylistEntry(..)
    , Playlist(..)
    , init
    ) where

import Json.Decode as JD


type alias PlaylistEntry =
    { title : String
    , url : String
    , startTime : String
    , endTime : String
    }


type alias Playlist =
    [PlaylistEntry]


playlistEntryDecoder : JD.Decoder PlaylistEntry


playlistDecoder : JD.Decoder Playlist
playlistDecoder =
    JD.list playlistEntryDecoder


init : String -> Result String Playlist
init =
