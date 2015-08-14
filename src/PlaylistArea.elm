module PlaylistArea
    ( PlaylistArea
    , Action
    , init
    , update
    , view
    )
    where

import Html exposing (..)

import Playlist exposing (Playlist)


type alias PlaylistArea =
    { playlists: List Playlist
    }


type Action
    = DoNothing
    | PlaylistAction (Playlist.Action)


init : List Playlist -> PlaylistArea
init playlists =
    { playlists = playlists }


update : Action -> PlaylistArea -> PlaylistArea
update action area =
    case action of
        DoNothing ->
            area


view : Signal.Address Action -> PlaylistArea -> Html
view address area =
    let
        forwardingAddress = Signal.forwardTo address PlaylistAction
        playlistHtmls =
            List.map (Playlist.view forwardingAddress) area.playlists
    in
        div [] playlistHtmls
