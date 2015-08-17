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
    | PlaylistAction Int Playlist.Action


init : List Playlist -> PlaylistArea
init playlists =
    { playlists = playlists }


update : Action -> PlaylistArea -> PlaylistArea
update action area =
    case action of
        DoNothing ->
            area

        PlaylistAction n action' ->
            let
                maybePlaylists' = MyList.getAt n playlists
                `Maybe.andThen` Just <| Playlist.update action'
                `Maybe.andThen` MyList.insertAt n
                `Maybe.andThen` MyList.removeAt <| n + 1

                playlists' = Maybe.withDefault area.playlists maybePlaylists
            in
                { area | playlists <- playlists' }


playlistNToHtml : Signal.Address Action -> Int -> Playlist -> Html
playlistNToHtml address n playlist =
    let
        forwardingAddress = Signal.forwardTo address <| PlaylistAction n
    in
        Playlist.view forwardingAddress playlist


view : Signal.Address Action -> PlaylistArea -> Html
view address area =
    let
        playlistHtmls = List.indexedMap (playlistNToHtml address) area.playlists
    in
        div [] playlistHtmls
