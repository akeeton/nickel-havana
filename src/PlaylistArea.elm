module PlaylistArea
    ( PlaylistArea
    , Action
    , init
    , update
    , view
    )
    where

import Debug -- TODO akeeton: Remove
import Html exposing (..)
import Html.Attributes exposing (id, type', width, height, src, style)
import Html.Events exposing (onClick, on, targetValue)
import Maybe as M

import MyList
import Playlist exposing (Playlist)


type alias PlaylistArea =
    { playlists: List Playlist
    , importTextAreaInput : String
    , importPreviewPlaylist : Result String Playlist
    }


type Action
    = DoNothing
    | PlaylistAction Int Playlist.Action
    | ImportablePlaylistAction Playlist.Action
    | ImportPlaylist Playlist
    | UpdateImportTextArea String


init : List Playlist -> PlaylistArea
init playlists =
    { playlists = playlists
    , importTextAreaInput = "Playlist goes here"
    , importPreviewPlaylist = Err ""
    }


update : Action -> PlaylistArea -> PlaylistArea
update action area =
    case action of
        DoNothing ->
            area

        PlaylistAction n playlistAction ->
            let
                maybePlaylists' =
                    MyList.getAt n area.playlists
                    `M.andThen` \p -> Just (Playlist.update playlistAction p)
                    `M.andThen` \p' -> MyList.replaceAt p' n area.playlists

                playlists' = M.withDefault area.playlists maybePlaylists'
            in
                { area | playlists <- playlists' }

        ImportablePlaylistAction playlistAction ->
            -- TODO akeeton: Remove
            Debug.crash "ImportablePlaylistAction case not implemented in PlaylistArea.update"

        ImportPlaylist playlist ->
            { area | playlists <- playlist :: area.playlists }

        UpdateImportTextArea input ->
            { area |
                importTextAreaInput <- input,
                importPreviewPlaylist <- Playlist.init input }


playlistNToHtml : Signal.Address Action -> Int -> Playlist -> Html
playlistNToHtml address n playlist =
    let
        forwardingAddress = Signal.forwardTo address <| PlaylistAction n
    in
        Playlist.view forwardingAddress playlist


handleImportTextAreaInput : Signal.Address Action -> String -> Signal.Message
handleImportTextAreaInput address text =
    Signal.message address <| UpdateImportTextArea text


view : Signal.Address Action -> PlaylistArea -> Html
view address area =
    let
        textAreaStyle = style
            [ ("width", "400px")
            , ("height", "300px")
            ]

        -- TODO akeeton: Refactor into function
        (importPlaylistButtonAction, importablePlaylistHtml) =
            case area.importPreviewPlaylist of
                Ok importablePlaylist ->
                    let
                        forwardingAddress =
                            Signal.forwardTo address ImportablePlaylistAction
                    in
                        ( ImportPlaylist importablePlaylist
                        , Playlist.view forwardingAddress importablePlaylist
                        )

                Err message ->
                    (DoNothing, text message)

        playlistHtmls = List.indexedMap (playlistNToHtml address) area.playlists
    in
        div
            [ id "playlist-area" ]
            [ div
                [ id "playlist-import" ]
                [ h1
                    []
                    [ text "Playlist Import" ]
                , textarea
                    [ on "input" targetValue <| handleImportTextAreaInput address
                    , textAreaStyle
                    ]
                    []
                , div
                    [ id "playlist-preview" ]
                    [ importablePlaylistHtml ]
                , button
                    [ onClick address importPlaylistButtonAction ]
                    [ text "Import playlist" ]
                ]
                , h1
                    []
                    [ text "Playlists" ]
            , div
                [ id "playlists" ]
                playlistHtmls
            ]
