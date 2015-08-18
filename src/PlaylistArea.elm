module PlaylistArea
    ( PlaylistArea
    , Action
    , init
    , update
    , view
    )
    where

-- import Debug -- TODO akeeton: Remove
import Html exposing (..)
import Html.Attributes exposing (id, type', width, height, src, style)
import Html.Events exposing (onClick, on, targetValue)
import Maybe as M
import Signal exposing (Address)

import MyList
import Playlist exposing (Playlist)


-- Public definitions


type alias PlaylistArea =
    { playlists: List Playlist
    , focus : Focus
    , importTextAreaInput : String
    , importablePlaylist : Result String Playlist
    }


type Action
    = DoNothing
    | PlaylistAction Int Playlist.Action
    | ChangeFocus Focus
    | ImportablePlaylistAction Playlist.Action
    | ImportPlaylist Playlist
    | UpdateImportTextArea String


init : List Playlist -> PlaylistArea
init playlists =
    { playlists = playlists
    , focus = Importer
    , importTextAreaInput = "Playlist goes here"
    , importablePlaylist = Err ""
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

        ChangeFocus focus' ->
            { area | focus <- focus' }

        ImportablePlaylistAction playlistAction ->
            let
                importablePlaylist' =
                    Result.map (Playlist.update playlistAction) area.importablePlaylist
            in
                { area | importablePlaylist <- importablePlaylist' }

        ImportPlaylist playlist ->
            { area | playlists <- area.playlists ++ [ playlist ] }

        UpdateImportTextArea input ->
            { area |
                importTextAreaInput <- input,
                importablePlaylist <- Playlist.init input }


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
        playlistTabListHtml = playlistToTabListHtml address area.playlists
        focusHtml = focusToHtml address area
    in
        div
            [ id "playlist-area" ]
            [ playlistTabListHtml
            , focusHtml
            ]


-- Private definitions


type Focus
    = PlaylistIndex Int
    | Importer


playlistToTabListHtml : Address Action -> List Playlist -> Html
playlistToTabListHtml address playlists =
    let
        playlistTabsHtmls =
            List.indexedMap (playlistToTabHtml address) playlists

        importerTabHtml =
            button
                [ onClick address <| ChangeFocus Importer ]
                [ text "+" ]
    in
        div
            [ id "playlist-tabs" ]
            (playlistTabsHtmls ++ [ importerTabHtml ])


playlistToTabHtml : Address Action -> Int -> Playlist -> Html
playlistToTabHtml address n playlist =
    let
        label = toString (n + 1) ++ ": " ++ Playlist.name playlist
        action = ChangeFocus <| PlaylistIndex n
    in
        button [ onClick address action ] [ text label ]


focusToHtml : Address Action -> PlaylistArea -> Html
focusToHtml address area =
    case area.focus of
        PlaylistIndex n ->
            let
                -- TODO akeeton: Move into playlistNToHtml
                maybePlaylist = MyList.getAt n area.playlists

                maybePlaylistHtml =
                    M.map (playlistNToHtml address n) maybePlaylist

                playlistHtml =
                    M.withDefault (text "Error") maybePlaylistHtml
            in
                div
                    [ id <| "playlist-" ++ toString n]
                    [ playlistHtml ]
        Importer ->
            let
                -- TODO akeeton: Refactor into function
                (importPlaylistButtonAction, importablePlaylistHtml) =
                    case area.importablePlaylist of
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
            in
                div
                    [ id "playlist-importer" ]
                    [ h1
                        []
                        [ text "Playlist Importer" ]
                    , textarea
                        [ on "input" targetValue <| handleImportTextAreaInput address
                        , style
                            [ ("width", "400px")
                            , ("height", "300px")
                            ]
                        ]
                        [ text area.importTextAreaInput ]
                    , button
                        [ onClick address importPlaylistButtonAction ]
                        [ text "Import playlist" ]
                    , div
                        [ id "playlist-preview" ]
                        [ importablePlaylistHtml ]
                    ]

