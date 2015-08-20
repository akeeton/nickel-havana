module PlaylistArea
    ( PlaylistArea
    , Action
    , init
    , update
    , view
    , getActivePlaylist
    , getActiveSong
    , cycleActivePlaylist
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
import Song exposing (Song)


-- Public definitions


type alias PlaylistArea =
    { playlists: List Playlist
    , focus : Focus
    , activePlaylistIndex : Int
    , importTextAreaInput : String
    , importablePlaylist : Result String Playlist
    }


type Action
    = DoNothing
    | PlaylistAction Int Playlist.Action
    | ChangeFocus Focus
    | ChangeActivePlaylist Int
    | ImportablePlaylistAction Playlist.Action
    | ImportPlaylist Playlist
    | UpdateImportTextArea String


init : List Playlist -> PlaylistArea
init playlists =
    let
        area =
            { playlists = playlists
            , focus = Importer
            , activePlaylistIndex = 0
            , importTextAreaInput = ""
            , importablePlaylist = Err ""
            }

        areaWithSamplePlaylist = updateImportTextArea samplePlaylist area
    in
        areaWithSamplePlaylist


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

        ChangeActivePlaylist n ->
            { area | activePlaylistIndex <- n }

        ImportablePlaylistAction playlistAction ->
            let
                importablePlaylist' =
                    Result.map (Playlist.update playlistAction) area.importablePlaylist
            in
                { area | importablePlaylist <- importablePlaylist' }

        ImportPlaylist playlist ->
            { area | playlists <- area.playlists ++ [ playlist ] }

        UpdateImportTextArea input ->
            updateImportTextArea input area


view : Signal.Address Action -> PlaylistArea -> Html
view address area =
    let
        playlistTabListHtml =
            playlistToTabListHtml address area.activePlaylistIndex area.playlists

        focusHtml = focusToHtml address area
    in
        div
            [ id "playlist-area" ]
            [ playlistTabListHtml
            , focusHtml
            ]


getActivePlaylist : PlaylistArea -> Maybe Playlist
getActivePlaylist area =
    MyList.getAt area.activePlaylistIndex area.playlists


getActiveSong : PlaylistArea -> Maybe Song
getActiveSong area =
    getActivePlaylist area
    `Maybe.andThen` (\playlist -> Playlist.getActiveSong playlist)


cycleActivePlaylist : PlaylistArea -> PlaylistArea
cycleActivePlaylist area =
    case getActivePlaylist area of
        Just activePlaylist ->
            replaceActivePlaylist area <| Playlist.cycle activePlaylist

        Nothing ->
            area


-- Private definitions


type Focus
    = PlaylistIndex Int
    | Importer


playlistNToHtml : Signal.Address Action -> Int -> Playlist -> Html
playlistNToHtml address n playlist =
    let
        forwardingAddress = Signal.forwardTo address <| PlaylistAction n
    in
        Playlist.view forwardingAddress playlist


handleImportTextAreaInput : Signal.Address Action -> String -> Signal.Message
handleImportTextAreaInput address text =
    Signal.message address <| UpdateImportTextArea text


playlistToTabListHtml : Address Action -> Int -> List Playlist -> Html
playlistToTabListHtml address activePlaylistIndex playlists =
    let
        playlistTabsHtmls =
            List.indexedMap (playlistToTabHtml address activePlaylistIndex) playlists

        importerTabHtml =
            button
                [ onClick address <| ChangeFocus Importer ]
                [ text "+" ]
    in
        div
            [ id "playlist-tabs" ]
            (playlistTabsHtmls ++ [ importerTabHtml ])


playlistToTabHtml : Address Action -> Int -> Int -> Playlist -> Html
playlistToTabHtml address activePlaylistIndex n playlist =
    let
        marker =
            if activePlaylistIndex == n then "*"
            else ""

        label = marker ++ toString (n + 1) ++ ": " ++ Playlist.name playlist
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
                    [ button
                        [ onClick address <| ChangeActivePlaylist n ]
                        [ text "Make active" ]
                    , playlistHtml
                    ]

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
                        [ h2 [] [ text "Preview" ]
                        , importablePlaylistHtml
                        ]
                    ]


updateImportTextArea : String -> PlaylistArea -> PlaylistArea
updateImportTextArea input area =
    { area |
        importTextAreaInput <- input,
        importablePlaylist <- Playlist.init input }


replaceActivePlaylist : PlaylistArea -> Playlist -> PlaylistArea
replaceActivePlaylist area activePlaylist' =
    case MyList.replaceAt activePlaylist' area.activePlaylistIndex area.playlists of
        Just playlists' ->
            { area | playlists <- playlists' }

        Nothing ->
            area


samplePlaylist : String
samplePlaylist =
    """{
    "name": "Sample playlist",
    "songs":
        [
            {
                "title": "Corgis!",
                "url": "https://www.youtube.com/watch?v=IAoVY2bQ8u4",
                "startTime": "",
                "endTime": ""
            },

            {
                "title": "Cats!",
                "url": "https://youtu.be/AvPNaJ8OWCM",
                "startTime": "",
                "endTime": ""
            },

            {
                "title": "Nothing!",
                "url": "http://url3.example.com",
                "startTime": "",
                "endTime": "3m7s"
            }
        ]
}"""
