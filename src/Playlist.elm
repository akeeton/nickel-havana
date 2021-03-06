module Playlist
    ( Playlist
    , Action
    , init
    , update
    , view
    , name
    , length
    , getActiveSong
    , cycle
    )
    where

import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Decode exposing ((:=))
import List

import MyList
import Song exposing (Song)

-- Public definitions


type alias Playlist =
    { name: String
    , songs: List Song
    }


init : String -> Result String Playlist
init = JD.decodeString decoder


update : Action -> Playlist -> Playlist
update action playlist =
    case action of
        MoveSong n destination ->
            let n' =
                case destination of
                    Up ->
                        n - 1

                    Down ->
                        n + 1

                    Top ->
                        0

                    Bottom ->
                        length playlist - 1
            in
                case MyList.move n n' playlist.songs of
                    Just songs' ->
                        { playlist | songs <- songs' }
                    Nothing ->
                        playlist

        DoNothing ->
            playlist


songDestinationToString : SongDestination -> String
songDestinationToString destination =
    case destination of
        Up ->
            "Up"

        Down ->
            "Down"

        Top ->
            "Top"

        Bottom ->
            "Bottom"


view : Signal.Address Action -> Playlist -> Html
view address playlist =
    let
        headerHtml = h3 [] [ text playlist.name ]
        songsHtmls = List.indexedMap (songNToHtml address) playlist.songs
    in
        div [] (headerHtml :: songsHtmls)


name : Playlist -> String
name =
    .name


length : Playlist -> Int
length playlist =
    List.length playlist.songs


getActiveSong : Playlist -> Maybe Song
getActiveSong playlist =
    List.head playlist.songs


{-| Moves the first song to the end of playlist -}
cycle : Playlist -> Playlist
cycle playlist =
    let
        maybeSongs' =
            MyList.move 0 (List.length playlist.songs - 1) playlist.songs

        songs' = Maybe.withDefault [] maybeSongs'
    in
        { playlist | songs <- songs' }

-- Private definitions


type Action
    = MoveSong Int SongDestination
    | DoNothing


songNToHtml : Signal.Address Action -> Int -> Song -> Html
songNToHtml address n song =
    let
        makeMoveButton : SongDestination -> Html
        makeMoveButton destination =
            button
                [ onClick address <| MoveSong n destination ]
                [ text <| songDestinationToString destination ]
    in
        div
            []
            [ makeMoveButton Up
            , makeMoveButton Down
            , makeMoveButton Top
            , makeMoveButton Bottom
            , Song.view song
            ]


type SongDestination
    = Up
    | Down
    | Top
    | Bottom


decoder : JD.Decoder Playlist
decoder =
    JD.object2
        (\name songs -> { name = name, songs = songs })
        ("name" := JD.string)
        ("songs" := JD.list Song.decoder)


