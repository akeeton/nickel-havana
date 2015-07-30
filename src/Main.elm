module Main where

--import ElmFire exposing (..)
--import ElmFire.Auth as Auth
import Html exposing (..)
import Html.Attributes exposing (id, type', width, height, src, style)
import Html.Events exposing (onClick, on, targetValue)

import Playlist exposing (Playlist)
import Player exposing (Player, NullPlayer)


-- MODEL

type alias Model a =
    { playlists : List Playlist
    , playing : Player a
    , importTextArea : String
    }


initialModel : Model {}
initialModel =
    { playlists = []
    , playing = Player.initNullPlayer
    , importTextArea = ""
    }


-- UPDATE
type Action
    = ImportPlaylist (Playlist)
    | UpdateImportTextArea (String)
    --| ChangePlaying (Player a)
    | DoNothing


update : Action -> Model a -> Model a
update action model =
    case action of
        ImportPlaylist playlist ->
            { model | playlists <- playlist :: model.playlists }
        UpdateImportTextArea text ->
            { model | importTextArea <- text }

        otherwise ->
            model


-- VIEW

view : Signal.Address Action -> Model a -> Html
view address model =
    let
        resImportedPlaylist = Playlist.init model.importTextArea

        (importPlaylistAction, importedPlaylistHtml) =
            case resImportedPlaylist of
                Ok importedPlaylist ->
                    ( ImportPlaylist importedPlaylist
                    , Playlist.view importedPlaylist
                    )

                Err message ->
                    (DoNothing, text message)

        textAreaStyle = style
            [ ("width", "400px")
            , ("height", "300px")
            ]

        playlistDisplay = List.map Playlist.view model.playlists
    in
        div
            [ id "site" ]
            [ div
                [ id "playlist-import" ]
                [ h1
                    []
                    [ text "Playlist Import" ]
                , textarea
                    [ on "input" targetValue handleImportTextAreaInput
                    , textAreaStyle
                    ]
                    []
                , div
                    [ id "playlist-preview" ]
                    [ importedPlaylistHtml ]
                , button
                    [ onClick address <| importPlaylistAction ]
                    [ text "Import playlist" ]
                ]
                , h1
                    []
                    [ text "Playlists" ]
            , div
                [ id "playlists" ]
                [ div
                    [ id "playlist-display" ]
                    playlistDisplay
                ]
            ]


handleImportTextAreaInput : String -> Signal.Message
handleImportTextAreaInput text =
    UpdateImportTextArea text
    |> Signal.message actions.address


countStyle : Attribute
countStyle =
    style
        [ ("font-size", "20px")
        , ("font-family", "monospace")
        , ("display", "inline-block")
        , ("width", "50px")
        , ("text-align", "center")
        ]


-- SIGNALS

main : Signal Html
main =
    Signal.map (view actions.address) model


model : Signal (Model {})
model =
    Signal.foldp update initialModel actions.signal


actions : Signal.Mailbox (Action)
actions =
    Signal.mailbox DoNothing
