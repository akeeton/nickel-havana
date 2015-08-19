module Main where

import Debug -- TODO akeeton: Remove
import Effects exposing (Effects, Never)
--import ElmFire exposing (..)
--import ElmFire.Auth as Auth
import Html exposing (..)
import Html.Attributes exposing (id, type', width, height, src, style)
-- import Html.Events exposing (onClick, on, targetValue)
import Signal exposing (Address)
import StartApp exposing (App, start)
import Task exposing (Task)

-- import Playlist exposing (Playlist)
import PlaylistArea exposing (PlaylistArea)
import SongPlayer exposing (SongPlayer)


type alias Model =
    { playlistArea : PlaylistArea
    , songPlayer : SongPlayer
    }


type Action
    = DoNothing
    | PlaylistAreaAction PlaylistArea.Action
    | SongPlayerAction SongPlayer.Action


init : (Model, Effects Action)
init =
    let
        initialModel : Model
        initialModel =
            { playlistArea = PlaylistArea.init []
            , songPlayer = SongPlayer.init "http://null.example.com"
            }
    in
        ( initialModel, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
    let
        model' = case action of
            DoNothing ->
                model

            PlaylistAreaAction playlistAreaAction ->
                let
                    playlistArea' =
                        PlaylistArea.update playlistAreaAction model.playlistArea
                in
                    { model | playlistArea <- playlistArea' }

            otherwise ->
                -- TODO akeeton: Remove
                Debug.crash "Main.Action case not implemented in Main.update"
    in
        (model', Effects.none)


view : Address Action -> Model -> Html
view address model =
    let
        songPlayerHtml = songPlayerToHtml address model.songPlayer
        playlistAreaHtml = playlistAreaToHtml address model.playlistArea
    in
        div
            [ id "site" ]
            [ h1 [] [ text "Song Player" ]
            , songPlayerHtml
            , hr [] []
            , playlistAreaHtml
            ]


inputs : List (Signal Action)
inputs =
    []


countStyle : Attribute
countStyle =
    style
        [ ("font-size", "20px")
        , ("font-family", "monospace")
        , ("display", "inline-block")
        , ("width", "50px")
        , ("text-align", "center")
        ]


playlistAreaToHtml : Address Action -> PlaylistArea -> Html
playlistAreaToHtml address playlistArea =
    let
        forwardingAddress = Signal.forwardTo address PlaylistAreaAction
    in
        PlaylistArea.view forwardingAddress playlistArea


songPlayerToHtml : Address Action -> SongPlayer -> Html
songPlayerToHtml address songPlayer =
    let
        forwardingAddress = Signal.forwardTo address SongPlayerAction
    in
        SongPlayer.view forwardingAddress songPlayer


app : App Model
app =
    start { init = init, view = view, update = update, inputs = inputs }


main : Signal Html
main =
    app.html


port tasks : Signal (Task Never ())
port tasks =
    app.tasks
