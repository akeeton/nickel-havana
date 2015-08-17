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
import Player exposing (Player, NullPlayer)


type alias Model a =
    { playlistArea : PlaylistArea
    , playing : Player a
    }


type Action
    = DoNothing
    | PlaylistAreaAction PlaylistArea.Action


init : (Model {}, Effects Action)
init =
    let
        initialModel : Model {}
        initialModel =
            { playlistArea = PlaylistArea.init []
            , playing = Player.initNullPlayer
            }
    in
        ( initialModel, Effects.none)


update : Action -> Model a -> (Model a, Effects Action)
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


view : Address Action -> Model a -> Html
view address model =
    let
        forwardingAddress = Signal.forwardTo address PlaylistAreaAction

        playlistAreaHtml =
            PlaylistArea.view forwardingAddress model.playlistArea
    in
        div
            [ id "site" ]
            [ playlistAreaHtml ]


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


app : App (Model {})
app =
    start { init = init, view = view, update = update, inputs = inputs }


main : Signal Html
main =
    app.html


port tasks : Signal (Task Never ())
port tasks =
    app.tasks
