module Main where

--import ElmFire exposing (..)
--import ElmFire.Auth as Auth
import Html exposing (..)
import Html.Attributes exposing (id, type', width, height, src, style)
-- import Html.Events exposing (onClick, on, targetValue)

import Debug -- TODO akeeton: Remove
-- import Playlist exposing (Playlist)
import PlaylistArea exposing (PlaylistArea)
import Player exposing (Player, NullPlayer)


-- MODEL

type alias Model a =
    { playlistArea : PlaylistArea
    , playing : Player a
    }


initialModel : Model {}
initialModel =
    { playlistArea = PlaylistArea.init []
    , playing = Player.initNullPlayer
    }


-- UPDATE
type Action
    = DoNothing
    | PlaylistAreaAction PlaylistArea.Action


update : Action -> Model a -> Model a
update action model =
    case action of
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


-- VIEW

view : Signal.Address Action -> Model a -> Html
view address model =
    let
        forwardingAddress = Signal.forwardTo address PlaylistAreaAction

        playlistAreaHtml =
            PlaylistArea.view forwardingAddress model.playlistArea
    in
        div
            [ id "site" ]
            [ playlistAreaHtml ]


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
