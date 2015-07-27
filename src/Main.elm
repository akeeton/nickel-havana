module Main where

--import ElmFire exposing (..)
--import ElmFire.Auth as Auth
import Html exposing (..)
import Html.Attributes exposing (id, type', width, height, src, style)
import Html.Events exposing (onClick)

import Playlist exposing (Playlist)
import Player exposing (Player, NullPlayer)


-- MODEL

type alias Model a =
    { playlists : List Playlist
    , playing : Player a
    }


initialModel : Model {}
initialModel =
    { playlists = []
    , playing = Player.initNullPlayer
    }


-- UPDATE
type Action
    = LoadPlaylist
    --| ChangePlaying(Player a)
    | DoNothing


update : Action -> Model a -> Model a
update action model =
    model


-- VIEW

view : Signal.Address Action -> Model a -> Html
view address model =
    div [] []


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
