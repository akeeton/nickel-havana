module Main where

--import ElmFire exposing (..)
--import ElmFire.Auth as Auth
import Html exposing (..)
import Html.Attributes exposing (id, type', width, height, src, style)
import Html.Events exposing (onClick)


-- MODEL

type alias Model =
    Int


-- UPDATE
type Action
    = Nop


update : Action -> Model -> Model
update action model =
    model


-- VIEW

view : Signal.Address Action -> Model -> Html
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


model : Signal Model
model =
  Signal.foldp update 0 actions.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox Nop
