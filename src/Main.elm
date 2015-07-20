module Main where

import ElmFire exposing (..)
import ElmFire.Auth as Auth
import Html exposing (..)
import Html.Attributes exposing (id, type', width, height, src, style)
import Html.Events exposing (onClick)


-- MODEL

type Source = Youtube String
videos = List.map Youtube ["vR5HJp_xXRs", "RwpjyLUj0XU"]

toHtml : Maybe Source -> Html
toHtml maybeSource =
  case maybeSource of
    Nothing                -> iframe [] []
    Just (Youtube videoId) ->
      let
        srcUrl = "https://www.youtube.com/embed/" ++ videoId ++ "?autoplay=1&controls=0&disablekb=1&enablejsapi=1&fs=0&rel=0&iv_load_policy=3"
      in
        iframe
          [ id "ytplayer"
          , type' "text/html"
          , width 720
          , height 405
          , src srcUrl
          , style [("border", "0")]
          ] []

type alias Model = List Source


-- UPDATE

type Action = Start | Next | Stop

update : Action -> Model -> Model
update action model =
  case action of
    Start -> model
    Next  -> Maybe.withDefault [] <| List.tail model
    Stop  -> []


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address Stop ] [ text "Stop" ]
    , button [ onClick address Next ] [ text "Next" ]
    , toHtml <| List.head model
    ]

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
  Signal.foldp update videos actions.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox Start
