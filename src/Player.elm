module Player where

import Html exposing (Html, text, div)
import Html.Attributes exposing (style)


type Action
    = Play
    | Pause


type Player a = Player
    { a |
        update : Action -> Player a -> Player a,
        view : Signal.Address Action -> Player a -> Html
    }


type TextPlayer = TextPlayer
    { update : Action -> TextPlayer -> TextPlayer
    , view : Signal.Address Action -> TextPlayer -> Html
    , model :
        { text : String
        , action : Action
        }
    }


unwrapTextPlayer player =
    case player of
        TextPlayer player' ->
            player'


textPlayerUpdate : Action -> TextPlayer -> TextPlayer
textPlayerUpdate action player =
    let
        player' = unwrapTextPlayer player
        model = player'.model
        model' = { model | action <- action }
    in
        TextPlayer { player' | model <- model' }


textPlayerView : Signal.Address Action -> TextPlayer -> Html
textPlayerView address player =
    let
        player' = unwrapTextPlayer player
        color = case player'.model.action of
            Play ->
                "green"

            Pause ->
                "yellow"
    in
        div
            [ style
                [ ("color", color) ]
            ]
            [ text player'.model.text ]


initTextPlayer : String -> TextPlayer
initTextPlayer text =
    let
        model' =
            { text = ""
            , action = Pause
            }
    in
        TextPlayer
            { update = textPlayerUpdate
            , view = textPlayerView
            , model = model'
            }
