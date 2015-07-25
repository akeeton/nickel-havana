module Player
    ( Player
    , NullPlayer
    , initNullPlayer
    )
    where

import Html exposing (Html, text, div)
import Html.Attributes exposing (style)


type Action
    = Play
    | Pause


type Player a =
    Player
        { a |
            update : Action -> Player a -> Player a,
            view : Signal.Address Action -> Player a -> Html
        }


--  unwrapPlayer : Player a -> {a | ...}
unwrapPlayer player =
    case player of
        Player(player') ->
            player'


type alias NullPlayer = Player {}


initNullPlayer : NullPlayer
initNullPlayer =
    Player
        { update action player = player
        , view address player = text ""
        }


type alias TextPlayerExtra =
    { model :
        { text : String
        , action : Action
        }
    }


type alias TextPlayer = Player TextPlayerExtra


textPlayerUpdate : Action -> TextPlayer -> TextPlayer
textPlayerUpdate action player =
    let
        player' = unwrapPlayer player
        model = player'.model
        model' = { model | action <- action }
    in
        Player { player' | model <- model' }


textPlayerView : Signal.Address Action -> TextPlayer -> Html
textPlayerView address player =
    let
        player' = unwrapPlayer player
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
         Player
            { update = textPlayerUpdate
            , view = textPlayerView
            , model = model'
            }
