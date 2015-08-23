module Main where

import Debug -- TODO akeeton: Remove
import ElmFire as Fire
import Html exposing (..)
import Html.Attributes exposing (id, type', width, height, src, style)
import Html.Events exposing (onClick, on, targetValue)
import Json.Decode as JD
import Json.Encode as JE
import Signal exposing (Address, Mailbox, mailbox, message, send)
import String
import Task exposing (Task)

-- import Playlist exposing (Playlist)
import PlaylistArea exposing (PlaylistArea)
import SongPlayer exposing (SongPlayer)


type alias Model =
    { errors : List String
    , playlistArea : PlaylistArea
    , songPlayer : SongPlayer
    , playing : Bool
    , username : String
    , enteredUsername : Bool
    , waitlist : Waitlist
    }


type Action
    = DoNothing
    | ErrorHappened String
    | PlaylistAreaAction PlaylistArea.Action
    | SongPlayerAction SongPlayer.Action
    | Play
    | Skip
    | Stop
    | UpdateUsername String
    | EnterUsername
    | WaitlistChanged JE.Value


init : Model
init =
    let
        initialModel : Model
        initialModel =
            { errors = []
            , playlistArea = PlaylistArea.init []
            , songPlayer = SongPlayer.init "http://null.example.com"
            , playing = False
            , username = ""
            , enteredUsername = False
            , waitlist = []
            }
    in
        initialModel


update : Action -> Model -> Model
update action model =
    case action of
        DoNothing ->
            model

        ErrorHappened error ->
            { model | errors <- error::model.errors }

        PlaylistAreaAction playlistAreaAction ->
            let
                playlistArea' =
                    PlaylistArea.update playlistAreaAction model.playlistArea
            in
                { model | playlistArea <- playlistArea' }

        Play ->
            if model.playing then
                model
            else
                loadActiveSongIntoSongPlayer { model | playing <- True }

        Skip ->
            let
                playlistArea' =
                    PlaylistArea.cycleActivePlaylist model.playlistArea

                model' = { model | playlistArea <- playlistArea' }
            in
                loadActiveSongIntoSongPlayer model'

        Stop ->
            { model |
                playing <- False,
                songPlayer <- SongPlayer.init "http://null.example.com" }

        UpdateUsername username ->
            { model | username <- username }

        EnterUsername ->
            { model | enteredUsername <- True }

        WaitlistChanged jsonWaitlist ->
            { model | waitlist <- decodeWaitlist jsonWaitlist }

        otherwise ->
            -- TODO akeeton: Remove
            Debug.crash "Main.Action case not implemented in Main.update"


-- TODO akeeton: Refactor
view : Address Action -> Model -> Html
view address model =
    let
        viewHtml =
            if model.enteredUsername then
                viewNormal address model
            else
                viewAskForName address model

        errorsHtml = textarea [] [ text <| String.join "\n" model.errors ]
    in
        div
            []
            [ viewHtml
            , hr [] []
            , h1 [] [ text "Errors" ]
            , errorsHtml
            ]


viewNormal : Address Action -> Model -> Html
viewNormal address model =
    let
        songPlayerHtml = songPlayerToHtml address model.songPlayer
        playlistAreaHtml = playlistAreaToHtml address model.playlistArea
        waitlistHtml = div [] <| List.map (\x -> p [] [ text x ]) model.waitlist
    in
        div
            [ id "site" ]
            [ h1 [] [ text "Waitlist" ]
            , waitlistHtml
            , hr [] []
            , h1 [] [ text "Song Player" ]
            , songPlayerHtml
            , button [ onClick address Play ] [ text "Play" ]
            , button [ onClick address Skip ] [ text "Skip" ]
            , button [ onClick address Stop ] [ text "Stop" ]
            , hr [] []
            , playlistAreaHtml
            ]


-- TODO akeeton: Refactor
viewAskForName : Address Action -> Model -> Html
viewAskForName address model =
    let
        buttonAction =
            if model.username == "" then
                DoNothing
            else
                EnterUsername
    in
        div
            []
            [ h1 [] [ text "Enter a name" ]
            , label
                []
                [ text "Username: "
                , input
                    [ on "input" targetValue (\x -> message address <| UpdateUsername x) ]
                    []
                , button
                    -- TODO akeeton: Make the enter key activate the button
                    [ onClick address buttonAction ]
                    [ text "Enter" ]
                ]
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


loadActiveSongIntoSongPlayer : Model -> Model
loadActiveSongIntoSongPlayer model =
    case PlaylistArea.getActiveSong model.playlistArea of
        Just song ->
            { model | songPlayer <- SongPlayer.init song.url }

        Nothing ->
            model


type alias Waitlist = List String


decodeWaitlist : JE.Value -> Waitlist
decodeWaitlist jsonWaitlist =
    let
        waitlistDecoder = JD.list JD.string
    in
        case JD.decodeValue waitlistDecoder jsonWaitlist of
            Ok waitlist ->
                waitlist

            Err _ ->
                []


fireUrl : String
fireUrl = "https://brilliant-inferno-5135.firebaseio.com"


fireOnWaitlistChange : Fire.Snapshot -> Task x ()
fireOnWaitlistChange snapshot =
    send actions.address <| WaitlistChanged snapshot.value


logError : x -> Task x ()
logError error =
    send actions.address <| ErrorHappened (toString error)


port fireSubscriber : Task x ()
port fireSubscriber =
    let
        location = Fire.sub "waitlist" <| Fire.fromUrl fireUrl

        subscriber : Task Fire.Error Fire.Subscription
        subscriber =
            Fire.subscribe
                fireOnWaitlistChange
                (always <| Task.succeed ()) -- TODO akeeton: Use a callback
                Fire.valueChanged
                location

        -- subscriberAndErrorLogger : Task x ()
        subscriberAndErrorLogger = Task.onError subscriber logError
    in
        subscriberAndErrorLogger


main : Signal Html
main =
    Signal.map (view actions.address) model


model : Signal Model
model =
    Signal.foldp update init actions.signal


actions : Signal.Mailbox (Action)
actions =
    Signal.mailbox DoNothing
