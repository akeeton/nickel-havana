module SongPlayer
    ( SongPlayer
    , Action
    , init
    , update
    , view
    )
    where

import Html exposing (..)
import Signal exposing (Address)


type alias SongPlayer =
    { service : Service
    , url : String
    , id : String
    }


type Action
    = DoNothing


init : String -> SongPlayer
init url =
    let
        (service, id) = getServiceAndIdFromUrl url
    in
        { service = service
        , url = url
        , id = id
        }


update : Action -> SongPlayer -> SongPlayer
update action player =
    case action of
        DoNothing ->
            player


view : Address Action -> SongPlayer -> Html
view address player =
    let
        embedHtml = toEmbedHtml player
    in
        div
            []
            [ embedHtml ]


type Service
    = Null
    | Youtube
    | Hearthis
    | Soundcloud


getServiceAndIdFromUrl : String -> (Service, String)
getServiceAndIdFromUrl url =
    let
        service = Null
        id = "id"
    in
        (service, id)

toEmbedHtml : SongPlayer -> Html
toEmbedHtml player =
    case player.service of
        Null ->
            div
                []
                [ p [] [ text "Plug.dj music service (= the null player) :D" ]
                , p [] [ text <| toString player ]
                ]

        otherwise ->
            div [] [ text "NOT IMPLEMENTED!!!" ]

