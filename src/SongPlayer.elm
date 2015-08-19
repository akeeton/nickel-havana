module SongPlayer
    ( SongPlayer
    , Action
    , init
    , update
    , view
    )
    where

import Html exposing (..)
import Html.Attributes exposing (height, id, src, style, type', width)
import Signal exposing (Address)


type alias SongPlayer =
    { service : Service
    , url : String
    , identity : String
    }


type Action
    = DoNothing


init : String -> SongPlayer
init url =
    let
        (service, identity) = getServiceAndIdFromUrl url
    in
        { service = service
        , url = url
        , identity = identity
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
        service = Youtube
        identity = "vR5HJp_xXRs"
    in
        (service, identity)


toEmbedHtml : SongPlayer -> Html
toEmbedHtml player =
    case player.service of
        Null ->
            div
                []
                [ p [] [ text "Plug.dj music service (= the null player) :D" ]
                , p [] [ text <| toString player ]
                ]

        Youtube ->
            let
                srcUrl = "https://www.youtube.com/embed/" ++ player.identity ++ "?autoplay=1&controls=0&disablekb=1&enablejsapi=1&fs=0&rel=0&iv_load_policy=3"
            in
                iframe
                    [ id "ytplayer"
                    , type' "text/html"
                    , width 720
                    , height 405
                    , src srcUrl
                    , style [("border", "0")]
                    ]
                    []

        otherwise ->
            div [] [ text "NOT IMPLEMENTED!!!" ]

