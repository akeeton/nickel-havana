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



{-
    List.map Youtube ["vR5HJp_xXRs", "RwpjyLUj0XU"]


toHtml : Maybe Source -> Html
toHtml maybeSource =
  case maybeSource of
    Nothing -> iframe [] []

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
-}
