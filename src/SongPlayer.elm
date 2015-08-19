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
import Regex exposing (Regex, regex)
import Signal exposing (Address)


type alias SongPlayer =
    { service : Service
    , url : String
    , songId : String
    }


type Action
    = DoNothing


init : String -> SongPlayer
init url =
    let
        (service, songId) = parseUrl url
    in
        { service = service
        , url = url
        , songId = songId
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


parseUrl : String -> (Service, String)
parseUrl url =
    Maybe.withDefault (Null, "null-ID") <| parseYoutubeUrl url


parseYoutubeUrl : String -> Maybe (Service, String)
parseYoutubeUrl url =
    let
        matches : List Regex.Match
        matches = Regex.find (Regex.AtMost 1) youtubeRegex url
    in
        List.head matches
        `Maybe.andThen` (\match -> List.head match.submatches)
        `Maybe.andThen` (\maybeSongId -> maybeSongId)
        `Maybe.andThen` (\songId -> Just (Youtube, songId))


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
                srcUrl = "https://www.youtube.com/embed/" ++ player.songId ++ "?autoplay=1&controls=0&disablekb=1&enablejsapi=1&fs=0&rel=0&iv_load_policy=3"
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


-- From http://stackoverflow.com/a/5831191/68086
-- This regex matches more than what is needed.  For example, it will match in
-- an <a> tag.
youtubeRegex : Regex
youtubeRegex =
    regex "https?:\\/\\/(?:[0-9A-Z-]+\\.)?(?:youtu\\.be\\/|youtube(?:-nocookie)?\\.com\\S*[^\\w\\s-])([\\w-]{11})(?=[^\\w-]|$)(?![?=&+%\\w.-]*(?:['\"][^<>]*>|<\\/a>))[?=&+%\\w.-]*"

