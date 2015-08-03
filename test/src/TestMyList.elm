module TestMyList where

import Html exposing (..)
import String

import ElmTest.Assertion as A exposing (assertEqual, assert)
import ElmTest.Runner.String exposing (runDisplay)
import ElmTest.Test exposing (..)

import MyList exposing(..)


tests : Test
tests =
    let
        zeroToFive = [0, 1, 2, 3, 4, 5]

        getAtTests = List.map defaultTest
            [ Nothing `assertEqual` getAt zeroToFive -1
            , Just 0 `assertEqual` getAt zeroToFive 0
            , Just 3 `assertEqual` getAt zeroToFive 3
            , Just 5 `assertEqual` getAt zeroToFive 5
            , Nothing `assertEqual` getAt zeroToFive 6
            , Nothing `assertEqual` getAt [] 0
            , Nothing `assertEqual` getAt [] 5
            ]

        removeAtTests = List.map defaultTest
            [ Nothing `assertEqual` removeAt zeroToFive -1
            , Just [1, 2, 3, 4, 5] `assertEqual` removeAt zeroToFive 0
            , Just [0, 1, 2, 4, 5] `assertEqual` removeAt zeroToFive 3
            , Just [0, 1, 2, 3, 4] `assertEqual` removeAt zeroToFive 5
            , Nothing `assertEqual` removeAt zeroToFive 6
            ]
    in
        suite "MyList Tests" <| getAtTests ++ removeAtTests


main : Html
main =
    let
        results = runDisplay tests
        lines = String.split "\n" results
        htmlLines = List.map (\s -> p [] [ text s ]) lines
    in
        div [] htmlLines

{- For node-elm-test
import Signal exposing (..)
import ElmTest.Run as R
import ElmTest.Runner.Console exposing (runDisplay)
import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)
import IO.Runner as Run

console : IO ()
console = runDisplay tests


port requests : Signal Request
port requests = Run.run responses console


port responses : Signal Response
-}
