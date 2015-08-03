module TestMyList where

import Html exposing (..)
import String

import ElmTest.Assertion as A exposing (assertEqual, assert)
import ElmTest.Runner.String exposing (runDisplay)
import ElmTest.Test exposing (..)

import MyList


tests : Test
tests =
    let
        zeroToFive = [0, 1, 2, 3, 4, 5]
    in
        suite "MyList Tests"
            [ defaultTest <| Just 0 `assertEqual` MyList.getAt zeroToFive 0
            , defaultTest <| Just 3 `assertEqual` MyList.getAt zeroToFive 3
            , defaultTest <| Just 5 `assertEqual` MyList.getAt zeroToFive 5
            , defaultTest <| Nothing `assertEqual` MyList.getAt zeroToFive -1
            , defaultTest <| Nothing `assertEqual` MyList.getAt zeroToFive 6
            , defaultTest <| Nothing `assertEqual` MyList.getAt [] 0
            , defaultTest <| Nothing `assertEqual` MyList.getAt [] 5
            ]


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
