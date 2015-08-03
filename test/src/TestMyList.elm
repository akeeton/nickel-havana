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
            [ defaultTest <| MyList.getAt zeroToFive 0 `assertEqual` Just 0
            , defaultTest <| MyList.getAt zeroToFive 5 `assertEqual` Just 5
            , defaultTest <| MyList.getAt zeroToFive -1 `assertEqual` Nothing
            , defaultTest <| MyList.getAt zeroToFive 6 `assertEqual` Nothing
            , defaultTest <| MyList.getAt [] 0 `assertEqual` Nothing
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
