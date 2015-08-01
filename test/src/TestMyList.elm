module TestMyList where

import Basics exposing (..)
import Signal exposing (..)

import ElmTest.Assertion as A exposing (assertEqual, assert)
import ElmTest.Run as R
import ElmTest.Runner.Console exposing (runDisplay)
import ElmTest.Test exposing (..)
import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)
import IO.Runner as Run

import MyList


tests : Test
tests =
    let
        inputList = [0, 1, 2, 3, 4, 5]
    in
    suite "MyList Tests"
        [ defaultTest <| MyList.getAt inputList 0 `assertEqual` Just 0
        ]


console : IO ()
console = runDisplay tests


port requests : Signal Request
port requests = Run.run responses console


port responses : Signal Response
