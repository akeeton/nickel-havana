module TestMyList where

import Html exposing (..)
import String

import ElmTest.Assertion as A exposing (assertEqual, assert)
import ElmTest.Runner.String exposing (runDisplay)
import ElmTest.Test exposing (..)

import MyList exposing(..)


zeroToFive = [0, 1, 2, 3, 4, 5]


getAtTests = List.map defaultTest
    [ Nothing `assertEqual` getAt -1 zeroToFive
    , Just 0 `assertEqual` getAt 0 zeroToFive
    , Just 3 `assertEqual` getAt 3 zeroToFive
    , Just 5 `assertEqual` getAt 5 zeroToFive
    , Nothing `assertEqual` getAt 6 zeroToFive
    , Nothing `assertEqual` getAt 0 []
    , Nothing `assertEqual` getAt 5 []
    ]


insertAtTests = List.map defaultTest
    [ Nothing `assertEqual` insertAt 9 -1 zeroToFive
    , Just [9, 0, 1, 2, 3, 4, 5] `assertEqual` insertAt 9 0 zeroToFive
    , Just [0, 9, 1, 2, 3, 4, 5] `assertEqual` insertAt 9 1 zeroToFive
    , Just [0, 1, 9, 2, 3, 4, 5] `assertEqual` insertAt 9 2 zeroToFive
    , Just [0, 1, 2, 9, 3, 4, 5] `assertEqual` insertAt 9 3 zeroToFive
    , Just [0, 1, 2, 3, 9, 4, 5] `assertEqual` insertAt 9 4 zeroToFive
    , Just [0, 1, 2, 3, 4, 9, 5] `assertEqual` insertAt 9 5 zeroToFive
    , Just [0, 1, 2, 3, 4, 5, 9] `assertEqual` insertAt 9 6 zeroToFive
    , Nothing `assertEqual` insertAt 9 7 zeroToFive
    ]


removeAtTests = List.map defaultTest
    [ zeroToFive `assertEqual` removeAt -1 zeroToFive
    , [1, 2, 3, 4, 5] `assertEqual` removeAt 0 zeroToFive
    , [0, 2, 3, 4, 5] `assertEqual` removeAt 1 zeroToFive
    , [0, 1, 2, 3, 4] `assertEqual` removeAt 5 zeroToFive
    , zeroToFive `assertEqual` removeAt 6 zeroToFive
    ]


moveTests = List.map defaultTest
    [ Just [1, 2, 3, 4, 5, 0] `assertEqual` move 0 5 zeroToFive
    , Just [1, 2, 3, 4, 0, 5] `assertEqual` move 0 4 zeroToFive
    , Just [1, 2, 3, 0, 4, 5] `assertEqual` move 0 3 zeroToFive
    , Just [1, 2, 0, 3, 4, 5] `assertEqual` move 0 2 zeroToFive
    , Just [1, 0, 2, 3, 4, 5] `assertEqual` move 0 1 zeroToFive
    , Just [0, 1, 2, 3, 4, 5] `assertEqual` move 0 0 zeroToFive
    , Just [0, 1, 2, 4, 5, 3] `assertEqual` move 3 5 zeroToFive
    , Just [0, 1, 2, 4, 3, 5] `assertEqual` move 3 4 zeroToFive
    , Just [0, 1, 2, 3, 4, 5] `assertEqual` move 3 3 zeroToFive
    , Just [0, 1, 3, 2, 4, 5] `assertEqual` move 3 2 zeroToFive
    , Just [0, 3, 1, 2, 4, 5] `assertEqual` move 3 1 zeroToFive
    , Just [3, 0, 1, 2, 4, 5] `assertEqual` move 3 0 zeroToFive
    , Just [0, 1, 2, 3, 4, 5] `assertEqual` move 5 5 zeroToFive
    , Just [0, 1, 2, 3, 5, 4] `assertEqual` move 5 4 zeroToFive
    , Just [0, 1, 2, 5, 3, 4] `assertEqual` move 5 3 zeroToFive
    , Just [0, 1, 5, 2, 3, 4] `assertEqual` move 5 2 zeroToFive
    , Just [0, 5, 1, 2, 3, 4] `assertEqual` move 5 1 zeroToFive
    , Just [5, 0, 1, 2, 3, 4] `assertEqual` move 5 0 zeroToFive
    , Nothing `assertEqual` move -1 3 zeroToFive
    , Nothing `assertEqual` move 6 3 zeroToFive
    , Nothing `assertEqual` move 3 -1 zeroToFive
    , Nothing `assertEqual` move 3 6 zeroToFive
    ]


isValidIndexTests = List.map defaultTest
    [ assert <| not (isValidIndex -1 zeroToFive)
    , assert <| isValidIndex 0 zeroToFive
    , assert <| isValidIndex 1 zeroToFive
    , assert <| isValidIndex 2 zeroToFive
    , assert <| isValidIndex 3 zeroToFive
    , assert <| isValidIndex 4 zeroToFive
    , assert <| isValidIndex 5 zeroToFive
    , assert <| not (isValidIndex 6 zeroToFive)
    , assert <| not (isValidIndex 0 [])
    ]


tests : Test
tests =
    suite "MyList Tests" <|
        getAtTests
        ++ insertAtTests
        ++ removeAtTests
        ++ moveTests
        ++ isValidIndexTests


main : Html
main =
    let
        results = runDisplay tests
        lines = String.split "\n" results
        htmlLines = List.map (\s -> p [] [ text s ]) lines
    in
        div [] htmlLines

