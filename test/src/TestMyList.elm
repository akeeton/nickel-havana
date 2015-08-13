module TestMyList where

import Html exposing (..)
import String

import ElmTest.Assertion as A exposing (assertEqual, assert)
import ElmTest.Runner.String exposing (runDisplay)
import ElmTest.Test exposing (..)

import MyList exposing(..)


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


insertAtTests = List.map defaultTest
    [ Nothing `assertEqual` insertAt zeroToFive 9 -1
    , Just [9, 0, 1, 2, 3, 4, 5] `assertEqual` insertAt zeroToFive 9 0
    , Just [0, 9, 1, 2, 3, 4, 5] `assertEqual` insertAt zeroToFive 9 1
    , Just [0, 1, 9, 2, 3, 4, 5] `assertEqual` insertAt zeroToFive 9 2
    , Just [0, 1, 2, 9, 3, 4, 5] `assertEqual` insertAt zeroToFive 9 3
    , Just [0, 1, 2, 3, 9, 4, 5] `assertEqual` insertAt zeroToFive 9 4
    , Just [0, 1, 2, 3, 4, 9, 5] `assertEqual` insertAt zeroToFive 9 5
    , Just [0, 1, 2, 3, 4, 5, 9] `assertEqual` insertAt zeroToFive 9 6
    , Nothing `assertEqual` insertAt zeroToFive 9 7
    ]


removeAtTests = List.map defaultTest
    [ zeroToFive `assertEqual` removeAt zeroToFive -1
    , [1, 2, 3, 4, 5] `assertEqual` removeAt zeroToFive 0
    , [0, 2, 3, 4, 5] `assertEqual` removeAt zeroToFive 1
    , [0, 1, 2, 3, 4] `assertEqual` removeAt zeroToFive 5
    , zeroToFive `assertEqual` removeAt zeroToFive 6
    ]


moveTests = List.map defaultTest
    [ Just [1, 2, 3, 4, 5, 0] `assertEqual` move zeroToFive 0 5
    , Just [1, 2, 3, 4, 0, 5] `assertEqual` move zeroToFive 0 4
    , Just [1, 2, 3, 0, 4, 5] `assertEqual` move zeroToFive 0 3
    , Just [1, 2, 0, 3, 4, 5] `assertEqual` move zeroToFive 0 2
    , Just [1, 0, 2, 3, 4, 5] `assertEqual` move zeroToFive 0 1
    , Just [0, 1, 2, 3, 4, 5] `assertEqual` move zeroToFive 0 0
    , Just [0, 1, 2, 4, 5, 3] `assertEqual` move zeroToFive 3 5
    , Just [0, 1, 2, 4, 3, 5] `assertEqual` move zeroToFive 3 4
    , Just [0, 1, 2, 3, 4, 5] `assertEqual` move zeroToFive 3 3
    , Just [0, 1, 3, 2, 4, 5] `assertEqual` move zeroToFive 3 2
    , Just [0, 3, 1, 2, 4, 5] `assertEqual` move zeroToFive 3 1
    , Just [3, 0, 1, 2, 4, 5] `assertEqual` move zeroToFive 3 0
    , Just [0, 1, 2, 3, 4, 5] `assertEqual` move zeroToFive 5 5
    , Just [0, 1, 2, 3, 5, 4] `assertEqual` move zeroToFive 5 4
    , Just [0, 1, 2, 5, 3, 4] `assertEqual` move zeroToFive 5 3
    , Just [0, 1, 5, 2, 3, 4] `assertEqual` move zeroToFive 5 2
    , Just [0, 5, 1, 2, 3, 4] `assertEqual` move zeroToFive 5 1
    , Just [5, 0, 1, 2, 3, 4] `assertEqual` move zeroToFive 5 0
    , Nothing `assertEqual` move zeroToFive -1 3
    , Nothing `assertEqual` move zeroToFive 6 3
    , Nothing `assertEqual` move zeroToFive 3 -1
    , Nothing `assertEqual` move zeroToFive 3 6
    ]


isValidIndexTests = List.map defaultTest
    [ assert <| not (isValidIndex zeroToFive -1)
    , assert <| isValidIndex zeroToFive 0
    , assert <| isValidIndex zeroToFive 1
    , assert <| isValidIndex zeroToFive 2
    , assert <| isValidIndex zeroToFive 3
    , assert <| isValidIndex zeroToFive 4
    , assert <| isValidIndex zeroToFive 5
    , assert <| not (isValidIndex zeroToFive 6)
    , assert <| not (isValidIndex [] 0)
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

