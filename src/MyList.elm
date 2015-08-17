module MyList
    ( getAt
    , insertAt
    , removeAt
    , replaceAt
    , move
    , isValidIndex
    )
    where

import Maybe exposing (andThen)


getAt : Int-> List a -> Maybe a
getAt index list =
    if isValidIndex index list then
        List.head <| List.drop index list
    else
        Nothing


insertAt : a -> Int -> List a -> Maybe (List a)
insertAt x index list =
    if isValidIndex index list || index == List.length list then
        Just <| List.take index list ++ x::(List.drop index list)
    else
        Nothing


removeAt : Int -> List a -> Maybe (List a)
removeAt index list =
    if isValidIndex index list then
        Just <| List.take index list ++ List.drop (index + 1) list
    else
        Nothing


replaceAt : a -> Int -> List a -> Maybe (List a)
replaceAt x index list =
    insertAt x index list
    `Maybe.andThen` removeAt (index + 1)


move : Int -> Int -> List a -> Maybe (List a)
move from to list =
    let
        (insertCorrection, removeCorrection) =
            if from <= to then
                (1, 0)
            else
                (0, 1)
    in
        getAt from list
        `andThen` (\x -> insertAt x (to + insertCorrection) list)
        `andThen` removeAt (from + removeCorrection)


isValidIndex : Int -> List a -> Bool
isValidIndex index list =
    index >= 0 && index < List.length list

