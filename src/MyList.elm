module MyList
    ( getAt
    , insertAt
    , removeAt
    , move
    , isValidIndex
    )
    where


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


removeAt : Int -> List Int -> List Int
removeAt index list =
    if isValidIndex index list then
        List.take index list ++ List.drop (index + 1) list
    else
        list


move : Int -> Int -> List a -> Maybe (List a)
move from to list = Nothing
    -- removeAt list from `Maybe.andThen` insertAt list to


isValidIndex : Int -> List a -> Bool
isValidIndex index list =
    index >= 0 && index < List.length list

