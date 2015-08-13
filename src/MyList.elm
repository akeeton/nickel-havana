module MyList
    ( getAt
    , removeAt
    , move
    , isValidIndex
    )
    where


getAt : List a -> Int -> Maybe a
getAt list index =
    if isValidIndex list index then
        List.head <| List.drop index list
    else
        Nothing


removeAt : List Int -> Int -> List Int
removeAt list index =
    if isValidIndex list index then
        List.take index list ++ List.drop (index + 1) list
    else
        list


move : List a -> Int -> Int -> Maybe (List a)
move list from to = Nothing


isValidIndex : List a -> Int -> Bool
isValidIndex list index =
    index >= 0 && index < List.length list

