module MyList
    ( getAt
    , removeAt
    )
    where


getAt : List a -> Int -> Maybe a
getAt list index =
    if index < 0 then
        Nothing
    else
        List.head <| List.drop index list


removeAt : List a -> Int -> Maybe (List a)
removeAt list index =
    if index < List.length list then
        Maybe.Just <| List.take index list ++ List.drop (index + 1) list
    else
        Nothing


{-
move : List a -> Int -> Int -> Maybe (List a)
move list from to =
    -}

