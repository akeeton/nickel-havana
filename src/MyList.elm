module MyList
    (
    )
    where


getAt : List a -> Int -> Maybe a
getAt list index =
    List.head <| List.drop index list


removeAt : List a -> Int -> Maybe a
removeAt list index =
    if index < length list then
        Maybe.Just <| (take index list) ++ (drop (index + 1) list)
    else
        Nothing


{-
move : List a -> Int -> Int -> Maybe (List a)
move list from to =
    -}

