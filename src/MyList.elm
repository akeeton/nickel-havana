module MyList
    ( getAt
    , removeAt
    , move
    )
    where


getAt : List a -> Int -> Maybe a
getAt list index =
    if index < 0 then
        Nothing
    else
        List.head <| List.drop index list


removeAt : List Int -> Int -> List Int
removeAt list index =
    if 0 <= index && index < List.length list then
        List.take index list ++ List.drop (index + 1) list
    else
        list


move : List a -> Int -> Int -> Maybe (List a)
move list from to = Nothing

