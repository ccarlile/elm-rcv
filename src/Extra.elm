module Extra exposing (maybeFlatten)

maybeFlatten : List (Maybe a) -> List a
maybeFlatten maybes =
    case maybes of
        Just a :: rest ->
            a :: ( maybeFlatten rest )
        Nothing :: rest ->
            maybeFlatten rest
        [] -> []
