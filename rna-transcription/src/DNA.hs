module DNA (toRNA) where


toRNA :: String -> Maybe String
toRNA xs = traverse transcribe xs
    where
        transcribe x =
            case x of
                'C' -> Just 'G'
                'G' -> Just 'C'
                'A' -> Just 'U'
                'T' -> Just 'A'
                _   -> Nothing
