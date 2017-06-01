import Test.DocTest


main :: IO ()
main =
    doctest
        [ "-isrc"
        , "-XOverloadedStrings"

        -- Dependencies

        , "tests/Test/Utils.hs"

        -- Tests

        , "src/Shikensu.hs"

        -- , "src/Shikensu/Contrib/IO.hs"
        -- , "src/Shikensu/Internal/Utilities.hs"
        --
        -- , "src/Shikensu/Contrib.hs"
        -- , "src/Shikensu/Metadata.hs"
        -- , "src/Shikensu/Sorting.hs"
        -- , "src/Shikensu/Types.hs"
        -- , "src/Shikensu/Utilities.hs"
        ]
