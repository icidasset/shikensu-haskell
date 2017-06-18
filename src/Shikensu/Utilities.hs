module Shikensu.Utilities
    ( (!~>)
    , (~>)
    , io
    , lsequence
    , mapIO
    ) where

import Data.Aeson (FromJSON, ToJSON, fromJSON)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Flow
import Shikensu.Types

import qualified Data.Aeson as Json (Object, Result(..), encode)
import qualified Data.HashMap.Strict as HashMap (lookup)
import qualified Data.List as List (unzip, zip)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Lazy as Lazy.Text (unpack)
import qualified Data.Text.Lazy.Encoding as Lazy.Text (decodeUtf8)
import qualified Data.Tuple as Tuple (fst, snd)


-- IO


{-| IO Sequence helpers
-}
io :: ([Definition] -> [IO Definition]) -> Dictionary -> IO Dictionary
io fn =
    fn .> sequence


mapIO :: (Definition -> IO Definition) -> Dictionary -> IO Dictionary
mapIO =
    fmap .> io


{-| One way to deal with multiple dictionaries.

> lsequence
>     [ ( "pages", Shikensu.list ["src/pages/**/*.html"] rootDir    )
>     , ( "js",    Shikensu.list ["src/javascript/**/*.js"] rootDir )
>     ]

From multiple IO monads to a single IO monad.
-}
lsequence :: Monad m => [( String, m a )] -> m [( String, a )]
lsequence list =
    let
        unzippedList =
            List.unzip list

        identifiers =
            Tuple.fst unzippedList

        dictionaries =
            Tuple.snd unzippedList
    in
        dictionaries
            |> sequence
            |> fmap (List.zip identifiers)



-- PURE


{-| Get stuff out of the metadata.
    Returns a `Maybe`.
-}
(~>) :: (FromJSON a, ToJSON a) => Metadata -> Text -> Maybe a
(~>) obj key =
    HashMap.lookup key obj
        |> fmap fromJSON
        |> fmap fromJSONResult


{-| Get stuff out of the metadata.
    Does NOT return a `Maybe`, but gives an error if it isn't found.
-}
(!~>) :: (FromJSON a, ToJSON a) => Metadata -> Text -> a
(!~>) obj key =
    case (obj ~> key) of
        Just x  -> x
        Nothing -> error <|
            "Could not find the key `" <> Text.unpack key         <> "` " <>
            "on the metadata object `" <> aesonObjectToString obj <> "` using (!~>)"



-- Private


fromJSONResult :: ToJSON a => Json.Result a -> a
fromJSONResult result =
    case result of
        Json.Success x -> x
        Json.Error err -> error err


aesonObjectToString :: Json.Object -> String
aesonObjectToString =
    Json.encode .> Lazy.Text.decodeUtf8 .> Lazy.Text.unpack
