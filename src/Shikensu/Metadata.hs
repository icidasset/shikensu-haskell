{-| Metadata functions.

-}
module Shikensu.Metadata
    ( transposeMetadata
    , transposeToMetadata
    ) where

import Flow
import Shikensu.Internal.Types

import qualified Data.Aeson as Aeson (FromJSON, Result(..), ToJSON, fromJSON, toJSON)
import qualified Data.Aeson.KeyMap as KeyMap (empty, fromList)


{-| Transpose metadata.

Transpose our metadata object to a given type
which implements the Aeson.FromJSON instance.

> data Example =
>     Example { some :: Text }
>     deriving (Generic, FromJSON)
>
> keyMap      = KeyMap.fromList [ ("some", "metadata") ]
> defaultEx   = Example { some = "default" }
> example     = transposeMetadata keyMap defaultEx :: Example

-}
transposeMetadata :: Aeson.FromJSON a => Metadata -> a -> a
transposeMetadata keyMap fallback =
    let
        result = keyMap
            |> Aeson.toJSON
            |> Aeson.fromJSON :: Aeson.FromJSON b => Aeson.Result b
    in
        case result of
            Aeson.Success x -> x
            Aeson.Error _   -> fallback


{-| Inverse of `transposeMetadata`.
-}
transposeToMetadata :: (Aeson.ToJSON a) => a -> Metadata
transposeToMetadata generic =
    let
        result = generic
            |> Aeson.toJSON
            |> Aeson.fromJSON :: Aeson.FromJSON b => Aeson.Result b
    in
        case result of
            Aeson.Success x -> x
            Aeson.Error _   -> KeyMap.empty
