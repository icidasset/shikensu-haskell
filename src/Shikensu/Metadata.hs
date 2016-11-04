module Shikensu.Metadata
  ( transposeMetadata
  , transposeToMetadata
  ) where


{-| Metadata functions.
-}

import Flow
import Shikensu.Types

import qualified Data.Aeson as Aeson (FromJSON, Result(..), ToJSON, fromJSON, toJSON)
import qualified Data.HashMap.Strict as HashMap (empty)



{-| Transpose metadata.

Transpose our metadata object to a given type
which implements the Aeson.FromJSON instance.

    data Example =
      Example { some :: Text }
      deriving (Generic, FromJSON)

    hashMap     = HashMap.fromList [ ("some", "metadata") ]
    defaultEx   = Example { some = "default" }
    example     = transposeMetadata hashMap defaultExample :: Example

-}
transposeMetadata :: Aeson.FromJSON a => Metadata -> a -> a
transposeMetadata hashMap fallback =
  let
    result = hashMap
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
      Aeson.Error _   -> HashMap.empty
