module Shikensu.Types where

import Data.Map.Lazy (Map)
import Data.Text.Lazy (Text)


{-| A file definition, along with some additional properties.
-}
data Definition =
  Definition
    { basename :: String
    , dirname :: FilePath
    , extname :: String
    , pattern :: Pattern
    , rootPath :: FilePath
    , workingDirectory :: FilePath

    -- Additional properties
    , content :: Maybe Text
    , metadata :: Metadata
    , parentPath :: Maybe FilePath
    , pathToRoot :: FilePath
    }


{-| Definition dependencies.

A set of properties all definitions from a `Dictionary` produced
by `makeDictionary` have in common.

-}
data Dependencies =
  Dependencies
    { _pattern :: String
    , _rootPath :: FilePath
    }




-- Type aliases


type Dictionary = [Definition]
type Metadata = Map String String
type Pattern = String
