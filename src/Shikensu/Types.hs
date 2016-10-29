{-# LANGUAGE OverloadedStrings #-}
module Shikensu.Types where

import Data.Aeson (Object)
import Data.Text (Text)


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
    } deriving (Show)


{-| Definition dependencies.

A set of properties all definitions from a `Dictionary` produced
by `makeDictionary` have in common.

-}
data Dependencies =
  Dependencies
    { _pattern :: String
    , _rootPath :: FilePath
    } deriving (Show)




-- Type aliases


type Dictionary = [Definition]
type Metadata = Object
type Pattern = String
