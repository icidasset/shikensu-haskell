{-| Types and path helpers.
    This is re-exported in the main `Shikensu` module.

-}
module Shikensu.Internal.Types where

import Data.Aeson ((.=), toJSON)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import System.FilePath (joinPath)

import qualified Data.Aeson as Aeson (Object, ToJSON, object)


{-| A file definition, along with some additional properties.
-}
data Definition =
    Definition
        { basename :: String
        , dirname :: FilePath
        , extname :: String
        , pattern :: String
        , rootDirname :: FilePath
        , workingDirname :: FilePath

        -- Additional properties
        , content :: Maybe ByteString
        , metadata :: Metadata
        , parentPath :: Maybe FilePath
        , pathToRoot :: FilePath
        } deriving (Eq, Show)


instance Aeson.ToJSON Definition where
    toJSON def =
        Aeson.object
            [ "basename"        .= basename def
            , "dirname"         .= dirname def
            , "extname"         .= extname def
            , "pattern"         .= pattern def
            , "workingDirname"  .= workingDirname def
            , "parentPath"      .= parentPath def
            , "pathToRoot"      .= pathToRoot def
            ]



-- Type aliases


type Dictionary = [Definition]
type Metadata = Aeson.Object



-- Path functions


absolutePath :: Definition -> String
absolutePath def =
    joinPath [rootDirname def, workspacePath def]


localPath :: Definition -> String
localPath def =
    joinPath [dirname def, basename def <> extname def]


workspacePath :: Definition -> String
workspacePath def =
    joinPath [workingDirname def, localPath def]
