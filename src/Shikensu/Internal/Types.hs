{-| Types and path helpers.
    This is re-exported in the main `Shikensu` module.

-}
module Shikensu.Internal.Types where

import Data.Aeson ((.=), toJSON)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.FilePath (joinPath)

import qualified Data.Aeson as Aeson (Object, ToJSON, object)


{-| A file definition, along with some additional properties.
-}
data Definition =
    Definition
        { basename :: Text
        , dirname :: Text
        , extname :: Text
        , pattern :: Text
        , rootDirname :: Text
        , workingDirname :: Text

        -- Additional properties
        , content :: Maybe ByteString
        , metadata :: Metadata
        , parentPath :: Maybe Text
        , pathToRoot :: Text
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


absolutePath :: Definition -> Text
absolutePath def =
    joinPath [rootDirname def, workspacePath def]


localPath :: Definition -> Text
localPath def =
    joinPath [dirname def, basename def <> extname def]


workspacePath :: Definition -> Text
workspacePath def =
    joinPath [workingDirname def, localPath def]
