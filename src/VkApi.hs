{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VkApi where

import           Data.Text

import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy.Char8  as BSLC8
import qualified Data.ByteString.Char8  as BSC8

import           GHC.Generics
import qualified Data.Aeson             as Aeson
import           Data.Aeson             ((.:))
import           Data.Aeson.Encode.Pretty (encodePretty)



-- For some reason Haskell doesn't support 'type' as record field name so it's replaced with 'rtype'.
typeFieldModifier :: String -> String
typeFieldModifier "rtype" = "type"
typeFieldModifier x = x

vkAesonDefaultOptions = Aeson.defaultOptions{ Aeson.fieldLabelModifier = typeFieldModifier }
--



-- Photo
data Photo = Photo {
    id :: Integer,
    album_id :: Integer,
    owner_id :: Integer,
    user_id :: Maybe Integer,
    text :: String,
    date :: Integer,
    width :: Integer,
    height :: Integer
} deriving (Show, Generic)

instance Aeson.FromJSON Photo where
    parseJSON = Aeson.genericParseJSON vkAesonDefaultOptions

instance Aeson.ToJSON Photo where
    toJSON = Aeson.genericToJSON vkAesonDefaultOptions
--

-- Document
data Document = Document {
    id :: Integer,
    owner_id :: Integer,
    title :: Integer,
    size :: Integer,
    ext :: String,
    url :: String,
    date :: Integer,
    rtype :: Integer
} deriving (Show, Generic)

instance Aeson.FromJSON Document where
    parseJSON = Aeson.genericParseJSON vkAesonDefaultOptions

instance Aeson.ToJSON Document where
    toJSON = Aeson.genericToJSON vkAesonDefaultOptions
--

-- -- Unknown
-- data Unknown = Unknown {
--     id :: Integer
-- } deriving (Show, Generic)

-- instance Aeson.FromJSON Unknown where
--     parseJSON = Aeson.genericParseJSON vkAesonDefaultOptions

-- instance Aeson.ToJSON Unknown where
--     toJSON = Aeson.genericToJSON vkAesonDefaultOptions
-- --

-- Attachment
data Attachment = AttachmentPhoto Photo | AttachmentDocument Document | AttachmentUnknown deriving (Show, Generic)

instance Aeson.FromJSON Attachment where
    parseJSON = Aeson.withObject "Attachment" $ \obj ->
        (obj .: "type") >>= \ (t :: Text) ->
            case t of
                "photo" -> fmap AttachmentPhoto (obj .: "photo")
                "doc" -> fmap AttachmentDocument (obj .: "doc")
                _ -> return AttachmentUnknown

instance Aeson.ToJSON Attachment where
    toJSON (AttachmentPhoto photo) = Aeson.toJSON photo
    toJSON (AttachmentDocument doc) = Aeson.toJSON doc
    toJSON AttachmentUnknown = ""
--

-- WallReplyEvent
data WallReplyEvent = WallReplyEvent {
    id :: Integer,
    from_id :: Integer,
    date :: Integer,
    text :: String,
    attachments :: Maybe [Attachment]
} deriving (Show, Generic, Aeson.FromJSON, Aeson.ToJSON)
--

-- CallbackEvent
data CallbackEvent = CallbackWallReplyEvent WallReplyEvent | CallbackConfirmationEvent | CallbackUnknownEvent deriving (Show, Generic)

instance Aeson.FromJSON CallbackEvent where
    parseJSON = Aeson.withObject "CallbackEvent" $ \obj ->
        (obj .: "type") >>= \ (t :: Text) ->
            if t `elem` ["wall_reply_new", "wall_reply_edit", "wall_reply_restore", 
                         "video_comment_new", "video_comment_edit", "video_comment_restore",
                         "photo_comment_new", "photo_comment_edit", "photo_comment_restore"]
            then fmap CallbackWallReplyEvent (obj .: "object")
            else if t == "confirmation" then return CallbackConfirmationEvent
            else return CallbackUnknownEvent
--

-- -- CallbackRequestScheme
-- data CallbackRequestScheme = CallbackRequestScheme {
--     rtype :: String,
--     object :: Aeson.Object,
--     group_id :: Integer
-- } deriving (Show, Generic)

-- instance Aeson.FromJSON CallbackRequestScheme where
--     parseJSON = Aeson.genericParseJSON vkAesonDefaultOptions

-- instance Aeson.ToJSON CallbackRequestScheme where
--     toJSON = Aeson.genericToJSON vkAesonDefaultOptions
-- --
