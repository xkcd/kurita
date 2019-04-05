{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kurita.Bot.Types where

import           Data.Aeson
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Data.Aeson.Types       (Parser)

import           Network.Linklater.Types (APIToken, Channel, Config)

import           Kurita.Prompt.Internal

data BotConfig = BotConfig {
  botConfigToken :: APIToken
, botConfigHook  :: (Channel, Config)
}

data BotCommand =
    AddPrompt Prompt -- Add a prompt
  | SetLive Prompt
  | List Int Int
  | CurrentBattle -- Current battle
  | AddData Text Text
  | RemoveData Text Text
  | SearchPrompts Text
  | TestPrompt Int
  | SetPrompt Int
  | LastRoundWinners
  | NextUp
  deriving (Eq, Ord, Show)

data BotEvent =
    EventMessage Message
  | EventMessageReply MessageReply
  | ReactionAdded Reaction
  | ReactionRemoved Reaction
  deriving (Eq, Ord, Show)

instance FromJSON BotEvent where
  parseJSON =
    withObject "BotEvent" $ \o -> do
      (messageType :: Text) <- o .: "type"
      case messageType of
        "message" -> do
          (st :: Maybe Text) <- o .:? "subtype"
          case st of
            (Just "message_replied") ->
              EventMessageReply <$> parseObject o
            (Just subtype) ->
              fail $ "Message sub_type " <> T.unpack subtype <> " not supported"
            Nothing ->
              EventMessage <$> parseObject o
        "reaction_added" ->
          ReactionAdded <$> parseObject o
        "ReactionRemoved" ->
          ReactionRemoved <$> parseObject o
        _ -> fail $ "Messages of type " <> T.unpack messageType <> " are not supported"

parseObject :: FromJSON a => Object -> Parser a
parseObject = parseJSON . Object

data MessageReply = MessageReply {
  messageReplyTimestamp :: TimeStamp
, messageReplyMessage   :: Message
} deriving (Eq, Ord, Show)

instance FromJSON MessageReply where
  parseJSON =
    withObject "MessageReply" $ \o ->
      MessageReply
        <$> o .: "ts"
        <*> o .: "message"

data Reaction = Reaction {
  reactionValue           :: Text
, reactionTargetTimestamp :: TimeStamp
, reactionUser            :: Text
} deriving (Eq, Ord, Show)

instance FromJSON Reaction where
  parseJSON =
    withObject "Reaction" $ \o -> do
      reaction <- o .: "reaction"
      item <- o .: "item"
      ts <- withObject "Item" (.: "ts") item
      user <- o .: "user"
      pure $ Reaction reaction ts user

data Message = Message {
  messageText      :: Text
, messageUser      :: Text
, messageTimestamp :: TimeStamp
} deriving (Eq, Ord, Show)

instance FromJSON Message where
  parseJSON =
    withObject "Message" $ \o ->
      Message
        <$> o .: "text"
        <*> o .: "user"
        <*> o .: "ts"

newtype TimeStamp = TimeStamp {
  unTimeStamp :: Text
} deriving (Eq, Ord, Show)

instance FromJSON TimeStamp where
  parseJSON j = TimeStamp <$> parseJSON j
