{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Kurita.Bot.Connection where


import           Control.Monad           (forever)
import           Control.Monad.Except
import           Data.Bifunctor

import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Char8   as Char8
import           URI.ByteString

import           Network.Linklater
import           Network.Linklater.Types

import qualified Network.WebSockets      as WS
import qualified Wuss                    as Wuss

import           Kurita.Bot.Types

startBot :: BotConfig -> (BotEvent -> IO ()) -> IO ()
startBot token onCommand = do
  sockUri <- runExceptT fetchRTMUri
  case (\uri -> (getUriPath uri,) <$> uriHost uri) =<< (first show sockUri) of
    Right (path, host) -> do
      Wuss.runSecureClient host 443 path $ app onCommand
      pure ()
    Left err -> error err
  pure ()
  where
    fetchRTMUri :: ExceptT RequestError IO URI
    fetchRTMUri = startRTM $ botConfigToken token
    uriHost :: URI -> Either String String
    uriHost =
      maybe (Left "Unable to find uri authority") (Right . Char8.unpack)
        . fmap (hostBS . authorityHost)
        . uriAuthority
    getUriPath :: URI -> String
    getUriPath =
      Char8.unpack . uriPath

app :: (BotEvent -> IO ()) -> WS.ClientApp ()
app onCommand conn = do
  putStrLn "Slack bot connected!"
  forever $ do
    msg <- WS.receiveData conn
    let
      botMessage :: Either String BotEvent
      botMessage = Aeson.eitherDecode' msg
    either
      (const $ pure ())
      onCommand
      botMessage
