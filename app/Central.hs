{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Time
import qualified Data.ByteString.Char8    as Char8
import           Data.Either              (rights)
import qualified Data.Sequence            as Seq
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Data.Time
import           Kurita.Bot.Types
import           Kurita.Emoji
import           Kurita.Prompt
import           Kurita.Protocol
import           Kurita.Server
import qualified Network.Wai.Handler.Warp as Warp

import           Crypto.MAC.SipHash
import           Network.Linklater.Types

gameTime :: a -> NominalDiffTime
gameTime = const 15

slackChannel :: Channel
slackChannel = Channel "ChannelID" "Channel name"

slackConfig :: Config
slackConfig = Config "Hook url here"

slackToken :: APIToken
slackToken = APIToken "Slack bot token here"

botCfg :: BotConfig
botCfg = BotConfig slackToken (slackChannel, slackConfig)

type HLLSZ = 7

initialComments :: UTCTime -> [(UTCTime, Text)]
initialComments t = [
    (t, "3) If I was creative around this time of night that would be helpful")
  , (t, "2) Some fantastic commentary that's really funny")
  , (t, "1) Starting the match between A and B!")
  ]

introPath :: FilePath
introPath = "intros.txt"

main :: IO ()
main = do
  let Right emoji = mapM uncodes ["U+1F600", "U+1F601", "U+1F606", "U+1F605"
                                 ,"U+1F923", "U+1F602", "U+1F642", "U+1F643"]

  intros <- newTMVarIO Seq.empty
  introResults <- fmap parsePrompt . Text.lines <$> Text.readFile introPath
  let unSipHash (SipHash h) = h
  let
    is = rights introResults
    randomIntro t = do
      let w = unSipHash $ hash (SipKey 4 7) $ Char8.pack (show t)
      let r = (is !! (fromIntegral w `mod` (length is)))
      pure r
  initState <- loadState (\st _ -> KGame st (initialComments st)) (\n _ _ -> KGame (15 `addUTCTime` n) (initialComments n)) emoji::IO (KuritaState HLLSZ Text)
  centralApp <- kuritaCentral "prompts.txt" "terms.txt" (unSipHash . hash (SipKey 4 7)) botCfg id 1 initState
  _ <- forkIO $ Warp.run 8081 $ centralApp
  delay (120::Int)
