{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Kurita.Bot where

import qualified Data.Aeson                 as Aeson
import           Text.Megaparsec
import           Text.Megaparsec.Char       (string)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Control.Monad.Except
import           Data.Foldable              (toList)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.SortedList            as SL
import           Data.Text                  (Text)

import           Data.Bifunctor
import qualified Data.ByteString.Lazy       as BSL
import           Data.Either
import           Data.Int
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text

import           Control.Concurrent.STM

import           Kurita.Bot.Types
import           Kurita.Prompt
import           Kurita.Prompt.Internal
import           Kurita.Protocol

import           Network.Linklater

import           Data.ByteString (ByteString)
import           Data.Word

data KuritaBot c = KuritaBot {
  kbotAddPrompt   :: Prompt -> IO Int
, kbotPrompts     :: IO (Seq Prompt)
, kbotAddTerm     :: Text -> Text -> IO ()
, kbotTerms       :: IO (Map Text (Set Text))
, kbotBracket     :: IO (Bracket c Int64 KuritaGame)
, kbotSetPrompt   :: Text -> IO ()
, kbotSlackConfig :: BotConfig
, kbotHash        :: ByteString -> Word64
}

loadKBotConfig :: FilePath -> FilePath -> (ByteString -> Word64) -> BotConfig -> IO (Bracket c Int64 KuritaGame) -> (Text -> IO ()) -> IO (KuritaBot c)
loadKBotConfig promptFile termsFile hash botCfg getBracket setPrompt = do
  promptResults <- fmap parsePrompt . Text.lines <$> Text.readFile promptFile
  promptsVar <- newTMVarIO $ Seq.fromList $ rights promptResults

  let
    addPrompt p = do
      prompts <- atomically $ takeTMVar promptsVar
      Text.appendFile promptFile $ (displayPrompt p) <> "\n"
      let
        prompts' = prompts Seq.|> p
      atomically $ putTMVar promptsVar $ prompts'
      pure $ Seq.length prompts' - 1
    getPrompts = atomically $ readTMVar promptsVar

  termResults <- fmap parseTerm . Text.lines <$> Text.readFile termsFile
  termsVar <- newTMVarIO $ foldr addToTermMap Map.empty $ rights termResults

  let
    addTerm k v = do
      terms <- atomically $ takeTMVar termsVar
      Text.appendFile termsFile $ (displayTerm k v) <> "\n"
      atomically $ putTMVar termsVar $ addToTermMap (k, v) terms
    getTerms = atomically $ readTMVar termsVar

  pure $ KuritaBot {
    kbotAddPrompt = addPrompt
  , kbotPrompts = getPrompts
  , kbotAddTerm = addTerm
  , kbotTerms = getTerms
  , kbotBracket = getBracket
  , kbotSetPrompt = setPrompt
  , kbotSlackConfig = botCfg
  , kbotHash = hash
  }

addToTermMap :: (Ord k, Ord t) => (k, t) -> Map k (Set t) -> Map k (Set t)
addToTermMap (key, val) terms =
  Map.alter (Just . maybe (Set.singleton val) (Set.insert val)) key terms

parseTerm :: Text -> Either Text (Text, Text)
parseTerm = first Text.pack . Aeson.eitherDecode . BSL.fromStrict . Text.encodeUtf8

displayTerm :: Text -> Text -> Text
displayTerm k v = Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode (k, v)

runBotEvent :: Ord c =>  (c -> Text) -> KuritaBot c -> BotEvent -> IO ()
runBotEvent toText kbot (EventMessage message) =
  case parseKBotMessage $ messageText message of
    Right cmd -> runBotCommand toText kbot cmd
    Left pErr  -> 
      when ("!" `Text.isPrefixOf` messageText message)
        $ kuritaSend (kbotSlackConfig kbot) $ "Uh oh that's a parse error: " <> Text.pack (show pErr)
runBotEvent _toText _kbot (EventMessageReply _reply) = pure ()
runBotEvent _toText _kbot (ReactionAdded _reaction) = pure ()
runBotEvent _toText _kbot (ReactionRemoved _reaction) = pure ()

runBotCommand :: Ord c => (c -> Text) -> KuritaBot c -> BotCommand -> IO ()
runBotCommand _toText kbot (AddPrompt prompt) = do
  promptIndex <- kbotAddPrompt kbot prompt
  kuritaSend (kbotSlackConfig kbot) $
    "New prompt ID: "
      <> (Text.pack $ show promptIndex)
      <> " added - \"" <> displayPrompt prompt <> "\""
runBotCommand _toText kbot (List i j) = do
  prompts <- kbotPrompts kbot
  let
    indexSeq = Seq.fromList $ [0..Seq.length prompts]
    promptsWithIndex = Seq.zip indexSeq $ displayPrompt <$> prompts
    prompts' = Seq.drop i $ Seq.take j promptsWithIndex
  kuritaSend (kbotSlackConfig kbot) $
    Text.intercalate "\n" $ (\(index,v) -> Text.pack (show index) <> " - " <> v) <$> toList prompts'
runBotCommand toText kbot (CurrentBattle) = do
  current <- kbotBracket kbot
  case _bCurrent current of
    Nothing -> kuritaSend (kbotSlackConfig kbot) "I wasn't able to find a running duel for some reason"
    Just game ->
      kuritaSend (kbotSlackConfig kbot) $ Text.concat [
          Text.intercalate " vs. " (displayVotes <$> SL.fromSortedList (_gameSorted game)), "\n"
        , Text.intercalate "\n" (displayCommentary <$> (_kgCommentary $ _gameExtra game))
        ]
  where
    displayVotes (voteCount, competitor) =
      toText competitor <> " (" <> Text.pack (show voteCount) <> ")"
    displayCommentary (time, prompt) =
      Text.pack (show time) <> " - " <> prompt
runBotCommand _toText kbot (AddData var val) = do
  kbotAddTerm kbot var val
  terms <- kbotTerms kbot
  maybe
    (kuritaSend (kbotSlackConfig kbot) $ "There was an error inserting " <> var <> " -> " <> val)
    (\vals -> kuritaSend (kbotSlackConfig kbot) $ var <> " now contains: " <> (Text.pack $ show $ Set.toList vals))
    $ Map.lookup var terms
runBotCommand _toText kbot (RemoveData _var _val) =
  kuritaSend (kbotSlackConfig kbot) "I'm sorry dave I can't do that"
runBotCommand _toText kbot (SearchPrompts term) = do
  prompts <- kbotPrompts kbot
  let
    indexSeq = Seq.fromList $ [0..Seq.length prompts]
    promptsWithIndex = Seq.zip indexSeq $ displayPrompt <$> prompts
    filtered = Seq.take 25 $ Seq.filter (\(_, v) -> term `Text.isInfixOf` v) promptsWithIndex
  kuritaSend (kbotSlackConfig kbot) $ Text.intercalate "\n" $ (\(i,v) ->  (Text.pack (show i) <> " - " <> v)) <$> toList filtered
runBotCommand toText kbot (TestPrompt i)  = do
  prompts <- kbotPrompts kbot
  meta <- kbotTerms kbot
  br <- kbotBracket kbot
  case renderPrompt toText (kbotHash kbot) meta br <$> Seq.lookup i prompts of
    Nothing -> kuritaSend (kbotSlackConfig kbot) $ "Unable to find prompt at index " <> Text.pack (show i)
    Just (Left err) -> kuritaSend (kbotSlackConfig kbot) $ "Error rendering prompt: " <> Text.pack err
    Just (Right prompt) -> kuritaSend (kbotSlackConfig kbot) prompt
runBotCommand toText kbot (SetPrompt i)  = do
  prompts <- kbotPrompts kbot
  meta <- kbotTerms kbot
  br <- kbotBracket kbot
  case renderPrompt toText (kbotHash kbot) meta br <$> Seq.lookup i prompts of
    Nothing -> kuritaSend (kbotSlackConfig kbot) $ "Unable to find prompt at index " <> Text.pack (show i)
    Just (Left err) -> kuritaSend (kbotSlackConfig kbot) $ "Error rendering prompt: " <> Text.pack err
    Just (Right prompt) -> do
      kbotSetPrompt kbot prompt
      kuritaSend (kbotSlackConfig kbot) $ "Prompt set to \"" <> prompt <> "\""
runBotCommand toText kbot (SetLive pr)  = do
  meta <- kbotTerms kbot
  br <- kbotBracket kbot
  case renderPrompt toText (kbotHash kbot) meta br pr of
    Left err -> kuritaSend (kbotSlackConfig kbot) $ "Error rendering prompt: " <> Text.pack err
    Right prompt -> do
      kbotSetPrompt kbot prompt
      kuritaSend (kbotSlackConfig kbot) $ "Prompt set to \"" <> prompt <> "\""
runBotCommand toText kbot NextUp  = do
  br <- kbotBracket kbot
  case _bUpcoming br of
    [a, b]:_ -> do
      kuritaSend (kbotSlackConfig kbot) $ "Up next, "<>(toText a)<>" vs. "<>(toText b)<>", I can't wait!"
    _ -> pure ()
runBotCommand toText kbot LastRoundWinners  = do
  br <- kbotBracket kbot
  case (map (map (toText . last . map snd . toList . _gameSorted)) . _bPlayed $ br) of
    _:lr:_ -> kuritaSend (kbotSlackConfig kbot) $ "The winners of the last round were: " <>
             (Text.intercalate ", " $ map (Text.pack . show) lr)
    _ -> pure ()

parseKBotMessage :: Text -> Either Text BotCommand
parseKBotMessage t =
  first (Text.pack . show) $ runParser parsers "" t
  where
    parsers =
          parseAdd
      <|> parseLive
      <|> parseList
      <|> parseCurrentBattle
      <|> parseAddData
      <|> parseSearch
      <|> parseTestPrompt
      <|> parseSet
      <|> parseNext
      <|> parseWinners
    parseAdd = do
      _ <- try $ string "!add "
      prompt <- promptParser
      pure $ AddPrompt prompt
    parseLive = do
      _ <- try $ string "!live "
      prompt <- promptParser
      pure $ SetLive prompt
    parseList = do
      _ <- try $ string "!list "
      i <- decimal
      _ <- string " "
      j <- decimal
      pure $ List i j
    parseCurrentBattle = do
      _ <- try $ string "!battle"
      pure CurrentBattle
    parseAddData = do
      _ <- try $ string "!meta "
      var <- takeWhile1P Nothing (\c -> c /= ' ')
      _ <- string " -> "
      val <- takeRest
      pure $ AddData var val
    parseSearch = do
      _ <- try $ string "!search "
      val <- takeRest
      pure $ SearchPrompts val
    parseTestPrompt = do
      _ <- try $ string "!test "
      i <- decimal
      pure $ TestPrompt i
    parseSet = do
      _ <- try $ string "!set "
      i <- decimal
      pure $ SetPrompt i
    parseNext = do
      _ <- try $ string "!next"
      pure $ NextUp
    parseWinners = do
      _ <- try $ string "!lastwinners"
      pure $ LastRoundWinners

kuritaSend :: BotConfig -> Text -> IO ()
kuritaSend botCfg msg = do
  err <- runExceptT $ say (SimpleMessage (EmojiIcon ":robot:") "kurita_bot" ch msg) cfg
  case err of
    Left e -> print e
    _      -> pure ()
  where
    (ch, cfg) = botConfigHook botCfg
