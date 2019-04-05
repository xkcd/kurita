{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kurita.Prompt where

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Control.Lens
import           Data.Bifunctor
import           Data.Int
import           Data.Maybe (catMaybes)
import qualified Data.SortedList        as SL
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Void

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Kurita.Prompt.Internal
import           Kurita.Protocol

import           Data.ByteString (ByteString)
import           Data.Word

-- given two emojis add a comment
-- Seed random numbers by match number

type Parser = Parsec Void Text

renderIntro :: Ord c => (c -> Text) -> [c] -> Prompt -> Either String Text
renderIntro toText competitors (Prompt ps) =
  fmap Text.concat <$> sequence $ renderPromptEntry <$> ps
  where
    renderPromptEntry :: PromptEntry -> Either String Text
    renderPromptEntry (PromptText t)      = Right t
    renderPromptEntry (PromptEntrySlot s) = renderSlot s
    renderSlot PromptFirst =
      maybe (Left "Unable to fill out {first} with current bracket") Right $ do
        (one, _two) <- sortedPair $ SL.toSortedList competitors
        pure $ toText $ one
    renderSlot PromptSecond =
      maybe (Left "Unable to fill out {second} with current bracket") Right $ do
        (_one, two) <- sortedPair $ SL.toSortedList competitors
        pure $ toText $ two
    renderSlot _ = Left "Unavailable in initial commentary"

-- | Fill a prompt if possible, otherwise return the filled prompt and leftover emojis
renderPrompt :: forall c. Ord c => (c -> Text) -> (ByteString -> Word64) -> Map Text (Set Text) -> Bracket c Int64 KuritaGame -> Prompt -> Either String Text
renderPrompt toText hash meta br (Prompt ps) =
  fmap Text.concat <$> sequence $ renderPromptEntry <$> ps
  where
    renderPromptEntry :: PromptEntry -> Either String Text
    renderPromptEntry (PromptText t)      = Right t
    renderPromptEntry (PromptEntrySlot s) = renderSlot s
    renderSlot :: PromptSlot -> Either String Text
    renderSlot PromptFirst =
      maybe (Left "Unable to fill out {first} with current bracket") Right $ do
        players <- br ^? bCurrent . _Just . gameSorted
        (one, _two) <- sortedPair players
        pure $ toText $ snd one
    renderSlot PromptSecond =
      maybe (Left "Unable to fill out {second} with current bracket") Right $ do
        players <- br ^? bCurrent . _Just . gameSorted
        (_one, two) <- sortedPair players
        pure $ toText $ snd two
    renderSlot PromptWinner =
      maybe (Left "Unable to fill out {first} with current bracket") Right $ do
        players <- br ^? bCurrent . _Just . gameSorted
        (_first, players') <- SL.uncons players
        (secondPlaer, _) <- SL.uncons players'
        pure $ toText $ snd secondPlaer
    renderSlot PromptLoser =
      maybe (Left "Unable to fill out {second} with current bracket") Right $ do
        players <- br ^? bCurrent . _Just . gameSorted
        (firstPlayer, _) <- SL.uncons players
        pure $ toText $ snd firstPlayer
    renderSlot PromptRandomLost =
      maybe (Left "Unable to fill out random-lost with current bracket") Right $ do
        pure $ toText $ randomElem id . map (snd.snd) . catMaybes . map (sortedPair . _gameSorted) . mconcat . _bPlayed $ br
    renderSlot PromptRandomWon =
      maybe (Left "Unable to fill out random-alive with current bracket") Right $ do
        pure $ toText . randomElem id . mconcat ._bUpcoming $ br
    renderSlot (PromptVar var) =
      maybe
        (Left $ "Unable to lookup {" <> Text.unpack var <> "} in the meta map")
        Right $ randomElem id . Set.toList <$> Map.lookup var meta
    randomSeed =
      hash
        $ Text.encodeUtf8
        $ maybe Text.empty (Text.concat . fmap (toText . snd) . SL.fromSortedList) currentPlayers
    randomElem f xs =
      xs !! ((fromIntegral $ f randomSeed) `mod` length xs)

    currentPlayers = br ^? bCurrent . _Just . gameSorted

sortedPair :: Ord a => SL.SortedList a -> Maybe (a, a)
sortedPair players =  do
  (a, players') <- SL.uncons players
  (b, _) <- SL.uncons players'
  pure $
    if (a > b)
    then (b, a)
    else (a, b)

displayPrompt :: Prompt -> Text
displayPrompt (Prompt ps) =
  Text.concat $ displayPromptEntry <$> ps
  where
    displayPromptEntry :: PromptEntry -> Text
    displayPromptEntry (PromptText t)      = t
    displayPromptEntry (PromptEntrySlot s) = displayPromptSlot s
    displayPromptSlot :: PromptSlot -> Text
    displayPromptSlot PromptFirst      = "{one}"
    displayPromptSlot PromptSecond     = "{two}"
    displayPromptSlot PromptWinner     = "{first}"
    displayPromptSlot PromptLoser      = "{second}"
    displayPromptSlot PromptRandomLost = "{random-lost}"
    displayPromptSlot PromptRandomWon  = "{random-alive}"
    displayPromptSlot (PromptVar var)  = "{" <> var <> "}"

parsePrompt :: Text -> Either Text Prompt
parsePrompt promptText =
  first (Text.pack . show) $ runParser promptParser "Prompt" promptText

promptParser :: Parser Prompt
promptParser =
  Prompt <$> manyTill (try (PromptText <$> textSegment) <|> PromptEntrySlot <$> promptSlot) eof

textSegment :: Parser Text
textSegment =
  takeWhile1P Nothing (\i -> i /= '{' && i /= '}')

promptSlot :: Parser PromptSlot
promptSlot = do
  _ <- char '{'
  var <- textSegment
  _ <- char '}'
  pure $
    case Text.unpack var of
      "one"          -> PromptFirst
      "two"          -> PromptSecond
      "first"        -> PromptWinner
      "second"       -> PromptLoser
      "random-lost"  -> PromptRandomLost
      "random-alive" -> PromptRandomWon
      "onetwo"       -> PromptFirst -- Left in for compatibility
      "twoone"       -> PromptSecond -- Left in for compatibility
      _              -> PromptVar var
