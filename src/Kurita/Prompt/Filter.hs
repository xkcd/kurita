{-# LANGUAGE OverloadedStrings #-}
module Kurita.Prompt.Filter where

{-

  The goal of this module is to filter input prompts.

  This is done by using a list of words read in from a file.
  The words are checked first and then the metaphone of a subset
  of the words is checked to see if it's contained in the sentance

  ****None of this was actually used. Commentary wasn't taken from users ****

-} 

import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Encoding

import           Data.ByteString.Char8             (ByteString)

import qualified Data.ByteString.Char8             as Char8

import           Data.Set                          (Set)
import qualified Data.Set                          as Set

import           Language.Phonetic.DoubleMetaphone

-- 3 symbols first competitor, second competitor, and random emoji

-- It might be bad to order the text in a set.. text comparisons :(
data BadEntry = BadEntry {
  badEntryWord      :: Text
, badEntryMetaphone :: Text
} deriving (Eq, Ord, Show)

unreasonableMetaphone :: Set BadEntry -> Text -> Set BadEntry
unreasonableMetaphone =
  unreasonableMatches (\entry input -> badEntryMetaphone entry `Text.isInfixOf` input)

unreasonableWord :: Set BadEntry -> Text -> Set BadEntry
unreasonableWord =
  unreasonableMatches (\entry input -> badEntryWord entry `Text.isInfixOf` input)

-- | Given a check to see if a value is bad, return all the matching bad entries for a single
-- sentence
unreasonableMatches :: (BadEntry -> Text -> Bool) -> Set BadEntry -> Text -> Set BadEntry
unreasonableMatches p badTokens input =
  Set.foldr go Set.empty badTokens
  where
    go entry matches =
      if p entry input
        then Set.insert entry matches
        else matches

-- | Read the word set file (line separated phrases) with the double metaphone applied to each word
readWordSet :: String -> IO (Set BadEntry)
readWordSet wordSetFile = do
  Set.fromList . filterEmptyEntries . fmap makeBadEntry <$> readWordFile
  where
    readWordFile = Char8.split '\n' <$> Char8.readFile wordSetFile
    filterEmptyEntries = filter (not . isEmptyEntry)

makeBadEntry :: ByteString -> BadEntry
makeBadEntry word =
  BadEntry
    (Encoding.decodeUtf8 word)
    (Encoding.decodeUtf8 . fst $ doubleMetaphone word)

isEmptyEntry :: BadEntry -> Bool
isEmptyEntry entry =
  Text.null (badEntryWord entry) || Text.null (badEntryMetaphone entry)
