{-# LANGUAGE OverloadedStrings #-}
module Kurita.Prompt.Internal where

import           Data.Text            (Text)

data PromptSlot =
    PromptWinner
  | PromptLoser
  | PromptFirst
  | PromptSecond
  | PromptRandomLost
  | PromptRandomWon
  | PromptVar Text
  deriving (Eq, Ord, Show)

data PromptEntry =
    PromptText Text
  | PromptEntrySlot PromptSlot
  deriving (Eq, Ord, Show)

newtype Prompt = Prompt {
  unPrompt :: [PromptEntry]
} deriving (Eq, Ord, Show)
