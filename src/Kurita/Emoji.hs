{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Kurita.Emoji (uncodes, uncode) where

import           Data.Char
import qualified Data.Text as T
import qualified Data.Text.Read as TR

uncodes :: T.Text -> Either String T.Text
uncodes = fmap T.pack . mapM uncode  . T.words

uncode :: T.Text -> Either String Char
uncode t =
  case T.stripPrefix "U+" t of
    Nothing -> fail "No \"U+\" prefix"
    Just n  -> fmap (chr . fst) . TR.hexadecimal $ n
