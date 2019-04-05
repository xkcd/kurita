{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Crypto.MAC.SipHash
import           Data.Bits.Extras (w32)
import           Data.Bytes.Put (runPutS)
import           Data.Bytes.Serial
import           Data.Proxy
import           Data.Text (Text)
import           Data.Time
import           Kurita.Server
import qualified Network.Wai.Handler.Warp as Warp

type HLLSZ = 7

main :: IO ()
main = do
  c <- relayCore (Proxy::Proxy HLLSZ) ("127.0.0.1", 8081, "/")::IO (Core Text)
  Warp.run 8080 $ kuritaRelay
      (\_ -> (w32 . (\(SipHash h) -> h) . hash (SipKey 4 7) . runPutS . serialize) <$> getCurrentTime)
      c
