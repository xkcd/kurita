{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Concurrent
import           Control.Time
import           Crypto.MAC.SipHash
import           Data.Bits.Extras (w32)
import           Data.Bytes.Put (runPutS)
import           Data.Bytes.Serial
import           Data.Time
import           Data.Text (Text)
import           Kurita.Emoji
import           Kurita.Protocol
import           Kurita.Server
import qualified Network.Wai.Handler.Warp as Warp


gameTime :: a -> NominalDiffTime
gameTime = const 15

type HLLSZ = 7

main :: IO ()
main = do
  let Right emoji = mapM uncodes ["U+1F600", "U+1F601", "U+1F606", "U+1F605"
                                 ,"U+1F923", "U+1F602", "U+1F642", "U+1F643"]
  initState <- loadState (\st _ ->  KGame st []) (\n _ _ -> KGame (15 `addUTCTime` n) []) emoji::IO (KuritaState HLLSZ Text)
  c <- localCore 1 initState
  _ <- forkIO $ Warp.run 8080 $ kuritaRelay
      (\_ -> (w32 . (\(SipHash h) -> h) . hash (SipKey 4 7) . runPutS . serialize) <$> getCurrentTime)
      c
  delay (120::Int)
