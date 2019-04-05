module Main where

import qualified Control.Concurrent.Async as Async
import           Control.Monad
import           Control.Monad.Loops
import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import           Data.List (sort)
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Data.Time
import           Kurita.Protocol
import qualified Network.WebSockets as WS
import           System.Environment (getArgs)
import           System.Exit (exitSuccess)

vote :: (TDown Text -> IO ()) -> (String, Int, String) -> NominalDiffTime -> IO ()
vote onDown (host, port, path) rate = do
  initTime <- getCurrentTime
  WS.runClient host port path $ \conn -> (`iterateM_` initTime) $ \lastVoteTime -> do
    Just td <- (JS.decode' . BL.fromStrict . TE.encodeUtf8) <$> WS.receiveData conn
    onDown td
    now <- getCurrentTime
    case td of
      BattleStart (Bracket {_bCurrent=Just c}) -> when ((rate `addUTCTime` lastVoteTime) <= now) $ do
                  WS.sendTextData conn . TE.decodeUtf8 . BL.toStrict . JS.encode .
                    Vote . head . sort . map snd . toList . _gameSorted $ c
      BattleStart (Bracket {_bCurrent=Nothing}) -> exitSuccess
      ScoreUpdate scs -> when ((rate `addUTCTime` lastVoteTime) <= now) $ do
                  WS.sendTextData conn . TE.decodeUtf8 . BL.toStrict . JS.encode .
                    Vote . head . sort . map fst $ scs
--      _ -> putStrLn "Unknown TDown"
    pure now

main :: IO ()
main = do
  let target = ("127.0.0.1", 8080, "/")
  let rate = 1.0
  args <- getArgs
  case args of
    [] -> do
         vote print target rate
    [n] -> do
         Async.replicateConcurrently_ (read n) $ vote (const $ pure ()) target rate
    _ -> putStrLn "Unknow args."
