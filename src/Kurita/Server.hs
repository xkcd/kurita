{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Kurita.Server where

import           BroadcastChan
import           Control.Concurrent
import qualified Control.Concurrent.Async as Async
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Control.Time
import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JS
import           Data.Approximate (estimate)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import qualified Data.HyperLogLog as HLL
import           Data.HyperLogLog.Type (HyperLogLog(HyperLogLog, runHyperLogLog))
import           Data.IORef
import           Data.Int
import           Data.List (sort)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Reflection (Reifies)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
import           Data.Tuple (swap)
import           Data.Word
import           Kurita.Protocol
import           Kurita.Bot
import           Kurita.Prompt.Internal
import           Kurita.Bot.Connection
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.WebSockets as WAI
import qualified Network.WebSockets as WS
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)

import           Network.Linklater
import           Network.Linklater.Types
import           Kurita.Bot.Types
import           Data.ByteString (ByteString)

saveFile :: String
saveFile = "state.json"

data KuritaState (p :: k) c
 = KState
   { _ksBracket  :: Bracket c (HyperLogLog p) KuritaGame
     -- ^ The bracket in play.
   , _ksSingleMeta :: UTCTime -> [c] -> KuritaGame
   , _ksGameMeta :: UTCTime -> Int -> [c] -> KuritaGame
   }

makeClassy ''KuritaState

loadState :: (Reifies p Integer, Ord c, FromJSON c)
          => (UTCTime -> [c] -> KuritaGame) -> (UTCTime -> Int -> [c] -> KuritaGame)
          -> [c] -> IO (KuritaState p c)
loadState sm fm csBase =
  handle
    (\(_::SomeException) ->
      getCurrentTime >>= \now -> pure (KState (seedBracket (sm now) (fm now) csBase) sm fm)) $ do
    Just b <- JS.decodeFileStrict' saveFile
    pure $ KState (changeVoteType HyperLogLog b) sm fm

data ClientData c
 = CData
   { _cdBracket :: Bracket c Int64 ClientGame
   , _cdBattleMsg :: {-# UNPACK #-} !Text
   , _cdScoreMsg :: (Maybe (([c], [Text]), Text))
   }
 deriving (Show, Eq, Ord)

data Core c
 = Core
   { coreWaitUpdate :: IO (ClientData c)
   , coreCurrent :: IO (ClientData c)
   , coreVote :: c -> Word32 -> IO ()
   }

bracket2ClientData :: (Ord c, ToJSON c) => Bracket c Int64 KuritaGame -> ClientData c
bracket2ClientData b' =
  let b = fmap k2cGame b'
  in CData b
           (TE.decodeUtf8 . BL.toStrict . JS.encode . BattleStart $ b)
           ((\cur -> ((cur^.gameSorted.to (sort . map snd . toList), _cgCommentary $ cur ^. gameExtra)
                    ,TE.decodeUtf8 . BL.toStrict . JS.encode .
                     ScoreUpdate . map swap . toList . _gameSorted $ cur)
            ) <$> b^.bCurrent)

ksToInf :: (Ord c, Reifies p Integer) => KuritaState p c -> Bracket c Int64 KuritaGame
ksToInf ks =
    ks^.ksBracket.to (changeVoteType (\s -> HLL.size s ^. estimate))

relayCore :: forall c p m
          . (Ord c, ToJSON c, FromJSON c, Reifies p Integer, MonadIO m)
          => Proxy p -> (String, Int, String) -> m (Core c)
relayCore _ (host, port, path) = liftIO $ do
  Just bInit <- retrying 10 $ WS.runClient host port path $ \conn -> do
    JS.decode' <$> WS.receiveData conn
  putStrLn "Got Initial"
  upChan <- newBroadcastChan
  curRef <- newIORef $ bracket2ClientData bInit
  votesRef <- newIORef $ mkVoteAgg bInit
  let cc::IO (ClientData c) = liftIO (readIORef curRef)
  let cu::IO (ClientData c) = liftIO $ maybe cc pure =<< readBChan =<< newBChanListener upChan
  let cv c v = liftIO $ do
                 atomicModifyIORef' votesRef $ \case
                     Nothing -> (Nothing, ())
                     Just cvm -> (Just . Map.adjust (HLL.insertHash v) c $ cvm, ())
  void . forkIO $ updateCentral upChan curRef votesRef
  pure $ Core cu cc cv
  where
    retrying :: Int -> IO a -> IO a
    retrying 0 act = act
    retrying c act = handle (\(_::SomeException) -> delay (1::Double) >> retrying (c-1) act) act

    updateCentral upChan curRef votesRef = do
      putStrLn "Updating Central"
      handle (\(e::SomeException) -> print e >> delay (0.1::Double) >> updateCentral upChan curRef votesRef) . forever $ do
        WS.runClient host port path $ \conn -> forever $ do
          Just b <- JS.decode' <$> WS.receiveData conn
          mOldVotes <- atomicModifyIORef' votesRef $ \vs -> (mkVoteAgg b, vs)
          let cd = bracket2ClientData b
          atomicWriteIORef curRef cd
          void $ writeBChan upChan cd
          case mOldVotes of
            Nothing -> pure ()
            Just oldVotes -> WS.sendBinaryData conn . JS.encode . Votes . Map.toList $ oldVotes
    mkVoteAgg =
      fmap (Map.fromList . map ((, mempty::HyperLogLog p) . snd) . toList . _gameSorted) . _bCurrent

localCore :: forall c p m
          . (Ord c, ToJSON c, Reifies p Integer, MonadIO m)
          => NominalDiffTime -> KuritaState p c -> m (Core c)
localCore rt ksInit = liftIO $ do
  upChan <- newBroadcastChan
  curRef <- newIORef ksInit
  let cc = (bracket2ClientData . ksToInf) <$> liftIO (readIORef curRef)
  let cu = liftIO $ maybe cc pure =<< readBChan =<< newBChanListener upChan
  let cv c v = liftIO $ do
                 atomicModifyIORef' curRef (\cs ->
                     let r = cs&ksBracket %~ (addScore c v)
                     in r `seq` (r, r)) >>= void . writeBChan upChan . bracket2ClientData . ksToInf
  void . forkIO $ whileJust_ ((^?ksBracket.bCurrent._Just.gameExtra.kgEndTime) <$> readIORef curRef) $ \w -> do
    bfdly <- getCurrentTime
    delayTill $ min (rt `addUTCTime` bfdly) w
    now <- getCurrentTime
    when (now >= w) $ do
      nks <- atomicModifyIORef' curRef $ \(KState b sm fm) ->
        let nb = finishGame (sm now) (fm now) b
            ks = KState nb sm fm
        in ks `seq` (ks, ks)
      atomicWriteFile saveFile . JS.encode . changeVoteType runHyperLogLog . _ksBracket $ nks
    -- We read from the IORef here just to have the most up-to-date though its not too important
    -- because the game can't have changed since  we handle that, but the votes might be outdated.
    writeBChan upChan . bracket2ClientData . ksToInf =<< readIORef curRef
  pure $ Core cu cc cv

kuritaRelay :: (Eq c,ToJSON c, FromJSON c) => (WAI.Request -> IO Word32) -> Core c -> WAI.Application
kuritaRelay mkVote core rq respond =
    maybe backupApp respond $ WAI.websocketsApp wsOptions wsApp rq
  where
    wsOptions = WS.defaultConnectionOptions { WS.connectionCompressionOptions=WS.PermessageDeflateCompression WS.defaultPermessageDeflate}

    backupApp :: IO WAI.ResponseReceived
    backupApp =
      coreCurrent core >>= respond . WAI.responseLBS HTTP.status200 [] . BL.fromStrict . TE.encodeUtf8 . _cdBattleMsg

    wsApp :: WS.ServerApp
    wsApp wp = do
      conn <- WS.acceptRequest wp
      initBattle <- coreCurrent core
      WS.sendTextData conn . _cdBattleMsg $ initBattle
      Async.race_ (recvWS conn) (sendWS conn initBattle)

    recvWS conn = forever $ do
      Just (Vote c) <- JS.decode' <$> WS.receiveData conn
      coreVote core c =<< mkVote rq
    sendWS conn = iterateM_ $ \lb -> do
      nb <- coreWaitUpdate core
      WS.sendTextData conn $ case (_cdScoreMsg lb, _cdScoreMsg nb) of
                               (Just (oldCs, _), Just (newCs, smsg)) | oldCs==newCs -> smsg
                               _ -> _cdBattleMsg nb
      pure nb

kuritaCentral :: forall c p
          . (Ord c, ToJSON c, FromJSON c, Reifies p Integer)
          => FilePath
          -> FilePath
          -> (ByteString -> Word64)
          -> BotConfig
          -> (c -> Text)
          -> NominalDiffTime
          -> KuritaState p c -> IO WAI.Application
kuritaCentral promptFile termsFile hash botCfg toText rt ksInit = do
  upChan <- newBroadcastChan
  curRef <- newIORef ksInit
  let cc    = liftIO $ ksToInf <$> readIORef curRef
  let cu    = liftIO $ maybe cc pure =<< readBChan =<< newBChanListener upChan
  let cv vs = liftIO $ do
                atomicModifyIORef' curRef (\cs ->
                  let r = cs&ksBracket %~ (addScores vs)
                  in r `seq` (r, ()))
  let addCommentary c = do
        currentTime <- getCurrentTime
        atomicModifyIORef' curRef (\cs ->
          let r = cs & ksBracket . bCurrent . _Just . gameExtra . kgCommentary %~ ((currentTime, c):)
          in r `seq` (r, ()))

  kbot <- loadKBotConfig promptFile termsFile hash botCfg cc addCommentary
  void . forkIO $ forever $ do
    startBot botCfg $ runBotEvent toText kbot
    putStrLn "Slack bot disconnected... attempting to reconnect in 1 minute"
    delay $ (60 :: Int) * 10 ^ (6 :: Int)

  void . forkIO $ whileJust_ ((^?ksBracket.bCurrent._Just.gameExtra.kgEndTime) <$> readIORef curRef) $ \w -> do
    bfdly <- getCurrentTime
    delayTill $ min (rt `addUTCTime` bfdly) w
    now <- getCurrentTime
    when (now >= w) $ do
      nk <- atomicModifyIORef' curRef $ \(KState b sm fm) ->
        let nb = finishGame (sm now) (fm now) b
            ks = KState nb sm fm
        in ks `seq` (ks, (changeVoteType runHyperLogLog $ _ksBracket ks))
      atomicWriteFile saveFile $ JS.encode nk
      void . forkIO $
        maybe (pure ()) (kuritaSend (kbotSlackConfig kbot)) $
          nk^?bCurrent._Just.gameSorted.to (T.intercalate " vs. " . map (toText . snd) . toList)
    writeBChan upChan  =<< cc

  pure $ \rq respond -> do

      let backupApp = cc >>= respond . WAI.responseLBS HTTP.status200 [] . JS.encode

      let sendBattle conn = WS.sendTextData conn . TE.decodeUtf8 . BL.toStrict . JS.encode

      let recvWS conn = forever $ do
            Just (Votes vs) <- JS.decode' <$> WS.receiveData conn
            cv vs
      let sendWS conn = forever $ do
            cu >>= sendBattle conn

      let wsApp wp = do
            conn <- WS.acceptRequest wp
            cc >>= sendBattle conn
            Async.race_ (recvWS conn) (sendWS conn)

      maybe backupApp respond $ WAI.websocketsApp wsOptions wsApp rq
  where
    wsOptions = WS.defaultConnectionOptions
