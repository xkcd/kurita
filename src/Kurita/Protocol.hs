{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Kurita.Protocol where

import           Control.Lens
import           Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), (.:))
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import           Data.Time
import           Data.Foldable
import           Data.Int
import qualified Data.HyperLogLog as HLL
import           Data.HyperLogLog.Type (HyperLogLog(HyperLogLog, runHyperLogLog))
import           Data.List (partition)
import           Data.List.Split (chunksOf)
import           Data.Maybe
import           Data.Ord
import           Data.Reflection (Reifies)
import           Data.SortedList (SortedList, toSortedList)
import qualified Data.SortedList as SL
import           Data.Text (Text)
import qualified Data.Vector.Unboxed as UV
import           Data.Word
import           GHC.Exts (fromList)

slHead :: SortedList a -> a
slHead = fst . fromJust . SL.uncons

-- | Who played in a game, the score they got, sorted in score order.
data PlayedGame c s m =
    PlayedGame { _gameExtra :: !m, _gameSorted :: !(SortedList (s, c)) }
  deriving (Show, Eq, Ord, Functor)

makeLenses ''PlayedGame

instance (ToJSON c, ToJSON s, ToJSON m) => ToJSON (PlayedGame c s m) where
    toJSON (PlayedGame e sg) =
      JS.object
        [("game", JS.Array . fromList .
                    map (\(s, c) -> JS.object [("competitor", toJSON c)
                                             ,("score", toJSON s)]) .
                    toList $ sg)
        ,("extra", toJSON e)
        ]

instance (FromJSON c, FromJSON s, FromJSON m, Ord c, Ord s) => FromJSON (PlayedGame c s m) where
  parseJSON =
    JS.withObject "PlayedGame" $ \v -> PlayedGame
      <$> v .: "extra"
      <*> ((v .: "game") >>=
           JS.withArray "Game Sorted"
             (fmap toSortedList .
                   mapM (JS.withObject "PlayedGame Competitor" $ \v' ->
                             (,) <$> v' .: "score" <*> v' .: "competitor") . toList))

-- Any solos in a round are immediately put into a played game in the round alone.
data Bracket c s m
 = Bracket
   { _bPlayed :: ![[PlayedGame c s m]]
     -- ^ list of rounds, recent first, containing played games, then which won.
     --   We append new games to the end of the first list.
     --   If we're playing a round, the first list in this must be for it.
   , _bUpcoming :: ![[c]]
     -- ^ a list of the list of competitors that are scheduled for a game in this round.
     --   who have upcoming matches in this round.
   , _bCurrent :: !(Maybe (PlayedGame c s m))
     -- ^ The current game, if Nothing the tournament is over.
   }
  deriving (Show, Eq, Ord, Functor)

makeClassy ''Bracket

instance (ToJSON c, ToJSON s, ToJSON m) => ToJSON (Bracket c s m) where
    toJSON (Bracket p u c) =
        JS.object [("played", toJSON p)
                  ,("upcoming", toJSON u)
                  ,("current", toJSON c)
                  ]

instance (FromJSON c, FromJSON s, FromJSON m, Ord c, Ord s) => FromJSON (Bracket c s m) where
    parseJSON = JS.withObject "Bracket" $ \v ->
                Bracket <$> v .: "played"
                        <*> v .: "upcoming"
                        <*> v .: "current"

data KuritaGame
 = KGame
   { _kgEndTime :: {-# UNPACK #-} !UTCTime
   , _kgCommentary :: {-# UNPACK #-} ![(UTCTime, Text)]
   }
 deriving (Show, Eq, Ord)

makeLenses ''KuritaGame

instance ToJSON KuritaGame where
  toJSON (KGame t cs) = JS.object [("end_time", toJSON t), ("commentary", toJSON cs)]

instance FromJSON KuritaGame where
  parseJSON = JS.withObject "KuritaGame" $ \v ->
    KGame <$> v .: "end_time" <*> v .: "commentary"

changeVoteType :: (Ord c, Ord s2) => (s1 -> s2) -> Bracket c s1 a -> Bracket c s2 a
changeVoteType f (Bracket p u cg) =
    Bracket
      (map (map changeTheGame) p)
      u
      (fmap changeTheGame cg)
  where
    changeTheGame (PlayedGame e sl) =
        PlayedGame e . toSortedList . map (\(s, c) -> (f s, c)) . toList $ sl

{-# SPECIALIZE changeVoteType :: (HyperLogLog p -> UV.Vector Int8) -> Bracket Text (HyperLogLog p) a -> Bracket Text (UV.Vector Int8) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 3) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 3) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 4) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 4) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 5) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 5) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 6) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 6) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 7) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 7) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 8) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 8) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 9) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 9) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 10) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 10) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 11) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 11) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 12) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 12) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 13) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 13) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 14) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 14) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 15) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 15) a #-}
{-# SPECIALIZE changeVoteType :: (UV.Vector Int8 -> HyperLogLog 16) -> Bracket Text (UV.Vector Int8) a -> Bracket Text (HyperLogLog 16) a #-}

seedBracket :: (Ord c, Ord s, Monoid s) => ([c] -> m) -> (Int -> [c] -> m) -> [c] -> Bracket c s m
seedBracket sm fm cs = Bracket [map (upToGame sm) singles] (tail pairs) (Just $ upToGame (fm 0) $ head pairs)
  where
    (singles, pairs) = partition ((==) 1 . length) . chunksOf 2 $ cs

-- | Finishes the current game and start the next, generating a new round if need be,
--   or finishing the game if this was the last round.
finishGame :: (Ord c, Ord s, Monoid s) => ([c] -> m) -> (Int -> [c] -> m) -> Bracket c s m -> Bracket c s m
finishGame _ _ b@(Bracket {_bCurrent=Nothing}) = b
finishGame sm fm b@(Bracket {_bPlayed=p, _bCurrent=Just cg}) =
  let plyd = (cg:(head p)):(tail p)
  in case b^.bUpcoming of
       ncs:r -> Bracket plyd r (Just $ upToGame (fm (length plyd)) ncs)
       -- The last round only ever has one game in it.
       -- If we're finishing a game, theres no games in the current round,
       -- and theres no unplayed games in the round, it must be the last one.
       [] | (b^.bPlayed.to (null.head)) -> Bracket plyd [] Nothing
       -- Since its not the last bracket, seed the next bracket and start it.
       [] -> let (singles, pairs) = partition ((==) 1 . length) .
                                   chunksOf 2 . reverse .
                                   map snd .
                                   map ((\(Down d) -> d) . slHead . SL.reverse . _gameSorted) $
                                   head plyd
            in Bracket ((map (upToGame sm) singles):plyd) (tail pairs) (Just $ upToGame (fm (1+length plyd)) $ head pairs)

upToGame :: (Ord c, Ord s, Monoid s) => ([c] -> m) -> [c] -> PlayedGame c s m
upToGame fm cs = PlayedGame (fm cs) . toSortedList . map (\c -> (mempty, c)) $ cs

addScores :: (Ord c, Ord s, Semigroup s) => [(c, s)] -> Bracket c s a -> Bracket c s a
addScores sc =
  bCurrent._Just.gameSorted %~
    SL.map (\(s, c) -> (,c) . fromMaybe s $ (fmap (s<>) (lookup c sc)))

addScore :: (Ord c, Reifies p Integer)
         => c -> Word32 -> Bracket c (HyperLogLog p) a -> Bracket c (HyperLogLog p) a
addScore ct v =
    bCurrent._Just.gameSorted %~
      SL.map (\(s, c) -> if c==ct then (HLL.insertHash v s, c) else (s, c))

data ClientGame
 = CGame
   { _cgEndTime :: {-# UNPACK #-} !UTCTime
   , _cgCommentary :: {-# UNPACK #-} ![Text]
   }
  deriving (Show, Eq, Ord)

k2cGame :: KuritaGame -> ClientGame
k2cGame (KGame et cs) = CGame et $ fmap snd $ take 10 $ cs


data TDown c
 = BattleStart
   { tdBrackets     :: Bracket c Int64 ClientGame
   }
 | ScoreUpdate [(c, Int64)]
 deriving (Show)

instance ToJSON ClientGame where
  toJSON (CGame et cs) =
    JS.object [("end_time", toJSON et), ("commentary", toJSON cs)]

instance FromJSON ClientGame where
  parseJSON = JS.withObject "ClientGame" $ \v ->
              CGame <$> v .: "end_time" <*> v .: "commentary"

instance ToJSON c => ToJSON (TDown c) where
    toJSON (BattleStart b) =
        JS.object [("event", JS.String "start")
                  ,("bracket", toJSON b)
                  ]
    toJSON (ScoreUpdate u) =
        JS.object [("event",  JS.String "score")
                  ,("scores", JS.Array . fromList .
                              map (\(c, s) -> JS.object [("competitor", toJSON c)
                                                       ,("score", toJSON s)]) $
                              u)
                  ]

instance (Ord c, FromJSON c) => FromJSON (TDown c) where
    parseJSON = JS.withObject "TDown" $ \v -> do
                  t <- v .: "event"
                  case t::Text of
                    "start" -> BattleStart <$> v .: "bracket"
                    "score" -> fmap ScoreUpdate . JS.withArray "TDown Score"
                                (mapM (JS.withObject "TDown Score Elem" (\v' -> (,) <$> v' .: "competitor" <*> v' .: "score")) . toList) =<< v .: "scores"
                    _ -> fail "Not a known TUp type"

data TUp c
 = Vote c
 deriving (Show, Eq, Ord)

instance ToJSON c => ToJSON (TUp c) where
    toJSON (Vote c) =
      JS.object [("event", JS.String "vote")
                ,("for", toJSON c)
                ]

instance FromJSON c => FromJSON (TUp c) where
    parseJSON = JS.withObject "TUp" $ \v -> do
                  t <- v .: "event"
                  case t::Text of
                    "vote" -> Vote <$> v .: "for"
                    _ -> fail "Not a known TUp type"
{-
data RDown c
 = CurrentBattle
   { rdBrackets     :: Bracket c Int64 KuritaGame
   }
 deriving (Show)
-}

data RUp hllsz c
 = Votes [(c, HyperLogLog hllsz)]
 deriving (Show)

instance ToJSON c => ToJSON (RUp hllsz c) where
    toJSON (Votes vs) =
        JS.object [("type", "votes")
                  ,("votes", JS.Array . fromList .
                              map (\(c, s) -> JS.object [("competitor", toJSON c)
                                                       ,("score", JS.Array . fromList .
                                                                  fmap toJSON . UV.toList .
                                                                  runHyperLogLog $ s)]) $
                              vs)
                  ]

instance (Reifies hllsz Integer, FromJSON c) => FromJSON (RUp hllsz c) where
    parseJSON = JS.withObject "RUp" $ \v -> do
                  t <- v .: "type"
                  case t::Text of
                    "votes" -> do
                      Votes <$> (v .: "votes" >>=
                        (JS.withArray "votes"
                           (mapM (JS.withObject "RUp single vote" $ \ v' ->
                                     ((,) <$> v' .: "competitor"
                                          <*> (v' .: "score" >>= decodeHLL)))  . toList)))
                    _ -> fail "Not a known RUp type"

decodeHLL :: Reifies p Integer => JS.Value -> JS.Parser (HyperLogLog p)
decodeHLL =
  JS.withArray "Score array"
    (fmap (HyperLogLog . UV.fromList) . mapM JS.parseJSON . toList)

