{-# LANGUAGE ConstraintKinds, InstanceSigs, LambdaCase, FlexibleContexts, DefaultSignatures, FlexibleInstances #-}

module Combinators where

import qualified Data.Map as M 
import Data.Function
import Data.List 
import Data.Maybe 

import OneWayMatching
import TwoWayMatching 
import SameSetMatching
import Info 
import MatchType
import TypeClasses 

ranks :: (Set a,Set b,Norm c,Weights a) => Info a b c -> Match a b 
ranks = Match . map (\(x,y) -> (x,sortSnd y,quota x)) . fromInfo . mapInfoWithKey1 norm'
    where norm' :: (Norm c,Weights a) => (a -> c -> Double) 
          norm' x y = sum. zipWith (*) (weights x) $ (components y)

          sortSnd :: Ord c => [(b,Maybe c)] -> [b]
          sortSnd = map fst . reverse . sortBy (compare `on` (fromJust.snd))

-- ==================================================================
-- Combinators for abstract encoding of preferences

completeWith :: Ord a => (b -> c -> d) -> Info a b c  -> Info a b d  
completeWith f = onInfo (M.map (mapRecWithKey f))

completeWith2 :: Ord a => (b -> c -> d -> e) -> Info a b (c,d)  -> Info a b e 
completeWith2 f = onInfo (M.map (mapRecWithKey (\k (x,y) -> f k x y)))

completeWith3 :: Ord a => (b -> c -> d -> e -> f) -> Info a b (c,d,e)  -> Info a b f 
completeWith3 f = onInfo (M.map (mapRecWithKey (\k (x,y,z) -> f k x y z)))

zipInfo :: Ord2 a b => Info a b c -> Info a b d -> Info a b (c,d)
zipInfo i = mapInfoWithKey (\x y z -> (lookupInfo x y i,z))    

zipInfo2 :: Ord2 a b => Info a b c -> Info a b d -> Info a b e -> Info a b (c,d,e)
zipInfo2 i1 i2 = 
    mapInfoWithKey (\x y z -> let (p,q) = lookupInfo x y (zipInfo i1 i2) in (p,q,z)) 

zipInfo3 :: Ord2 a b => Info a b c -> Info a b d -> Info a b e -> Info a b f -> Info a b (c,d,e,f)
zipInfo3 i1 i2 i3 =
   mapInfoWithKey (\x y z -> let (p,q,r) = lookupInfo x y (zipInfo2 i1 i2 i3) in (p,q,r,z))

-- ==================================================================
-- Combinators for modifying Info values

modInfoWith :: (Ord2 a b) => a -> (Rec b c -> Rec b c) -> Info a b c -> Info a b c 
modInfoWith x f = onInfo (M.alter (Just . f . fromJust) x)

updateWithRow :: (Ord2 a b) => Info a b c -> (a,[(b,c)]) ->  Info a b c 
updateWithRow is (x,ls) = modInfoWith x (\ys -> combineRec ys $ mkRec ls) is 

modWithRowDef :: (Ord2 a b,Preference a b c) => (a,[(b,c)]) ->  Info a b c 
modWithRowDef (x,ls) = modInfoWith x (\_ -> mkRec ls) gather 

modRow' :: Ord2 a b => a -> Rec b c -> Info a b c -> Info a b c 
modRow' x ls = modInfoWith x (\_ -> ls)

updtInfoH :: Ord2 a b => a -> Rec b c -> Info a b c -> Info a b c 
updtInfoH x ls is = updateWithRow is (x,map f . fromRec $ ls)
    where f (x,Just y) = (x,y)

modWithInfo :: Ord2 a b => Info a b c  -> Info a b c -> Info a b c 
modWithInfo xs ys = M.foldrWithKey modRow' xs (unInfo ys)

updateWithInfo :: Ord2 a b => Info a b c  -> Info a b c -> Info a b c 
updateWithInfo xs ys = M.foldrWithKey updtInfoH xs (unInfo ys)

modWithInfoDef :: (Ord2 a b,Preference a b c) => Info a b c -> Info a b c 
modWithInfoDef = modWithInfo gather 

modWithRanks :: Ord2 a b => Info a b Rank -> (a,[b]) -> Info a b Rank 
modWithRanks is (x,rs) =  is `updateWithRow` (x --> (assocRanks rs)) 
    where  assocRanks =  zipWith (\q p -> p --> Rank q) [1..] 

modWithRanksDef :: (Ord2 a b,Preference a b Rank) => (a,[b]) -> Info a b Rank 
modWithRanksDef = modWithRanks gather 

-- ==================================================================
-- Combinators for comparing two matches

diffRanks :: (Eq2 a b,Preference a b c,Set2 a b,Norm c) => Match a b -> Match a b -> CompRanks a b 
diffRanks xs = (toCompRanks (ranks gather). diffMatch xs)

diffMatch :: (Eq a,Eq b) => Match a b -> Match a b -> CompMatch a b 
diffMatch xs ys = CompMatch $ combine xs' ys'
    where f = map rmvthrd . unMatch
          xs' = f xs 
          ys' = f ys 
          rmvthrd = \(x,y,z) -> (x,y)