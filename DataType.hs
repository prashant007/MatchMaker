{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds, InstanceSigs, LambdaCase, FlexibleContexts, DefaultSignatures, FlexibleInstances #-}
module DataType where

import Data.Function
import Data.List 
import Data.Maybe 
import GaleShapley 
import Info 
import qualified Data.Map as M 

data Level =  VLow | Low | Med | High | VHigh deriving (Eq,Ord,Show)
type Rating = Int -- 1 to 10 with 10 being highest

data Rank = Rank {unRank :: Int} 
rank = Just . Rank
-- =============================================================================================

class  (Bounded a,Enum a,Ord a) => Set a where
    members :: [a]
    members = enumFromTo minBound maxBound

    capacity :: a -> Capacity  
    capacity _ = 1 

data Match a b = Match {unMatch:: [(a,[b],Capacity)]} 

instance (Show a,Show b) => Show (Match a b) where
    show = concatMap f . unMatch  
        where 
            f (x,y,z) = show x ++ ": \n\t\t Matched with " ++ show y
                        ++ "\n\t\t Remaining capacity: " ++ show z ++ "\n"
     
sortSnd :: Ord c => [(b,Maybe c)] -> [b]
sortSnd = map fst . reverse . sortBy (compare `on` (fromJust.snd))

type Val o a = Info o a [Double]

decomposed ::  (Ord a,Norm b) => Info o a b -> Val o a 
decomposed = mapInfo components 

rankOrder :: (Set a,Set b,Norm c,Ord b) => Info a b c -> Match a b 
rankOrder = Match . map (\(x,y) -> (x,sortSnd y,capacity x)) . fromInfo . mapInfo norm
     
class Relate a b c | a b -> c where
    gather :: Info a b c 


class Ord a => Fixed a b where
    profile :: Rec a b 
    profile = toRec []

-- class (Ord a,Ord b,Fixed b c) => Relate a b c d | a b -> c d where

--     gather :: Info a b d 
--     gather = info []


class Norm a where
    components :: a -> [Double]
    components _ = []

    norm :: a -> Double
    norm = sum . components

instance Norm Rank where
    norm (Rank r) = (1/fromIntegral r)

instance Norm Bool where
    norm = \case {False -> 0.0 ; True -> 1.0 } 

instance Norm (Double,Double) where
    norm (v,lv) = min (v/lv) 1   

instance Norm (Int,Int) where
    norm (v,lv) = min (fromIntegral v/fromIntegral lv) 1

instance Norm (Double,Int) where
    norm (v,lv) = min (v/fromIntegral lv)  1 

instance Norm (Int,Double) where
    norm (v,lv) = min (fromIntegral v/lv) 1 

-- =================================================================================================
-- =================================================================================================

type SetNorm a b c d = (Set a,Set b,Norm c, Norm d)
type Set2 a b = (Set a,Set b)
-- type Rel2 a b c d = (Relate a b c,Relate b a d)

rankOrder1 :: (Set a,Ord b) => Info a b Rank -> Match a b 
rankOrder1 = Match . map (\(x,y) -> (x,sortRank y,capacity x)) . fromInfo . mapInfo unRank 
    where 
        sortRank :: Ord c => [(b,Maybe c)] -> [b]
        sortRank = map fst . sortBy (compare `on` (fromJust.snd))

class (Relate a b Rank,Relate b a Rank, Set2 a b) => TwowayMatchWithRank a b where
    stableMatchWithRank :: Match a b 
    stableMatchWithRank = Match $ map (\(p,(_,r,_,t)) -> (p,r,t)) ls 
        where
          ls = galeShapley (f x) (f y) 
          (x,y) = (rankOrder1 gather,rankOrder1 gather)
          f = map (\(a,b,c) -> (a,(b,[],c,c))) . unMatch   
  

choices :: (Ord a,Ord b) => [(a,[b])] -> Info a b Rank
choices = info . map (\(x,ys) -> (x,assocRanks ys))
    where assocRanks =  toRec . zipWith (\q p -> p --> rank q) [1..] 


class (Relate a b c,Set a, Set b, Norm c, Norm d) =>
      TwowayMatch a b c d | a b -> c d where

      stableMatch :: Relate b a d => Match a b
      stableMatch = Match $ map (\(p,(_,r,_,t)) -> (p,r,t)) ls 
          where
            ls = galeShapley (f x) (f y) 
            (x,y) = (rankOrder gather,rankOrder gather)
            f = map (\(a,b,c) -> (a,(b,[],c,c))) . unMatch 

      stableMatchOneWay :: Match a b 
      stableMatchOneWay = undefined    

type RommateMatch a = Match a a 

class StableRoommate a b | a -> b where
    stableRoommate :: RommateMatch a 
    stableRoommate = undefined   

class Relate a b c => OnewayMatch a b c | a b -> c where
    rankMaximal :: Match a b 

-- =================================================================================================
weight :: [Double] -> [Double] -> [Double] 
weight ws = zipWith (*) ws 

evalL = \case {VLow -> 0.2; Low -> 0.4 ; Med -> 0.6 ; High -> 0.8 ; VHigh -> 1.0 } 
evalB = \case {False -> 0.0 ; True -> 1.0 } 

evalR :: Rating -> Double
evalR = \v -> (fromIntegral v)/10.0

expressCh :: (Set b,Norm c,Eq b) => [b] -> [Maybe c] -> (b -> Maybe c) 
expressCh bs cs b = cs !! i
    where i = (fromJust.elemIndex b) bs  

class Set b => Annotate a b | b -> a where
    labels :: [b]
    labels = members

