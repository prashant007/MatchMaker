{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,  InstanceSigs, LambdaCase, DefaultSignatures, FlexibleInstances #-}
module DataType where

import Data.Function
import Data.List 
import Data.Maybe 
import GaleShapley 
import Info 
import qualified Data.Map as M 

data Level =  VLow | Low | Med | High | VHigh deriving (Eq,Ord,Show)
type Rating = Int -- 1 to 10 with 10 being highest
-- =============================================================================================

class MatchSet a where
    members :: [a]

    capacity :: a -> Int  
    capacity _ = 1 

type Rank a b =  [(a,[b],TCapacity)] 


infoToRank :: (MatchSet a,MatchSet b,Evaluable c) => Info a b c -> Rank a b 
infoToRank i = undefined 


-- sortInfo ::(MatchSet a,Ord b,Evaluable c) => Info a b c -> Rank a b 
-- sortInfo = map (\(x,y) -> (x,sortOnSnd y,capacity x)) . fromInfo . evalInfo 
--     where
--         evalInfo = mapInfo2 norm . filterInfo (\_ -> True) 


-- sortOnSnd :: [(a,Maybe Double)] -> [a]
-- sortOnSnd = map fst . reverse . sortBy (compare `on` (fromJust.snd))

              
class (MatchSet a,MatchSet b,Evaluable c,Ord b) => Relate a b c | a b -> c where
    assignVal :: Info a b c 

    getRanks :: Rank a b 
    getRanks = map (\(x,y) -> (x,sortOnSnd y,capacity x)) . fromInfo . evalInfo $ assignVal 
        where 
          evalInfo  = mapInfo2 norm . filterInfo (\_ -> True) 
          sortOnSnd = map fst . reverse . sortBy (compare `on` (fromJust.snd))

class Evaluable a where
    norm :: a -> Double

    evalM :: Maybe a -> Maybe Double 
    evalM Nothing = Nothing 
    evalM (Just x) = Just $ norm x 


instance Evaluable Bool where
    norm = \case {False -> 0.0 ; True -> 1.0 } 

instance Evaluable (Double,Double) where
    norm (v,lv) = v/lv  

instance Evaluable (Int,Int) where
    norm (v,lv) = (fromIntegral v)/(fromIntegral lv)  

instance Evaluable (Double,Int) where
    norm (v,lv) = v/(fromIntegral lv)  

instance Evaluable (Int,Double) where
    norm (v,lv) = (fromIntegral v)/ lv


-- =================================================================================================
-- =================================================================================================

type Match a b = [(a,[b],Int)]
type RankP a b =  (Rank a b,Rank b a)

class  (Relate a b c,Relate b a d,Eq b,Eq a) => StableMarriage a b c d | a b -> c d where
    solveP :: Match a b
    solveP = map (\(p,(_,r,_,t)) -> (p,r,t)) ls 
        where
          ls = galeShapley (f x) (f y) (f x)
          (x,y) = (getRanks,getRanks)
          f = map (\(a,b,c) -> (a,(b,[],c,c))) 


class Relate a a b => StableRoommate a b | a -> b where
    solveS :: Match a b 
    solveS = undefined  

-- =================================================================================================


madm ws = sum.zipWith (*) ws


evalL = \case {VLow -> 0.2; Low -> 0.4 ; Med -> 0.6 ; High -> 0.8 ; VHigh -> 1.0 } 
evalB = \case {False -> 0.0 ; True -> 1.0 } 

evalR :: Rating -> Double
evalR = \v -> (fromIntegral v)/10.0

expressCh :: (MatchSet b,Evaluable c,Eq b) => [b] -> [Maybe c] -> (b -> Maybe c) 
expressCh bs cs b = cs !! i
    where i = (fromJust.elemIndex b) bs  

-- =================================================================================================

showMatch :: (Show a,Show b) => Match a b -> IO ()
showMatch ls = do 
    let f (x,y,z) = show x ++ ": \n\t\t Matched with " ++ show y ++ "\n\t\t Remaining capacity: " ++ show z ++ "\n"
    mapM_ (putStrLn.f) ls  

-- =================================================================================================


