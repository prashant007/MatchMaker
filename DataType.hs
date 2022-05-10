{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds, InstanceSigs, LambdaCase, FlexibleContexts, DefaultSignatures, FlexibleInstances #-}
module DataType where

import Data.Function
import Data.List 
import Data.Maybe 
import GaleShapley 
import ImplementStableRoommate
import Info 
import qualified Data.Map as M 
import MatchDatatype
import SerialDictatorship


data Level =  VLow | Low | Med | High | VHigh deriving (Eq,Ord,Show)
type Rating = Int -- 1 to 10 with 10 being highest

-- =============================================================================================

every :: b -> a -> b 
every c _ = c 

class  (Bounded a,Enum a,Ord a) => Set a where
    members :: [a]
    members = enumFromTo minBound maxBound

    capacity :: a -> Capacity  
    capacity = every 1


groupings :: Set a =>  [[a]]
groupings = [[x,y] | x <- members, y <- members, x /= y]
     
sortSnd :: Ord c => [(b,Maybe c)] -> [b]
sortSnd = map fst . reverse . sortBy (compare `on` (fromJust.snd))

type Val o a = Info o a [Double]


rankOrder :: (Set a,Set b,Norm c,Weights a) => Info a b c -> Match a b 
rankOrder = Match . map (\(x,y) -> (x,sortSnd y,capacity x)) . fromInfo . mapInfoWithKey1 norm'


norm' :: (Norm c,Weights a) => (a -> c -> Double) 
norm' x y = sum. zipWith (*) (weights x) $ (components y)

class Weights a where
    weights :: a -> [Double]
    weights _ = [1.0]
     
class Weights a => Preference a b c | a b -> c where
    gather :: Info a b c 

class Preference a b c => Exchange a b c  where 
    endowment :: [(a,b)]
    endowment = []

class Preference a [b] c => Acceptable a b c where
    acceptable :: Info a b c 

class Norm a where
    components :: a -> [Double]
    components _ = []

    norm :: (a,Maybe Double) -> Double
    norm (x,_) = sum . components $ x

data NDouble = ND Double 
data NInt    = NI Int 

instance Norm Rank where
    components (Rank r) = [1/fromIntegral r]
    norm (Rank r,Nothing) = 1/fromIntegral r

instance Norm Bool where
    norm (x,Nothing) = case x of {False -> 0.0 ; True -> 1.0} 

instance Norm Double where
    norm (v,lv) = min (v/(fromJust lv)) 1   

instance Norm NDouble where
    norm (ND v,lv) = min ((fromJust $ lv)/v) 1  

instance Norm Int where
    norm (v,lv) = min (fromIntegral v/(fromJust lv)) 1

instance Norm NInt where
    norm (NI v,lv) = min (fromJust lv/fromIntegral v) 1

normAll :: Norm a => [(a,Maybe Double)] -> [Double]
normAll = map norm 

-- =================================================================================================
-- =================================================================================================

type SetNorm a b c d = (Set a,Set b,Norm c, Norm d)
type Set2 a b = (Set a,Set b)
type Norm2 a b = (Norm a,Norm b)
type Preference2 a b c d = (Preference a b c,Preference b a d)

-- choicesM :: (Ord a,Ord b) => [(a,[[b]])] -> Info a b Rank
-- choicesM = info . map (\(x,ys) -> (x,assocRanks ys))
--     where assocRanks =  zipWith (\q p -> p --> Rank q) [1..] 

twoWayWithCapacity :: (Preference2 a b c d,Set2 a b,Norm2 c d) => CMatch a b
twoWayWithCapacity = CMatch $ map (\(p,(_,r,_,t)) -> (p,r,t)) ls 
    where
      ls = galeShapley (f x) (f y) 
      (x,y) = (rankOrder gather,rankOrder gather)
      f = map (\(a,b,c) -> (a,(b,[],c,c))) . unMatch 

twoWayWithCapacity':: (Preference2 a b c d,Set2 a b,Norm2 c d) =>  Info a b c -> Info b a d -> CMatch a b
twoWayWithCapacity' xs ys = CMatch $ map (\(p,(_,r,_,t)) -> (p,r,t)) ls 
    where
      ls = galeShapley (f x) (f y) 
      (x,y) = (rankOrder xs,rankOrder ys)
      f = map (\(a,b,c) -> (a,(b,[],c,c))) . unMatch


twoWay' :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Info a b c -> Info b a d -> Match a b
twoWay' xs ys = rmvCapacity $ twoWayWithCapacity' xs ys 

twoWay :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Match a b
twoWay = rmvCapacity twoWayWithCapacity

-- compareF :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Info a b c -> CompMatch a b
-- compareF is = diffMatch twoWay (twoWay' is gather)

-- compareR :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Info a b c -> CompMatch b a
-- compareR is = diffMatch twoWay (twoWay' gather is) 

twoWayDiff :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Info a b c -> Info b a d -> CompMatch a b 
twoWayDiff xs ys = diffMatch twoWay (twoWay' xs ys)

-- compR1 :: (Relate2 a b c d,Set2 a b,Norm2 c d) => Info b a d -> CompMatch a b  
-- compR1 is = diffMatch (twoWay' gather is) twoWay 

-- compR2 :: (Relate2 a b c d,Set2 a b,Norm2 c d) => Info b a d -> CompMatch b a   
-- compR2 is = diffMatch (twoWay' is gather) twoWay 

-- class CompMatch a b c | a b -> c where
--     compTwoWay :: Info a b c -> CompMatch a b 


rmvCapacity :: CMatch a b -> Match a b 
rmvCapacity (CMatch ls) = Match $ map (\(p,r,c) -> (p,r,0)) ls

twoWayExpl :: (Preference2 a b c d,Set2 a b,Norm2 c d) => a -> b -> Match a b
twoWayExpl a b = Match $ map (\(p,(_,r,_,t)) -> if p == a then (p,b:r,t) else (p,r,t)) ls
    where 
        ls = galeShapley (f x') (f y') 
        (x,y) = (rankOrder gather,rankOrder gather)
        x1 = reducecapacity a 1 x 
        y1 = reducecapacity b 1 y
        x' = changepreferences (delete b) a x1 
        y' = changepreferences (delete a) b y1
        f = map (\(a,b,c) -> (a,(b,[],c,c))) . unMatch 


sameSet :: (Preference a a b,Set a,Norm b) => SameSetMatch a
sameSet = irvings $ rankOrder gather

oneWayWithCapacity :: (Preference a b c, Set2 a b,Norm c) => CMatch a b 
oneWayWithCapacity = CMatch $ unMatch $ serialAssignment members [] $ rankOrder gather

oneWayWithCapacity' :: (Preference a b c, Set2 a b,Norm c) => [a] -> CMatch a b 
oneWayWithCapacity' xs = CMatch $ unMatch $ serialAssignment xs [] $ rankOrder gather

oneWay' :: (Preference a b c,Set2 a b,Norm c) => [a] -> Match a b
oneWay' xs = rmvCapacity $ oneWayWithCapacity' xs 

oneWay :: (Preference a b c,Set2 a b,Norm c) => Match a b
oneWay = rmvCapacity oneWayWithCapacity 

trade :: (Preference a b c,Set2 a b,Norm c) => Match a b
trade = undefined 

getRank :: (Eq a, Eq b) => a -> b -> Match a b -> Rank 
getRank a b = Rank . (\x -> fromJust x + 1) . elemIndex b . getpreferences a 

toCompRanks :: (Eq a,Eq b) => Match a b -> CompMatch a b -> CompRanks a b
toCompRanks m = CompRanks .  map f . unCompMatch
    where f (x,ys,zs) = (x,map (g x) ys,map (g x) zs)
          g p q = (q,getRank p q m)

-- data CompMatch a b = CompMatch {unCompMatch :: [(a,[b],[b])]} 

-- data CompRanks a b = CompRanks {unCompRanks :: [(a,[(b,Rank)],[(b,Rank)])]} 

diffRanks :: (Eq a,Eq b,Preference a b c,Set2 a b,Norm c) => Match a b -> Match a b -> CompRanks a b 
diffRanks xs = (toCompRanks (rankOrder gather). diffMatch xs)

-- sameSetExpl :: (Relate a a b,Set a,Norm b) => a -> a -> SameSetMatch a 
-- sameSetExpl x y = case m2 of 
--                       Nothing -> Nothing 
--                       Just ms -> let Match ms' =  ms 
--                                  in Just $ Match $ (y,[x],0):(x,[y],0):ms'  
--     where m  = rankOrder gather
--           m1 = delPreference m 
--           m2 = irvings m1 

-- =================================================================================================
-- weight' :: [Double] -> [Double] -> [Double] 
-- weight' ws = zipWith (*) ws 

weight :: [(Double,Double)] ->  Double
weight xs = sum . zipWith (*) (map fst xs) $ (map snd xs)


evalL = \case {VLow -> 0.2; Low -> 0.4 ; Med -> 0.6 ; High -> 0.8 ; VHigh -> 1.0 } 
evalB = \case {False -> 0.0 ; True -> 1.0 } 

evalR :: Rating -> Double
evalR = \v -> (fromIntegral v)/10.0

expressCh :: (Set b,Norm c,Eq b) => [b] -> [Maybe c] -> (b -> Maybe c) 
expressCh bs cs b = cs !! i
    where i = (fromJust.elemIndex b) bs  

-- ================================================================================================




-- class Schedule a b | a -> b where
--     constraints :: Constraints a
--     availability :: Info a b Rank  

-- data Quantifier  = All | Oneof | LeastOne

modInfoWith :: (Ord a,Ord b) => a -> (Rec b c -> Rec b c) -> Info a b c -> Info a b c 
modInfoWith x f = onInfo (M.alter (Just . f . fromJust) x)

updateWithRow :: (Ord a,Ord b) => Info a b c -> (a,[(b,c)]) ->  Info a b c 
updateWithRow is (x,ls) = modInfoWith x (\ys -> combineRec ys $ mkRec ls) is 


-- modWithRow is (x,ls) = modInfoWith x (\ys -> mkRec ls) is 

modWithRowDef :: (Ord a,Ord b,Preference a b c) => (a,[(b,c)]) ->  Info a b c 
modWithRowDef (x,ls) = modInfoWith x (\_ -> mkRec ls) gather 

modRow' :: (Ord a,Ord b) => a -> Rec b c -> Info a b c -> Info a b c 
modRow' x ls = modInfoWith x (\_ -> ls)

updtInfoH :: (Ord a,Ord b) => a -> Rec b c -> Info a b c -> Info a b c 
updtInfoH x ls is = updateWithRow is (x,map f . fromRec $ ls)
    where f (x,Just y) = (x,y)
-- modWithElem :: (Ord a,Ord b) => Info a b c -> (a,(b,c)) -> Info a b c 
-- modWithElem is (x,(y,z)) = modInfoWith x (onRec (M.alter (f z) y)) is 
--     where  f p = \_ -> Just . Just $ p

-- modWithElemDef :: (Ord a,Ord b,Preference a b c) => (a,(b,c)) -> Info a b c 
-- modWithElemDef = modWithElem gather 

modWithInfo :: (Ord a,Ord b) => Info a b c  -> Info a b c -> Info a b c 
modWithInfo xs ys = M.foldrWithKey modRow' xs (unInfo ys)

updateWithInfo :: (Ord a,Ord b) => Info a b c  -> Info a b c -> Info a b c 
updateWithInfo xs ys = M.foldrWithKey updtInfoH xs (unInfo ys)

modWithInfoDef :: (Ord a,Ord b,Preference a b c) => Info a b c -> Info a b c 
modWithInfoDef = modWithInfo gather 

modWithRanks :: (Ord a,Ord b) => Info a b Rank -> (a,[b]) -> Info a b Rank 
modWithRanks is (x,rs) =  is `updateWithRow` (x --> (assocRanks rs)) 
    where  assocRanks =  zipWith (\q p -> p --> Rank q) [1..] 

modWithRanksDef :: (Ord a,Ord b,Preference a b Rank) => (a,[b]) -> Info a b Rank 
modWithRanksDef = modWithRanks gather 