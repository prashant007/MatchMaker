{-# LANGUAGE FunctionalDependencies, ConstraintKinds #-}

module TypeClasses where

import MatchType 
import Info 
import Data.Maybe 

every :: b -> a -> b 
every c _ = c 

class  (Bounded a,Enum a,Ord a) => Set a where
    members :: [a]
    members = enumFromTo minBound maxBound

    capacity :: a -> Capacity  
    capacity = every 1


class Weights a where
    weights :: a -> [Double]
    weights _ = [1.0]
     
class Weights a => Preference a b c | a b -> c where
    gather :: Info a b c 

class Exchange a b where 
    endowment :: [(a,b)]
    endowment = []

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

outOf :: Norm a => a -> Double -> Double 
outOf x y = norm  (x,Just y)

only :: Norm a => a -> Double
only x = norm (x,Nothing)

type SetNorm a b c d = (Set a,Set b,Norm c, Norm d)
type Set2 a b = (Set a,Set b)
type Norm2 a b = (Norm a,Norm b)
type Preference2 a b c d = (Preference a b c,Preference b a d)