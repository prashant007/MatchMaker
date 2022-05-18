{-# LANGUAGE ConstraintKinds #-} 

module MatchType where 

import Data.Maybe 
import Data.List 
import Control.Applicative



type Eq2 a b  = (Eq a,Eq b)
type Ord2 a b = (Ord a,Ord b)
type Show2 a b = (Show a,Show b)
type Show3 a b c = (Show a,Show b,Show c)


data Rank = Rank {unRank :: Int} deriving (Eq,Ord)

instance Show Rank where
    show (Rank r) = show r 

rank = Just . Rank

(-->) :: a -> b -> (a,b)
(-->) x y = (x,y)

type Capacity = Int 

data Match a b = Match {unMatch:: [(a,[b],Maybe Capacity)]} 
type SameSetMatch a = Maybe (Match a a) 

data CompMatch a b = CompMatch {unCompMatch :: [(a,[b],[b])]} 
data CompRanks a b = CompRanks {unCompRanks :: [(a,[(b,Rank)],[(b,Rank)])]} 

-- toMatch :: Match a b -> Match a b 
-- toMatch = Match . map (\(x,y,z) -> (x,y,Just z)). unMatch

-- toCMatch :: Match a b -> CMatch a b 
-- toCMatch = CMatch . map (\(x,y,Just z) -> (x,y,z)). unMatch

assign :: [(a,b)] -> Match a b 
assign = Match . map f 
    where f (x,y) = (x,[y],Just 1)

combine :: (Eq a,Eq b) => [(a,[b])] -> [(a,[b])] -> [(a,[b],[b])]
combine [] ys = map (\(x,y) -> (x,[],y)) ys 
combine ((a,as):xs) ys = case lookup a ys of 
    Nothing -> (a,as,[]):combine xs ys 
    Just as'-> (a,as,as'):combine xs (delete (a,as') ys)

instance (Show a,Show b,Ord2 a b) => Show (Match a b) where
    show = parens. concat. intersperse ", ". map f . sort . unMatch  
        where 
            parens = \x -> "{" ++ x ++ "}"
            showF a b = show a ++ " --> " ++ show b 
            f (x,y,z) = if z == Nothing 
                          then showF x y
                          else showF x y ++ " : " ++ (show.fromJust) z 

instance (Show2 a b,Eq b,Ord2 a b) => Show (CompMatch a b) where
    show = parens. concat. intersperse ", ". map f . sort . filter g . unCompMatch  
        where 
            g (x,y,z) = y /= z
            parens = \x -> "{" ++ x ++ "}"
            f (x,y,z) = show x ++ " --> " ++  show y ++ " => " ++ show z 

instance (Show2 a b,Eq b,Ord2 a b) => Show (CompRanks a b) where
    show = parens. concat. intersperse ", ". map f . sort . filter g . unCompRanks 
        where 
            g (x,y,z) = y /= z
            parens = \x -> "{" ++ x ++ "}"
            f (x,y,z) = show x ++ " --> " ++  showPairs (head y) (head z) 
            
            showPair :: Show b =>  (b,Rank) -> String 
            showPair (b,r) = show b ++ " : " ++ show r 

            showPairs :: (Show b, Ord b) => (b,Rank) -> (b,Rank) -> String  
            showPairs xp@(x,xr) yp@(y,yr) 
                | xr < yr   = showPair xp ++ " > " ++ showPair yp 
                | xr > yr   = showPair xp ++ " < " ++ showPair yp 
                | otherwise = showPair xp ++ " ? " ++ showPair yp 


type StabMatch a = Match a a 

mkPair (x,y,z) = (x,(y,z))

fromMatch' :: Match a b -> [(a,b)]
fromMatch' = map(\(x,y,z)->(x,head y)) .unMatch

lookupMatch :: Eq a => a -> Match a b -> ([b],Capacity)
lookupMatch a = g . fromJust . lookup a . map mkPair . unMatch 
    where g = \(x,y)->(x,fromJust y)

applyMatch :: ([(a,[b],Capacity)] -> c) -> Match a b -> c 
applyMatch f = f . map rmvMaybe. unMatch

putMaybe :: (a,b,Capacity) -> (a,b,Maybe Capacity)
putMaybe = \(x,y,z) -> (x,y,Just z)

rmvMaybe :: (a,b,Maybe Capacity) -> (a,b,Capacity) 
rmvMaybe = \(x,y,z) -> (x,y,fromJust z)

onMatch :: ([(a,[b],Capacity)] -> [(a,[b],Capacity)]) -> 
           Match a b -> Match a b 
onMatch f = Match . map putMaybe . f . map rmvMaybe. unMatch 

foldMatch :: ((a,[b],Capacity) -> c -> c) -> c -> Match a b -> c 
foldMatch f acc = foldr g acc . unMatch
    where g (x,y,z) = f (x,y,fromJust z)


modMatch :: (Eq a,Eq b) => ((a,[b],Capacity) -> (a,[b],Capacity)) ->
                           Match a b -> Match a b 
modMatch f = Match . map (putMaybe . f . rmvMaybe). unMatch 

delMatch :: Eq a => a -> Match a b -> Match a b 
delMatch v = Match . filter (\(x,_,_) -> x /= v) . unMatch 

delMatchAll :: Eq a => [a] -> Match a b -> Match a b 
delMatchAll vs = Match . filter (\(x,_,_) -> not $ elem x vs) . unMatch 

getpreferences :: Eq a => a -> Match a b -> [b]
getpreferences x = fst . lookupMatch x

getcapacity :: Eq a => a -> Match a b -> Capacity 
getcapacity x = snd . lookupMatch x

id2 :: a -> b -> b 
id2 _ = id 

reducecapacity :: (Eq a,Eq b) => a -> Int -> Match a b -> Match a b 
reducecapacity a c = 
    modMatch (\xc@(x,y,z) -> if x == a then (x,y,z-c) else xc)

changepreferences :: (Eq a,Eq b) => ([b] -> [b]) -> a -> Match a b -> Match a b 
changepreferences f a = modMatch (\xc@(x,y,z) -> if x == a then (x,f y,z) else xc)

delpreference :: (Eq a,Eq b) =>  b -> Match a b -> Match a b
delpreference b = modMatch (\xc@(x,y,z) -> (x,delete b y,z))

delpreferenceAll :: (Eq a,Eq b) =>  [b] -> Match a b -> Match a b
delpreferenceAll bs = modMatch (\xc@(x,y,z) -> (x, y \\ bs,z))

iterChangeperefrences :: (Eq a,Eq b) => ([b] -> [b]) -> [a] -> Match a b -> Match a b 
iterChangeperefrences _ [] m = m 
iterChangeperefrences f (x:xs) m = iterChangeperefrences f xs (changepreferences f x m)

topchoice :: Eq a => a -> Match a b -> b  
topchoice x = head . getpreferences x

allProposers :: Eq a => Match a b -> [a]
allProposers = foldMatch (\(x,_,_) acc -> x:acc) []


instance Num a => Num (Maybe a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger