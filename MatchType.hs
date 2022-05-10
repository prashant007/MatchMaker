{-# LANGUAGE ConstraintKinds #-} 

module MatchType where 

import Data.Maybe 
import Data.List 


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

data Match a b = Match {unMatch:: [(a,[b],Capacity)]} 
type SameSetMatch a = Maybe (Match a a) 

data CMatch a b =  CMatch {unCMatch :: [(a,[b],Capacity)]} 
data CompMatch a b = CompMatch {unCompMatch :: [(a,[b],[b])]} 
data CompRanks a b = CompRanks {unCompRanks :: [(a,[(b,Rank)],[(b,Rank)])]} 

assign :: [(a,b)] -> Match a b 
assign = Match . map f 
    where f (x,y) = (x,[y],1)


combine :: (Eq a,Eq b) => [(a,[b])] -> [(a,[b])] -> [(a,[b],[b])]
combine [] ys = map (\(x,y) -> (x,[],y)) ys 
combine ((a,as):xs) ys = case lookup a ys of 
    Nothing -> (a,as,[]):combine xs ys 
    Just as'-> (a,as,as'):combine xs (delete (a,as') ys)

instance (Show a,Show b,Ord2 a b) => Show (Match a b) where
    show = parens. concat. intersperse ", ". map f . sort . unMatch  
        where 
            parens = \x -> "{" ++ x ++ "}"
            f (x,y,z) = show x ++ " --> " ++ show y 

instance (Show2 a b,Ord2 a b) => Show (CMatch a b) where
    show = parens. concat. intersperse ", ". map f . sort. unCMatch  
        where 
            parens = \x -> "{" ++ x ++ "}"
            f (x,y,z) = show x ++ " --> " ++ show y ++ " : " ++ show z 
            
instance (Show2 a b,Eq b,Ord2 a b) => Show (CompMatch a b) where
    show = parens. concat. intersperse ", ". map f . sort . filter g . unCompMatch  
        where 
            g (x,y,z) = y /= z
            parens = \x -> "{" ++ x ++ "}"
            f (x,y,z) = show x ++ " --> " ++  show y ++ " => " ++ show z 

instance (Show2 a b,Eq b,Ord2 a b) => Show (CompRanks a b) where
    show = parens. concat. intersperse ", ". map f . sort . unCompRanks 
        where 
            parens = \x -> "{" ++ x ++ "}"
            f (x,y,z) = show x ++ " --> " ++  showPairs y z 
            
            showPair :: Show b =>  (b,Rank) -> String 
            showPair (b,r) = show b ++ " : " ++ show r 

            showPairList :: Show b => [(b,Rank)] -> String 
            showPairList [x] = showPair x 
            showPairList xs = "[" ++ concatMap showPair xs ++ "]"
             

            showPairs :: (Show b, Ord b) => [(b,Rank)] -> [(b,Rank)] -> String  
            showPairs xs ys 
                | and ls = f " < " xs ys 
                | not.or $ ls = f " > " xs ys 
                | otherwise  = f " ? " xs ys 
                where ls = zipWith (\x y -> not $ x < y ) xs ys 
                      f x y z = showPairList y ++ x ++ showPairList z

type StabMatch a = Match a a 

mkPair (x,y,z) = (x,(y,z))

lookupMatch :: Eq a => a -> Match a b -> ([b],Capacity)
lookupMatch a = fromJust . lookup a . map mkPair . unMatch 

applyMatch :: ([(a,[b],Capacity)] -> c) -> Match a b -> c 
applyMatch f = f . unMatch

onMatch :: ([(a,[b],Capacity)] -> [(a,[b],Capacity)]) -> 
           Match a b -> Match a b 
onMatch f = Match . f . unMatch 




foldMatch :: ((a,[b],Capacity) -> c -> c) -> c -> Match a b -> c 
foldMatch f acc = foldr f acc . unMatch

modMatch :: (Eq a,Eq b) => ((a,[b],Capacity) -> (a,[b],Capacity)) -> Match a b -> Match a b 
modMatch f = Match . map f . unMatch 

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
reducecapacity a c = modMatch (\xc@(x,y,z) -> if x == a then (x,y,z-c) else xc)

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



