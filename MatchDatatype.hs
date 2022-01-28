module MatchDatatype where 


import Data.Maybe 
import Data.List 

type Capacity = Int 
data Match a b = Match {unMatch:: [(a,[b],Capacity)]} 

instance (Show a,Show b) => Show (Match a b) where
    show = concatMap f . unMatch  
        where 
            f (x,y,z) = show x ++ ": \n\t\t Matched with " ++ show y
                        ++ "\n\t\t Remaining capacity: " ++ show z ++ "\n"

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

-- modMatch :: (Eq a,Eq b) => (a -> [b] -> [b]) -> (a -> Capacity -> Capacity) ->
--             a -> Match a b -> Match a b 
-- modMatch f g x m = onMatch h m 
--                  where (xchs,xcap) = lookupMatch x m    
--                        h y = (x,f x xchs,g x xcap):(y \\ [(x,xchs,xcap)])

modMatch :: (Eq a,Eq b) => ((a,[b],Capacity) -> (a,[b],Capacity)) -> Match a b -> Match a b 
modMatch f = Match . map f . unMatch 

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

iterChangeperefrences :: (Eq a,Eq b) => ([b] -> [b]) -> [a] -> Match a b -> Match a b 
iterChangeperefrences _ [] m = m 
iterChangeperefrences f (x:xs) m = iterChangeperefrences f xs (changepreferences f x m)

topchoice :: Eq a => a -> Match a b -> b  
topchoice x = head . getpreferences x

allProposers :: Eq a => Match a b -> [a]
allProposers = foldMatch (\(x,_,_) acc -> x:acc) []

-- allProposers = foldMatch (map (fst.mkPair))

-- allProposees :: Eq a => Match a b -> [a]
-- allProposees = foldMatch (map (fst.snd.mkPair))
