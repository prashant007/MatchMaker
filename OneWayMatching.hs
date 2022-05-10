module OneWayMatching where

import MatchType 
import Data.Maybe
import TypeClasses 

type PrefTable a b = Match a b
type Endowment a b = [(a,b)]

serial :: (Eq a,Eq b) => [a] ->[(a,Maybe b)] -> PrefTable a b -> Match a b 
serial [] a _ = Match $ map f (reverse a)
    where f (x,y) = if y == Nothing then (x,[],0) else (x,[fromJust y],0)

serial (x:xs) as m = serial xs (a':as) m' 
    where p  = (getpreferences x m)
          m' = delpreference (p!!0) m
          a' = if p == [] then (x,Nothing) else (x,Just $ p!!0)

ttc :: (Set a,Eq2 a b) => Match a b -> Endowment a b -> [(a,b,a)] -> Match a b 
ttc m es ft 
    | matchCond m = mkMatch ft m 
    | os == [] = mkMatch ft m'
    | otherwise = ttc m' es (ft++os)
    where 
          (os,m') = oneIter m es  
          xs = topChoices m es 
          ls = map (\(x,_,_) -> x) xs 
          matchCond :: Match a b -> Bool 
          matchCond = (\x -> null x || length x ==1). unMatch

mkMatch :: [(a,b,a)] -> Match a b -> Match a b 
mkMatch xs = Match . (\x -> x ++ xs'). unMatch
    where xs' = map (\(x,y,z)->(x,[y],0)) xs 

oneIter ::  Match a b -> Endowment a b -> ([(a,b,a)],Match a b)
oneIter m es = (os,rmvMatchElems os m)
    where os = oneCycle ls xs
          xs = topChoices m es 
          ls = map (\(x,_,_) -> x) xs 

topChoices :: Eq b => Match a b -> Endowment a b -> [(a,b,a)]
topChoices m e = map (f m e) (allProposers m)    
    where f m e x = let y = topchoice x m 
                    in (x,y,holds y e)
          holds x = fromJust . lookup x . map swap
          swap (x,y) = (y,x)

oneCycle :: Eq a => [a] -> [(a,b,a)] -> [(a,b,a)]
oneCycle [] ls = []
oneCycle (x:xs) ls 
    | not.null $ ys  = ys 
    | otherwise = oneCycle xs ls 
    where ys = mkCycle x (find x ls) ls []  
          find a = head . filter (\(c,d,e) -> a == c)  

          mkCycle :: Eq a => a -> (a,b,a) -> [(a,b,a)] -> [(a,b,a)] -> [(a,b,a)]
          mkCycle o _ [] _ = []
          mkCycle o v@(x,y,z) ls acc
            | x == z = [v]
            | r == o = v:t:acc 
            | otherwise = mkCycle o t ls (v:acc)
            where t@(p,q,r) = find z ls
                  

rmvMatchElems :: Eq2 a b => [(a,b,a)] -> Match a b -> Match a b 
rmvMatchElems xs = delMatchAll as . delpreferenceAll bs 
    where as = map (\(x,y,z) -> x) xs 
          bs = map (\(x,y,z) -> y) xs 


