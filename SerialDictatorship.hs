module SerialDictatorship where

import MatchDatatype 

type PrefTable a b = Match a b

removeJust :: Maybe a -> a 
removeJust (Just x) = x

serialAssignment :: (Eq a,Eq b) => [a] ->[(a,Maybe b)] ->  PrefTable a b -> Match a b 
serialAssignment [] a _ = Match $ map (\(x,y) -> if y == Nothing then (x,[],0) 
                                                                 else (x,[removeJust y],0)) (reverse a)
serialAssignment (x:xs) as m = let p = (getpreferences x m)
                                   m' = delpreference (p!!0) m
                                   a' = if p == [] then (x,Nothing) else (x,Just $ p!!0)
                               in serialAssignment xs (a':as) m' 
