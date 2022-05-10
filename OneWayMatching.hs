module OneWayMatching where

import MatchType 
import Data.Maybe

type PrefTable a b = Match a b

serial :: (Eq a,Eq b) => [a] ->[(a,Maybe b)] -> PrefTable a b -> Match a b 
serial [] a _ = Match $ map f (reverse a)
    where f (x,y) = if y == Nothing then (x,[],0) else (x,[fromJust y],0)

serial (x:xs) as m = serial xs (a':as) m' 
    where p  = (getpreferences x m)
          m' = delpreference (p!!0) m
          a' = if p == [] then (x,Nothing) else (x,Just $ p!!0)
          
