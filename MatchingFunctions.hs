{-# LANGUAGE ConstraintKinds #-} 
module MatchingFunctions where

import Data.Function
import Data.List 
import Data.Maybe 
import OneWayMatching 
import TwoWayMatching 
import SameSetMatching
import Info 
import qualified Data.Map as M 
import MatchType
import TypeClasses
import Combinators


twoWayWithCapacity :: (Preference2 a b c d,Set2 a b,Norm2 c d) => CMatch a b
twoWayWithCapacity = CMatch $ map (\(p,(_,r,_,t)) -> (p,r,t)) ls 
    where
      ls = galeShapley (f x) (f y) 
      (x,y) = (ranks gather,ranks gather)
      f = map (\(a,b,c) -> (a,(b,[],c,c))) . unMatch 

twoWayWithCapacity':: (Preference2 a b c d,Set2 a b,Norm2 c d) =>  Info a b c -> Info b a d -> CMatch a b
twoWayWithCapacity' xs ys = CMatch $ map (\(p,(_,r,_,t)) -> (p,r,t)) ls 
    where
      ls = galeShapley (f x) (f y) 
      (x,y) = (ranks xs,ranks ys)
      f = map (\(a,b,c) -> (a,(b,[],c,c))) . unMatch

twoWay' :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Info a b c -> Info b a d -> Match a b
twoWay' xs ys = rmvCapacity $ twoWayWithCapacity' xs ys 

twoWay :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Match a b
twoWay = rmvCapacity twoWayWithCapacity

twoWayDiff :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Info a b c -> Info b a d -> CompMatch a b 
twoWayDiff xs ys = diffMatch twoWay (twoWay' xs ys)

rmvCapacity :: CMatch a b -> Match a b 
rmvCapacity (CMatch ls) = Match $ map (\(p,r,c) -> (p,r,0)) ls

sameSet :: (Preference a a b,Set a,Norm b) => SameSetMatch a
sameSet = irvings $ ranks gather

oneWayWithCapacity :: (Preference a b c, Set2 a b,Norm c) => CMatch a b 
oneWayWithCapacity = CMatch $ unMatch $ serial members [] $ ranks gather

oneWayWithCapacity' :: (Preference a b c, Set2 a b,Norm c) => [a] -> CMatch a b 
oneWayWithCapacity' xs = CMatch $ unMatch $ serial xs [] $ ranks gather

oneWay' :: (Preference a b c,Set2 a b,Norm c) => [a] -> Match a b
oneWay' xs = rmvCapacity $ oneWayWithCapacity' xs 

oneWay :: (Preference a b c,Set2 a b,Norm c) => Match a b
oneWay = rmvCapacity oneWayWithCapacity 

-- tradewithCapacity :: (Preference a b c, Set2 a b,Norm c) => CMatch a b 
-- tradeWithCapacity = CMatch $ ttc $ ranks gather

trade :: (Preference a b c,Set2 a b,Norm c,Exchange a b c) => Match a b
trade = ttc (ranks gather) endowment []  

