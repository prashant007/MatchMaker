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


twoWayWithCapacity :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Match a b
twoWayWithCapacity = Match $ map (\(p,(_,r,_,t)) -> (p,r,Just t)) ls 
    where
      ls = galeShapley (f x) (f y) 
      (x,y) = (ranks gather,ranks gather)
      f = map (\(a,b,Just c) -> (a,(b,[],c,c))) . unMatch 

twoWayWithPref :: (Preference2 a b c d,Set2 a b,Norm2 c d) =>
                  Info a b c -> Info b a d -> Match a b
twoWayWithPref xs ys = rmvCapacity Match $ map (\(p,(_,r,_,t)) -> (p,r,Just t)) ls 
    where
      ls = galeShapley (f x) (f y) 
      (x,y) = (ranks xs,ranks ys)
      f = map (\(a,b,Just c) -> (a,(b,[],c,c))) . unMatch

-- twoWayWithPref :: (Preference2 a b c d,Set2 a b,Norm2 c d) => 
--                   Info a b c -> Info b a d -> Match a b
-- twoWayWithPref xs ys = rmvCapacity $ twoWayWithCapacity' xs ys 

twoWay :: (Preference2 a b c d,Set2 a b,Norm2 c d) => Match a b
twoWay = rmvCapacity twoWayWithCapacity

twoWayDiff :: (Preference2 a b c d,Set2 a b,Norm2 c d) =>
              Info a b c -> Info b a d -> CompMatch a b 
twoWayDiff xs ys = diffMatch twoWay (twoWayWithPref xs ys)

rmvCapacity :: Match a b -> Match a b 
rmvCapacity (Match ls) = Match $ map (\(p,r,c) -> (p,r,Nothing)) ls

sameSet :: (Preference a a b,Set a,Norm b) => SameSetMatch a
sameSet = fmap rmvCapacity $ irvings $ ranks gather

oneWayGeneral :: (Weights a,Norm c,Set2 a b) => [a] -> Info a b c -> Match a b
oneWayGeneral xs ys = serial xs [] (ranks ys)

oneWayWithCapacity :: (Preference a b c,Set2 a b,Norm c) => Match a b 
oneWayWithCapacity = oneWayGeneral members gather 

oneWayWithOrder :: (Preference a b c,Set2 a b,Norm c) => [a] -> Match a b
oneWayWithOrder xs = rmvCapacity $ oneWayGeneral xs gather 

oneWayWithPref :: (Preference a b c, Set2 a b,Norm c) => Info a b c -> Match a b 
oneWayWithPref = oneWayGeneral members 

oneWay :: (Preference a b c,Set2 a b,Norm c) => Match a b
oneWay = rmvCapacity oneWayWithCapacity 

trade :: (Preference a b c,Set2 a b,Norm c,Exchange a b) => Match a b
trade = ttc (ranks gather) (fromMatch' endowment) []  

