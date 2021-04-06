{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies #-}
module MDS where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function
import Data.List
import Text.Printf
import DataType 
import Info 
import Data.Maybe (fromJust)

-- import Record
--import Info
-- import Valuation
-- import Dimension

-- Analysis of Decisions:
--  (support, barrier, dominators, mds)
--

type Label a = a 

data LabeledVal a = LVal {unLVal :: M.Map (Label a) Double}

mkLVal :: Ord a => [(a,Double)] -> LabeledVal a
mkLVal = LVal . M.fromList

emptyLVal :: Ord a => LabeledVal a
emptyLVal = mkLVal []

fromLVal :: LabeledVal a -> [(a,Double)]
fromLVal = M.toList . unLVal 

mapLVal2 :: Ord a => (Double -> Double -> Double) -> LabeledVal a -> LabeledVal a -> LabeledVal a
mapLVal2 g x y = LVal $ merge preserveMissing preserveMissing
                       (zipWithMatched (\_->g)) (unLVal  x) (unLVal  y)

mapLVal :: Ord a => (Double -> Double) -> LabeledVal a -> LabeledVal a
mapLVal f =  LVal . M.map f . unLVal 

foldLVal :: (Double -> b -> b) -> b -> LabeledVal a -> b
foldLVal f x =  M.foldr f x . unLVal 

foldLValWithKey :: (a -> Double -> b -> b) -> b -> LabeledVal a -> b
foldLValWithKey f x = M.foldrWithKey f x . unLVal 

instance Ord a => Num (LabeledVal a) where
  (+) = mapLVal2 (+)
  (*) = mapLVal2 (*)
  (-) = mapLVal2 (-)
  negate = mapLVal negate
  abs    = mapLVal abs
  signum = mapLVal signum
  fromInteger x = undefined

type Analysis a = (LabeledVal a,LabeledVal a,[LabeledVal a],[LabeledVal a])

-- Summarized set
--
data SumSet a = SumSet [a] Double deriving Eq 

instance Eq a => Ord (SumSet a) where
   SumSet _ x <= SumSet _ y = x <= y 


showSet :: [String] -> String
showSet xs = "{" ++ intercalate ", " xs ++ "}"

instance Show a => Show (SumSet a) where
  show (SumSet xs d) = showSet (map show xs) ++ " : " ++ printf ("%.7f") d

-- Dominance relation
--
data Dominance a = Dominance (SumSet a) (SumSet a)

instance Show a => Show (Dominance a) where
  show (Dominance x y) = show x ++ " > |" ++ show y ++ "|"

-- Explanation
--
data Explanation o a = Explanation o o (Dominance a)

instance (Show o,Show a) => Show (Explanation o a) where
  show (Explanation w r d) =
       show w ++ " is better than " ++ show r ++ " because\n" ++ show d

analyze :: Ord a => LabeledVal a -> Analysis a
analyze v = (mkLVal support,mkLVal barrier,map mkLVal sdoms,map mkLVal smdss)
  where
    (support,barrier) = partition ((>0) . snd) (fromLVal v)
    absSum = abs . sum . map snd
    doms   = [d | d <- subsequences support, absSum d > absSum barrier]
    sdoms  = sortBy (compare `on` length) doms
    mdss   = takeWhile (\p -> length p == (length.head) sdoms) sdoms
    smdss  = reverse $ sortBy (compare `on` absSum) mdss

barrier :: Ord a => LabeledVal a -> LabeledVal a
barrier r = b where (_,b,_,_) = analyze r

dominators :: Ord a => LabeledVal a -> [LabeledVal a]
dominators r = ds where (_,_,ds,_) = analyze r

mds :: Ord a => LabeledVal a -> [LabeledVal a]
mds r = ds where (_,_,_,ds) = analyze r

dominance :: Ord a => LabeledVal a -> Dominance a
dominance r = Dominance (toSumSet d) (toSumSet b)
              where (_,b,_,d:_) = analyze r

toSumSet :: LabeledVal a -> SumSet a
toSumSet = (\x -> SumSet (fst x) (snd x)) . 
           foldLValWithKey (\x y (l,v) -> (x:l,y+v)) ([],0)

-- explain :: (Set o,Set a,Norm b,Annotate b c,Ord c) => o -> (a,a) -> Info o a b -> Explanation a c    
-- explain o (w,l) i = Explanation w l (Dominance (toSumSet d) (toSumSet b))
--                     where deci = decomposed i 
--                           lkup a b = fromJust . lookupInfo a b 
--                           (winval,losval) = (lkup o w deci,lkup o l deci) 
--                           labVal = mkLVal . zip labels
--                           (win,los) = (labVal winval,labVal losval)
--                           (_,b,_,d:_) = analyze $ win - los 


explain :: (Ord o,Ord a,Norm b,Annotate b c,Ord c) => o -> (a,a) -> Info o a b -> Explanation a c    
explain o (w,l) i = Explanation w l (Dominance (toSumSet d) (toSumSet b))
                    where (_,b,_,d:_) = analyze $ diff i o w l 

(!) :: (Ord o,Ord a ,Norm b,Annotate b c) => Info o a b -> (o,a) -> LabeledVal c  
(!) i (x,y) = labVal $ fromJust $ lookupInfo (x,y) deci 
            where labVal = mkLVal . zip labels  
                  deci = decomposed i


diff :: (Ord o,Ord a,Norm b,Annotate b c) => Info o a b -> o -> a -> a -> LabeledVal c 
diff i o w l =  i!(o,w) - i!(o,l)

-- diff :: (Ord o,Ord r) => Info o r -> o -> o -> Rec r
-- diff i o1 o2 = i!o1 - i!o2

-- sensitivity :: (Set o,Set a,Norm b,Annotate b c,Ord c,Denormalize d) => 
--                o -> a -> c -> Info o a b -> d
-- sensitivity o a l ival =  
--                     where m = stableMatch ival 
--                           index = fromJust . elemIndex l labels
--                           aval  = fromJust . lookupInfo o a . decomposed $ i

-- explain :: (Ord o,Ord a) => Val o a -> Explanation o a
-- explain v = Explanation win rup (Dominance (total d) (total b))
--             where (win,rup)   = (winner v, runnerUp v)
--                   (_,b,_,d:_) = analyze $ diff v win rup

