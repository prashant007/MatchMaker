module SameSetMatching where

import MatchType 

import Data.Maybe 
import Data.List 


type Proposer a = a 
type Proposee a = a
type PrefTable a = Match a a 
type Flag = Int 
type PList a = [a]
type QList a = [a]
type PQPair a = (PList a,QList a)

-- The list of offers from Proposers currently held by Proposees.
-- This is an intermediate structure and keeps getting updated currently  
type OfferTable a = [(Proposee a,Proposer a)]
data OfferStatus a = Reject 
                   | NoEntryAccept (OfferTable a)
                   | BetterOfferAccept (OfferTable a) deriving Show 

-- check if the first element is higher in a list than the second element. 
-- return true if yes, false otherwise 
higher :: Eq a => a -> a -> [a] -> Bool 
higher x y ls = (index x ls - index y ls) < 0
              where index p = fromJust . elemIndex p 


-- check if the current proposer is rejected by the proposee 
-- if the proposee already holds a better proposal than the current proposer then reject (Reject) 
-- the current prposer. if proposee is not present in OfferTable (NoEntryAccept) or has
-- a worse offer than the current proposer then accept  
offerStatus :: Eq a => Proposer a -> Proposee a -> OfferTable a -> PrefTable a -> OfferStatus a 
offerStatus x y oTable m = 
    case lookup y oTable of 
        Nothing -> NoEntryAccept ((y,x):oTable)
        Just oldProposer -> if higher oldProposer x (getpreferences y m)
                                 then Reject 
                                 else BetterOfferAccept $ 
                                        (y,x): (oTable \\ [(y,oldProposer)])


termination :: Eq a => PrefTable a -> OfferTable a -> (Bool,Bool)
termination m o = (rejectedbyAll m,oneproposalAll m o)

-- check if any one person has been rejected by all, that is 
-- has an empty preference list 
rejectedbyAll :: PrefTable a -> Bool 
rejectedbyAll = foldMatch (\(_,x,_) -> (||) (null x)) False 

-- check if every person has one proposal in phase 1
oneproposalAll :: Eq a => PrefTable a -> OfferTable a ->  Bool 
oneproposalAll m oTable = (allProposers m) == (map fst oTable)

reducedList :: Eq a => OfferTable a -> PrefTable a -> PrefTable a 
reducedList [] m = m 
reducedList ((p,q):os) m = reducedList os (changepreferences (\_ -> prefGeqq) p m')
                         where prefs = getpreferences p m 
                               prefGeqq = takeWhile ((/=) q) prefs ++ [q]  
                               m' = iterChangeperefrences (delete p) (prefs \\ prefGeqq) m  


{-
Irving's Paper - (https://uvacs2102.github.io/docs/roomates.pdf)
An Efficient Algorithm for the “Stable Roommates” Problem
-}

phase1 :: Eq a => [a] -> OfferTable a -> PrefTable a -> Maybe (OfferTable a,PrefTable a) 
phase1 [] oTable m = return (oTable,m)
phase1 xc@(x:xs) oTable m = 
    case termination m oTable of 
        (True,_)  -> Nothing 
        (_,True)  -> return (oTable,m) 
        _         -> do 
            let tc = topchoice x m 
            case offerStatus x tc oTable m of 
                Reject -> phase1 xc oTable (changepreferences tail x m)
                NoEntryAccept oTable' -> phase1 xs oTable' m 
                -- top choice of x (i.e. tc) accepted x's offer rejecting the old one 
                BetterOfferAccept oTable' -> do 
                   let r = fromJust . lookup tc $ oTable 
                   phase1 (r:xs) oTable' (changepreferences tail r m)    

phase1WithReduction :: Eq a => PrefTable a -> Maybe (PrefTable a)
phase1WithReduction m = do 
    let ps = allProposers m 
    case phase1 ps [] m of 
        Nothing -> Nothing
        Just (oTable,m') -> return $ reducedList oTable m'


-- one choice remaining for every person  
onechoiceLeft :: PrefTable a -> Bool 
onechoiceLeft = foldMatch (\(_,x,_) -> (&&) (length x == 1)) True  

mutualreject :: Eq a => a -> a -> PrefTable a -> PrefTable a
mutualreject x y m = let m' = changepreferences (delete y) x m 
                     in changepreferences (delete x) y m' 

reduceWithCycle :: Eq a => [(a,a)] -> PrefTable a -> PrefTable a 
reduceWithCycle [] m = m
reduceWithCycle [(a,b)] m = mutualreject a b m 
reduceWithCycle ((a,b):xs) m = let m' = mutualreject a b m
                               in reduceWithCycle xs m'

trimPQList :: Eq a => a -> PQPair a -> [(a,a)]
trimPQList x (p:ps,qs)
  | x == p = zip ps (drop (length qs-length ps) qs)
  | otherwise = trimPQList x (ps,qs)

gencycle :: Eq a => a -> PrefTable a -> PQPair a -> [(a,a)]
gencycle x m (p,q) 
  | elem x p = trimPQList x (p++[x],q)
  | otherwise = gencycle lsnd m (p++[x],q++[sndx])
  where sndx = (getpreferences x m)!!1
        lsnd = last.getpreferences sndx $ m

-- first person with multiple choices 
multichoiceElem :: PrefTable a -> a 
multichoiceElem = head . foldMatch f [] 
    where f (x,y,_) acc = if length y > 1 then (x:acc) else acc   


irvings :: Eq a => PrefTable a -> Maybe (PrefTable a)
irvings m = case phase1WithReduction m of 
   Nothing -> Nothing 
   Just m' -> case onechoiceLeft m' of 
      True  -> return m'
      False -> do 
          let x  = multichoiceElem m'
          irvings (reduceWithCycle (gencycle x m' ([],[])) m')    



