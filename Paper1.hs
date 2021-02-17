{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

import DataType
import Info 

data Candidate = Prashant | Divya | Daniel deriving (Eq,Show,Ord) 
data Paper = Paper2 | Paper3 | Paper20 deriving (Eq,Show,Ord) 


class (Bounded a,Enum a,Ord a) => Set a where
  members :: [a]
  members = enumFromTo minBound maxBound

instance MatchSet Candidate where
    members = [Prashant,Divya,Daniel]

instance MatchSet Paper where
    members = [Paper2,Paper3,Paper20]


instance Relate Candidate Paper RankC where
    assignVal = info [Prashant --> prashantChoice, Divya  --> divyaChoice, Daniel --> danielChoice] 

prashantChoice = [Paper2 --> rank 1, Paper3 --> rank 2, Paper20 --> rank 3]
divyaChoice  = [Paper2 --> rank 2, Paper3 --> rank 1, Paper20 --> rank 3]
danielChoice = [Paper2 --> rank 3, Paper3 --> rank 1, Paper20 --> rank 2]

instance Relate Paper Candidate RankC where
    assignVal = info [Paper2 --> p2Choice,Paper3 --> p3Choice, Paper20 --> p20Choice] 

p2Choice = [Prashant --> rank 1, Divya --> rank 2, Daniel --> rank 3]
p3Choice  = [Prashant --> rank 3, Divya --> rank 1, Daniel --> rank 2]
p20Choice = [Prashant --> rank 2, Divya --> rank 3, Daniel --> rank 1]


data RankC = RankC Int 
  
rank = Just . RankC


instance Evaluable RankC where
    norm (RankC r) = (1/fromIntegral r)

instance StableMarriage Candidate Paper RankC RankC 
instance StableMarriage Paper Candidate RankC RankC 