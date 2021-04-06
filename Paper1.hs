{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances, DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

import DataType
import Info 

data Candidate = Prashant | Divya | Daniel deriving (Eq,Show,Ord,Enum,Bounded,Set) 
data Paper = Paper2 | Paper3 | Paper20 deriving (Eq,Show,Ord,Enum,Bounded,Set) 

instance Relate Candidate Paper Rank where
    gather = info [Prashant --> prashantChoice, Divya  --> divyaChoice, Daniel --> danielChoice] 

prashantChoice = [Paper2 --> rank 1, Paper3 --> rank 2, Paper20 --> rank 3]
divyaChoice  = [Paper2 --> rank 2, Paper3 --> rank 1, Paper20 --> rank 3]
danielChoice = [Paper2 --> rank 3, Paper3 --> rank 1, Paper20 --> rank 2]

instance Relate Paper Candidate Rank where
    gather = info [Paper2 --> p2Choice,Paper3 --> p3Choice, Paper20 --> p20Choice] 

p2Choice  = [Prashant --> rank 1, Divya --> rank 2, Daniel --> rank 3]
p3Choice  = [Prashant --> rank 3, Divya --> rank 1, Daniel --> rank 2]
p20Choice = [Prashant --> rank 2, Divya --> rank 3, Daniel --> rank 1]

data Rank = Rank Int 
  
rank = Just . Rank

instance Norm Rank where
    norm (Rank r) = (1/fromIntegral r)


instance StableMatch Candidate Paper Rank Rank 
instance StableMatch Paper Candidate Rank Rank 

-- *NRMPExample> showMatch (solveP :: Match Candidate Hospital)