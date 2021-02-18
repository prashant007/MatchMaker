{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

module NRMPExample where

import DataType hiding (Rank)
import Info 

data Candidate = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord,Enum,Bounded) 
data Hospital = City | Mercy | General deriving (Eq,Show,Ord,Enum,Bounded)

instance MatchSet Candidate 

instance MatchSet Hospital where
    capacity _ = 2 


data Rank = Rank Int 
  
rank = Just . Rank


-- =============================================================================================
instance Relate Hospital Candidate Rank where
    assignVal = info [Mercy --> mercyChoice, City --> cityChoice, General --> generalChoice] 

mercyChoice, cityChoice, generalChoice :: [(Candidate,Maybe Rank)]
mercyChoice = [Joseph --> rank 2, Darrius --> rank 1]
cityChoice  = [Arthur --> rank 2, Sunny --> rank 3, Joseph --> rank 5, Latha  --> rank 4, Darrius --> rank 1]
generalChoice = [Arthur  --> rank 2, Joseph --> rank 4, Latha --> rank 4, Darrius --> rank 1]

instance Relate Candidate Hospital Rank where
    assignVal = info [Arthur --> arthurChoice, Sunny  --> sunnyChoice, Joseph --> josephChoice,
                      Latha  --> lathaChoice, Darrius --> darriusChoice]

arthurChoice = [City --> rank 1]
sunnyChoice  = [City --> rank 1, Mercy --> rank 2]
josephChoice = [City --> rank 1, Mercy --> rank 3,General --> rank 2]
lathaChoice  = [City --> rank 2,Mercy --> rank 1,General --> rank 3]
darriusChoice= [City --> rank 1,Mercy --> rank 2,General --> rank 3]

-- =============================================================================================
-- =============================================================================================

instance Normalizable Rank where
    norm (Rank r) = (1/fromIntegral r)

 
instance StableMatch Candidate Hospital Rank Rank 
instance StableMatch Hospital Candidate Rank Rank 



