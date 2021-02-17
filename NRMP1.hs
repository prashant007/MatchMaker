{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

module NRMPExample where

import DataType
import Info 

data Candidate = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord) 
data Hospital = City | Mercy | General deriving (Eq,Show,Ord) 

instance MatchSet Candidate where
    members = [Arthur,Sunny,Joseph,Latha,Darrius]

instance MatchSet Hospital where
    members = [City,Mercy,General]
    capacity _ = 2 


data NRank = NRank Int 
  
rank = Just . NRank


-- =============================================================================================
instance Relate Hospital Candidate NRank where
    assignVal = info [Mercy --> mercyChoice, City --> cityChoice, General --> generalChoice] 

mercyChoice = [Joseph --> rank 2, Darrius --> rank 1]
cityChoice  = [Arthur --> rank 2, Sunny --> rank 3, Joseph --> rank 5, Latha  --> rank 4, Darrius --> rank 1]
generalChoice = [Arthur  --> rank 2, Joseph --> rank 4, Latha --> rank 4, Darrius --> rank 1]


instance Relate Candidate Hospital NRank where
    assignVal = info [Arthur --> arthurChoice, Sunny  --> sunnyChoice, Joseph --> josephChoice,
                      Latha  --> lathaChoice, Darrius --> darriusChoice]

arthurChoice = [City --> rank 1]
sunnyChoice  = [City --> rank 1, Mercy --> rank 2]
josephChoice = [City --> rank 1, Mercy --> rank 3,General --> rank 2]
lathaChoice  = [City --> rank 2,Mercy --> rank 1,General --> rank 3]
darriusChoice= [City --> rank 1,Mercy --> rank 2,General --> rank 3]

-- =============================================================================================
-- =============================================================================================

instance Evaluable NRank where
    norm (NRank r) = (1/fromIntegral r)

 
instance StableMarriage Candidate Hospital NRank NRank 
instance StableMarriage Hospital Candidate NRank NRank 



