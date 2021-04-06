{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

module NRMPExample where

import DataType 
import Info 

data Applicant = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord,Enum,Bounded) 
data Hospital = City | Mercy | General deriving (Eq,Show,Ord,Enum,Bounded)

instance Set Applicant 

instance Set Hospital where
    capacity _ = 2 



-- =============================================================================================
instance Relate Hospital Applicant Rank where
    gather = info [Mercy --> mercyChoice, City --> cityChoice, General --> generalChoice] 

mercyChoice, cityChoice, generalChoice :: [(Applicant,Maybe Rank)]
mercyChoice = [Joseph --> rank 2, Darrius --> rank 1]
cityChoice  = [Arthur --> rank 2, Sunny --> rank 3, Joseph --> rank 5, Latha  --> rank 4, Darrius --> rank 1]
generalChoice = [Arthur --> rank 2, Joseph --> rank 4, Latha --> rank 4, Darrius --> rank 1]

instance Relate Applicant Hospital Rank where
    gather = info [Arthur --> arthurChoice, Sunny  --> sunnyChoice, Joseph --> josephChoice,
                   Latha  --> lathaChoice, Darrius --> darriusChoice]

arthurChoice = [City --> rank 1]
sunnyChoice  = [City --> rank 1, Mercy --> rank 2]
josephChoice = [City --> rank 1, Mercy --> rank 3,General --> rank 2]
lathaChoice  = [City --> rank 2,Mercy --> rank 1,General --> rank 3]
darriusChoice= [City --> rank 1,Mercy --> rank 2,General --> rank 3]

-- =============================================================================================
-- =============================================================================================

instance TwowayMatch1 Applicant Hospital  
instance TwowayMatch1 Hospital Applicant  



