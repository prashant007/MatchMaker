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


data CChoice = Cch {lctnPref::Rating,salary::Double,clgIntrst ::Level,specReput::Level}

data HChoice = Hch {examScore::Int, intervPerf::Level, prevRelExpr::Bool}
  
cval x y z w = Just $ Cch x y z w 
hval x y z = Just $ Hch x y z   

-- =============================================================================================
instance Relate Hospital Candidate HChoice where
    assignVal = info [Mercy --> mercyChoice, City --> cityChoice, General --> generalChoice] 

mercyChoice = [Joseph --> hval 700 Med True, Darrius --> hval 770 High True]

cityChoice  = [Arthur --> hval 790 Med True, Sunny --> hval 750 Med True, Joseph --> hval 690 Low False,
               Latha  --> hval 750 Low True, Darrius --> hval 800 VHigh True]

generalChoice = [Arthur  --> hval 790 Med True, Joseph --> hval 780 VLow False, Latha --> hval 690 VLow False, 
                 Darrius --> hval 770 VHigh True]


instance Relate Candidate Hospital CChoice where
    assignVal = info [Arthur --> arthurChoice, Sunny  --> sunnyChoice, Joseph --> josephChoice,
                      Latha  --> lathaChoice, Darrius --> darriusChoice]

arthurChoice = [City --> cval 5 90000 High VHigh]
sunnyChoice  = [City --> cval 6 90000 High Med, Mercy --> cval 6 80000 Med High]
josephChoice = [City --> cval 8 90000 High VHigh, Mercy --> cval 2 100000 Low Low,General --> cval 5 90000 High High]
lathaChoice  = [City --> cval 7 100000 High VHigh,Mercy --> cval 8 120000 VHigh VHigh,General --> cval 6 95000 High VHigh]
darriusChoice= [City --> cval 7 110000 VHigh High,Mercy --> cval 5 100000 High Med,General --> cval 6 90000 VLow Med]

-- =============================================================================================
-- =============================================================================================

instance Evaluable Level where
    norm = \case {VLow -> 0.2; Low -> 0.4 ; Med -> 0.6 ; High -> 0.8 ; VHigh -> 1.0 } 

instance Evaluable CChoice where
    norm (C l s c r) = madm [0.2,0.3,0.3,0.2] 
                                 [norm (l::Rating,10::Double),norm (s::Double,120000.0::Double),
                                  norm c, norm r]
instance Evaluable HChoice where
    norm (Hch m i p) = madm [0.4,0.3,0.3] [norm (m::Int,800::Double), norm i, norm p]

 
instance StableMarriage Candidate Hospital CChoice HChoice 
instance StableMarriage Hospital Candidate HChoice CChoice 



