{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances,DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

module NRMPExample where

import DataType
import Info 
import MDS 

data Applicant = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord,Enum,Bounded) 
data Hospital = City | Mercy | General deriving (Eq,Show,Ord,Enum,Bounded) 

instance Set Applicant 

instance Set Hospital where
    capacity _ = 2 


data AChoice = AChoice {lctnPref :: Rating, salary :: Double,
                        clgIntrst :: Level, specReput :: Level} deriving Show

data HChoice = HChoice {examScore :: Int, intervPerf :: Level, prevRelExpr::Bool} deriving Show
 
aval :: Rating -> Double -> Level -> Level -> Maybe AChoice   
aval x y z w = Just $ AChoice x y z w 

hval :: Int -> Level -> Bool -> Maybe HChoice
hval x y z = Just $ HChoice x y z   


-- instance Profile Applicant ApplicantData where
--   basic = info [Joseph --> [GPA --> , Publications --> ]] 

-- =============================================================================================
instance Relate Hospital Applicant HChoice where
    gather = info [Mercy --> mercyChoice, City --> cityChoice, General --> generalChoice] 

mercyChoice   = [Joseph --> hval 700 Med True, Darrius --> hval 770 High True]

cityChoice    = [Arthur --> hval 790 Med True, Sunny --> hval 750 Med True,
                 Joseph --> hval 690 Low False, Latha  --> hval 750 Low True, 
                 Darrius --> hval 800 VHigh True]

generalChoice = [Arthur  --> hval 790 Med True, Joseph --> hval 780 VLow False, 
                 Latha --> hval 690 VLow False, Darrius --> hval 770 VHigh True]


instance Relate Applicant Hospital AChoice where
    gather = info [Arthur --> arthurChoice, Sunny  --> sunnyChoice, Joseph --> josephChoice,
                   Latha  --> lathaChoice, Darrius --> darriusChoice]

arthurChoice = [City --> aval 5 90000 High VHigh]
sunnyChoice  = [City --> aval 6 90000 High Med, Mercy --> aval 6 80000 Med High]
josephChoice = [City --> aval 8 90000 High VHigh, Mercy --> aval 2 100000 Low Low,
                General --> aval 5 90000 High High]
lathaChoice  = [City --> aval 7 100000 High VHigh,Mercy --> aval 8 120000 VHigh VHigh,
                General --> aval 6 95000 High VHigh]
darriusChoice= [City --> aval 7 110000 VHigh High,Mercy --> aval 5 100000 High Med,
                General --> aval 6 90000 VLow Med]

-- =============================================================================================
-- =============================================================================================

instance Norm Level where
    components x = [case x of {VLow -> 0.2; Low -> 0.4 ; Med -> 0.6 ; High -> 0.8 ; VHigh -> 1.0}] 

instance Norm AChoice where
    components (AChoice l s c r)
      = weight [0.2,0.3,0.3,0.2] [norm (l::Rating,10::Int), norm (s::Double,120000.0::Double),
                                  norm c, norm r]
      
      = weight [(l::Rating,10::Int) --> 0.2,
                (s::Double,120000.0::Double) --> 0.3,
                c --> 0.3,
                r --> 0.2]

instance Norm HChoice where
    components (HChoice m i p) = weight [0.4,0.3,0.3] [norm (m::Int,800::Int), norm i, norm p]

instance TwowayMatch Applicant Hospital AChoice HChoice 
instance TwowayMatch Hospital Applicant HChoice AChoice 

-- instance Labels AChoice where
--   labels _ = ["Location Preference", "Salary", "Interest in College","Specialization's Reputation"] 

data HLabel = ExamScore | IntervPerf | PrevRelExpr deriving (Ord,Show,Eq,Set,Bounded,Enum) 

-- instance Annotate HChoice String where
--   labels = ["Exam Score", "Interview Performance", "Previous Relevant Experience"] 

instance Annotate HChoice HLabel where
  labels = [ExamScore,IntervPerf,PrevRelExpr] 


applicantInfo  = gather :: Info Hospital Applicant HChoice
expArthurSunny = explain City (Arthur,Sunny) applicantInfo :: Explanation Applicant HLabel  