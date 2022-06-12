{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}

module NRMP_ADT where

import TypeClasses
import Info 
import MatchType
import Combinators 
import MatchingFunctions

data Applicant = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord,Enum,Bounded,Set) 
data Hospital =  Mercy | City | General deriving (Eq,Show,Ord,Enum,Bounded) 

instance Set Hospital where
    quota = forall 2 

data AInfo = Appl {examScore :: Double, experience :: Double,interviewScore :: Double, cameForVisit::Bool} 

instance Show AInfo where
    show (Appl e ex i c) = "(" ++ show  e ++ "," ++ show ex ++ "," ++ show i ++ "," ++ show c ++ ")"

data HInfo  =  Hptl {hospitalRank :: Rank, cityLivability :: Int, desirabilityScore :: Double}

type DScore = Double 

hProfile :: Hospital -> DScore -> HInfo
hProfile h = case h of 
                    Mercy  -> Hptl (Rank 2) 9 
                    City   -> Hptl (Rank 1) 10
                    General-> Hptl (Rank 3) 8

desirability :: Info Applicant Hospital DScore
desirability =  info [Arthur --> [City --> 3],
                      Sunny  --> [Mercy --> 4,City --> 3],
                      Joseph --> [Mercy --> 1,City --> 5,General --> 4],
                      Latha  --> [Mercy --> 5,City --> 1,General --> 1],
                      Darrius--> [Mercy --> 5,City --> 5,General --> 4]]    

instance Preference Applicant Hospital HInfo where
  gather = hProfile `completedWith` desirability 

instance Norm AInfo where
    components (Appl e x i c) = [e `outOf` 800, 
                                 x `outOf` 10, 
                                 i `outOf` 10,
                                 only c]

instance Weights Hospital where
    weights x = case x of 
                    Mercy -> [0.3,0.3,0.3,0.1]
                    _     -> [0.2,0.2,0.6,0.0]                     


instance Norm HInfo where
    components (Hptl h c d) = [only h, c `outOf` 10, d `outOf` 5]

instance Weights Applicant where
    weights = forall [0.2,0.2,0.6] 

aProfile :: Applicant -> Double -> Bool -> AInfo
aProfile a = case a of 
                Arthur -> Appl 700 2 
                Sunny  -> Appl 720 2 
                Joseph -> Appl 750 1  
                Latha  -> Appl 650 5   
                Darrius-> Appl 790 2  

interview :: Info Hospital Applicant Double
interview = info [Mercy   --> [Joseph --> 8,Darrius --> 9],
                  City    --> [Arthur --> 10,Sunny --> 9,Joseph --> 4,Latha --> 6,Darrius--> 10], 
                  General --> [Arthur --> 9,Joseph --> 8,Latha --> 5,Darrius --> 10]]

school ::  Info Hospital Applicant Bool 
school = info [Mercy   --> [Joseph --> False,Darrius--> True],
               City    --> [Arthur --> True, Sunny  --> False,Joseph --> False,Latha --> False,Darrius--> False], 
               General --> [Arthur --> False,Joseph --> True,Latha --> False,Darrius --> False]]

instance Preference Hospital Applicant AInfo where
    gather = aProfile `completedWith2` (interview `zipInfo` school)

deltaInterview = info [Mercy --> [Sunny --> 8, Arthur --> 8],
                       City  --> [Sunny --> 9]]  

deltaSchool = info [Mercy --> [Sunny --> True, Arthur --> False],
                    City  --> [Sunny --> False]]  

interview1 = interview `updateWithInfo` deltaInterview
school1 = school `updateWithInfo` deltaSchool

updatedHosp = aProfile `completedWith2` (interview1 `zipInfo` school1)

-- ============== Getting the matchings ==========================

-- *Main> twoWay :: Match Applicant Hospital
-- {Arthur --> [City], Sunny --> [], Joseph --> [General], 
--  Latha  --> [General], Darrius --> [City]}

-- *Main> twoWayWithCapacity :: Match Hospital Applicant
-- {Mercy --> [] : 2, City --> [Arthur,Darrius] : 0, 
--  General --> [Latha,Joseph] : 0}

-- *Main> twoWayDiff updatedHosp gather
-- {Mercy --> [] => [Sunny]}