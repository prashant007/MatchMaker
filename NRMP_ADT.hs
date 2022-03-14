{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}

import DataType
import Info 
import MatchDatatype

data Applicant = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord,Enum,Bounded,Set) 
data Hospital =  Mercy | City | General deriving (Eq,Show,Ord,Enum,Bounded) 

instance Set Hospital where
    capacity = every 2 

data AInfo  =  Appl {examScore :: Double, experience :: Double,interviewScore :: Double}
data HInfo  =  Hptl {hospitalRank :: Rank, cityLivability :: Int, desirabilityScore :: Double}

appProfile :: Applicant -> Double -> AInfo
appProfile a = case a of 
                Arthur -> Appl 700 2 
                Sunny  -> Appl 720 2 
                Joseph -> Appl 750 1                         
                Latha  -> Appl 650 5   
                Darrius-> Appl 790 2  


interview  = completeWith appProfile 

-- completeWith :: Ord a => (a -> b -> c) -> [(a,b)] -> Rec a c 
-- completeWith f = toRec . map (\(a,i) -> (a,Just $ f a i)) 

             
instance Relate Hospital Applicant AInfo where
  gather = info [Mercy   --> interview [Joseph --> 8,Darrius --> 9],
                 City    --> interview [Arthur -->10,Sunny --> 9,Joseph --> 8,Latha --> 6,Darrius--> 10],
                 General --> interview [Arthur --> 9,Joseph --> 8,Latha --> 5,Darrius --> 10]]  

instance Norm AInfo where 
    components (Appl e x i) = weight [norm (e::Double,800::Int) --> 0.3,
                                      norm (x::Double,10::Int)  --> 0.3,
                                      norm (i::Double,10::Int)  --> 0.4]

hospProfile :: Hospital -> Double -> HInfo
hospProfile h = case h of 
                    Mercy  -> Hptl (Rank 2) 9 
                    City   -> Hptl (Rank 1) 10
                    General-> Hptl (Rank 2) 8

desirability = completeWith hospProfile

instance Relate Applicant Hospital HInfo where
  gather = info [Arthur --> desirability [City --> 3],
                 Sunny  --> desirability [Mercy --> 4,City --> 3],
                 Joseph --> desirability [Mercy --> 1,City --> 5,General --> 4],
                 Latha  --> desirability [Mercy --> 5,City --> 1,General --> 1],
                 Darrius--> desirability [Mercy --> 5,City --> 5,General --> 4]]  

instance Norm HInfo where 
    norm (Hptl h c d) = weight [norm h                  --> 0.2,
                                norm (c::Int,10::Int)   --> 0.2,
                                norm (d::Double,5::Int) --> 0.6]
























