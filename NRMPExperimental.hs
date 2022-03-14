{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

module NRMPExample where

import DataType
import Info 

class Ord a => Fixed a b where
    profile :: Rec a b 
    profile = toRec []

class Fixed a   
    fixedInfo :: a -> Double 


instance Fixed AFixedInfo where
    fixedInfo = 



class (Ord a,Ord b,Fixed b c) => Relate a b c d | a b -> c d where

    gather :: Info a b d 
    gather = info []


data Applicant = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord,Enum,Bounded) 
data Hospital =  Mercy | City | General deriving (Eq,Show,Ord,Enum,Bounded) 

-- Default capacity 1
instance Set Applicant 

instance Set Hospital where
    capacity _ = 2 


-- These attributes have fixed values irrespective of the hospital they are applying to
data AInfo   =  Appl {examScore :: Double, experience :: Double,
                      interview :: Double}


instance Fixed AInfo where
    fixedInfo  = 

-- Values of these attributes are different for an applicant based on the college he applies to. 
data AVaryingInfo = Interview 

-- construct a profile map of applicants 
appProfile :: Double -> Double -> [(AFixedInfo,Double)]
appProfile exam expr = [ExamScore --> exam, Experience --> expr]

-- create profile records for applicants 
instance Fixed Applicant AFixedInfo where
   profile = toRec [Arthur  --> appProfile 800 2, Sunny --> appProfile 750 3, 
                    Joseph  --> appProfile 780 3, Latha --> appProfile 670 5, 
                    Darrius --> appProfile 670 4]

-- Hospital's collect data on applicants in two ways:
-- 1) From the profile data of the applicants
-- 2) Individually "gather" applicant data specific to them
instance Relate Hospital Applicant AFixedInfo AVaryingInfo where
   gather = info [Mercy --> mercyChoice, City --> cityChoice, General --> generalChoice]

interview :: Double -> Maybe [(AVaryingInfo,Double)]
interview x = Just [Interview --> x]

mercyChoice   = [Joseph --> interview 5, Darrius --> interview 7]

cityChoice    = [Arthur  --> interview 7.5, Sunny  --> interview 8,
                 Joseph  --> interview 4.5, Latha  --> interview 9.5 , 
                 Darrius --> interview 6.5]

generalChoice = [Arthur --> interview 7, Joseph  --> interview 9, 
                 Latha  --> interview 4, Darrius --> interview 7.5]


data HFixedInfo   = Salary | Reputation
data HVaryingInfo = Location | Interest 

hprofile :: Double -> Double -> [(HFixedInfo,Double)]
hprofile s r = [Salary --> s, Reputation --> r ]

hval :: Double -> Double -> [(HVaryingInfo,Double)]
hval l i = [Location --> l, Interest --> i]


instance Fixed Hospital HFixedInfo where
   profile = toRec [Mercy   --> hprofile 90000 9, City --> hprofile 85000 9,
                    General --> hprofile 93000 8]

instance Relate Applicant Hospital HFixedInfo HVaryingInfo where
   gather = info [Arthur --> arthurChoice, Sunny  --> sunnyChoice, Joseph --> josephChoice,
                  Latha  --> lathaChoice, Darrius --> darriusChoice]

arthurChoice = [City --> hval 5 10]
sunnyChoice  = [City --> hval 6 6, Mercy --> hval 6 8]
josephChoice = [City --> hval 8 10, Mercy --> hval 2 2, General --> hval 5 8]
lathaChoice  = [City --> hval 7 10, Mercy --> hval 8 10, General --> hval 6 10]
darriusChoice= [City --> hval 7 8, Mercy --> hval 5 6, General --> hval 6 6]



{-
Similarly, the repeated use of "interview" in the definitions of mercyChoice, etc. 
can be factored.
-}

-- **************************
-- old representation

-- Values of these attributes are different for an applicant based on the college he applies to. 
data AVaryingInfo = Interview | GPA 

interview' :: 

citychoice = interview [Arthur --> (2,3) , Sunny --> ] 

cityChoice = [Arthur  --> interview 7.5, Sunny  --> interview 8,
              Joseph  --> interview 4.5, Latha  --> interview 9.5 , 
              Darrius --> interview 6.5]

interview :: Double -> Maybe [(AVaryingInfo,Double)]
interview x = Just [Interview --> x]

instance Relate Hospital Applicant AFixedInfo AVaryingInfo where
   gather = info [Mercy --> mercyChoice, City --> cityChoice, General --> generalChoice]

-- **************************
-- new representation 

interview  :: [Double] -> [(Applicant,Maybe [(AVaryingInfo,Double)])] 
interview = zipWith (\x -> Just [Interview --> x]) members


-- members = [Arthur,Sunny,Joseph,Latha,Darrius]


instance Relate Hospital Applicant AFixedInfo AVaryingInfo where
   gather = info [Mercy   --> interview [0,0,5,0,7], 
                  City    --> interview [7.5,8,4.5,9.5,6.5], 
                  General --> interview [7.5,8,4.5,9.5,6.5]
                 ]


{-
 However, the entries "Mercy --> mercyChoice", "City --> cityChoice", etc. 
 look suspiciously redundant and can probably be improved? 
-}





{-
One thing that probably should also be discussed is that for some applications
the use of an enumeration type for entities to be matched is probably not appropriate. 
Especially in a realistic version of the hospital example, applicants would probably be
identified by some ID number.
-}