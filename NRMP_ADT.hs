{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}

import DataType
import Info 
import MatchDatatype
import qualified Data.Map as M 

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
 
interview :: Info Hospital Applicant Double
interview = info [Mercy   --> [Joseph --> 8, Darrius --> 9],
                  City    --> [Arthur -->10,Sunny --> 9,Joseph --> 8,Latha --> 6,Darrius--> 10], 
                  General --> [Arthur --> 9,Joseph --> 8,Latha --> 5,Darrius --> 10]]

             
instance Relate Hospital Applicant AInfo where
    gather = appProfile `completeWith` interview

 
hospProfile :: Hospital -> Double -> HInfo
hospProfile h = case h of 
                    Mercy  -> Hptl (Rank 2) 9 
                    City   -> Hptl (Rank 1) 10
                    General-> Hptl (Rank 2) 8

desirability :: Info Applicant Hospital Double
desirability =  info [Arthur --> [City --> 3],
                      Sunny  --> [Mercy --> 4,City --> 3],
                      Joseph --> [Mercy --> 1,City --> 5,General --> 4],
                      Latha  --> [Mercy --> 5,City --> 1,General --> 1],
                      Darrius--> [Mercy --> 5,City --> 5,General --> 4]]    


instance Relate Applicant Hospital HInfo where
  gather = hospProfile `completeWith` desirability 


instance Norm AInfo where
    components (Appl e x i) = map norm [e `with` 800, x `with` 10, i `with` 10]

instance Weights Hospital where
    weights x = case x of 
                    Mercy -> [0.3,0.3,0.4]
                    _     -> [0.2,0.2,0.6]                     


instance Norm HInfo where
    components (Hptl h c d) = [norm $ only h, norm $ c `with` 10, norm $ d `with` 5]

instance Weights Applicant where
    weights = every [0.2,0.2,0.6] 



-- weights x = every' members



every' :: [a] -> [Double]
every' xs = replicate n d 
    where n = length xs 
          d = 1.0/(fromIntegral n)

-- instance Norm HInfo where 
--     norm (Hptl h c d) = weight [norm h                  --> 0.2,
--                                 norm (c::Int,10::Int)   --> 0.2,
--                                 norm (d::Double,5::Int) --> 0.6]




-- data AInfo  =  Appl {examScore :: Double, experience :: Double,interviewScore :: Double, cameForVisit::Bool}

-- applProfile :: Applicant -> Double -> Bool -> AInfo
-- applProfile a = case a of 
--                   Arthur -> Appl 700 2 
--                   Sunny  -> Appl 720 2 
--                   Joseph -> Appl 750 1  
--                   Latha  -> Appl 650 5   
--                   Darrius-> Appl 790 2  


-- applVariable :: Info Hospital Applicant (Double,Bool)
-- applVariable = info [Mercy   --> [Joseph --> (8,True),  Darrius--> (9,False)],
--                      City    --> [Arthur --> (10,False),Sunny  --> (9,False),
--                                   Joseph --> (8,True),  Latha  --> (6,False),
--                                   Darrius--> (10,True)], 
--                      General --> [Arthur --> (9,True),  Joseph --> (8,True),
--                                   Latha  --> (5,False), Darrius--> (10,True)]]


-- instance Relate Hospital Applicant AInfo where
--     gather = applProfile `completeWith2` applVariable


-- combine :: Info a b c -> Info a b d -> Info a b (c,d)
-- combine x y = undefined    

-- interview :: Info Hospital Applicant Double
-- interview = info [Mercy   --> [Joseph --> 8, Darrius --> 9],
--                   City    --> [Arthur -->10,Sunny --> 9,Joseph --> 8,Latha --> 6,Darrius--> 10], 
--                   General --> [Arthur --> 9,Joseph --> 8,Latha --> 5,Darrius --> 10]]


-- visit ::  Info Hospital Applicant Bool 
-- visit = info [Mercy   --> [Joseph --> True,Darrius --> False],
--               City    --> [Arthur --> True,Sunny --> True,Joseph --> False,Latha --> False,Darrius--> True], 
--               General --> [Arthur --> True,Joseph --> False,Latha --> False,Darrius --> False]]


-- instance Relate Hospital Applicant AInfo where
--     gather = applProfile `completeWith2` (interview `combine` visit)



