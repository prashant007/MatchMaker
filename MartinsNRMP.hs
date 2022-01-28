{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, FunctionalDependencies #-}

import DataType
import Info 

data Applicant = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord,Enum,Bounded) 
data Hospital =  Mercy | City | General deriving (Eq,Show,Ord,Enum,Bounded) 

data AInfo  =  Appl {examScore :: Double, experience :: Double,interviewScore :: Double}


profile' :: Applicant -> Double -> AInfo
profile' a = case a of 
                Arthur -> Appl 700 2 
                Sunny  -> Appl 750 3 
                Joseph -> Appl 780 0  
                Latha  -> Appl 650 5   
                Darrius-> Appl 660 2  

interview :: [(Applicant,Double)] -> Rec Applicant AInfo 
interview = toRec . map (\(a,i) -> (a,Just $ profile' a i)) 

               
-- defaultVal = 0 

-- profile' :: Applicant -> AInfo
-- profile' a = case a of 
--                 Arthur -> Appl 700 2 defaultVal 
--                 Sunny  -> Appl 750 3 defaultVal 
--                 Joseph -> Appl 780 0 defaultVal
--                 Latha  -> Appl 650 5 defaultVal    
--                 Darrius-> Appl 660 2 defaultVal

modInfo1 :: AInfo -> Double -> AInfo 
modInfo1 (Appl x y _) z = Appl x y z

-- interview :: [(Applicant,Double)] -> Rec Applicant AInfo
-- interview = toRec . map (\(a,i) -> (a, Just $ modInfo1 (profile' a) i)) 


instance Relate Hospital Applicant AInfo where
  gather = info [Mercy   --> interview [Joseph --> 8,Darrius --> 9],
                 City    --> interview [Arthur -->10,Sunny --> 9,Joseph --> 8,Latha --> 5,Darrius--> 8],
                 General --> interview [Arthur --> 7,Joseph --> 8,Latha --> 9,Darrius --> 7]]  



-- instance Norm AInfo where 
--     components (Appl e x i) = weight [0.4,0.3,0.3] [norm (e::Int,800::Int),
--                                                     norm (x::Double,10::Int),
--                                                     norm (i::Double,10::Int)]

weight' :: [(Double,Double)] ->  [Double] 
weight' xs = weight (map fst xs) (map snd xs)

instance Norm AInfo where 
    components (Appl e x i b) = weight' [norm (e::Double,800::Int) --> 0.4,
                                         norm (x::Double,10::Int)  --> 0.2,
                                         norm (i::Double,10::Int)  --> 0.2,
                                         norm b                    --> 0.2]





























class Ord a => Profile a b c where
   profile1 :: a -> b -> c 

   mkProfile :: [(a,b)] ->  Rec a c 
   mkProfile = toRec . map (\(a,i) -> (a,Just $ profile1 a i)) 


instance Profile Applicant Double AInfo where
    profile1 a = case a of 
                    Arthur -> Appl 700 2 
                    Sunny  -> Appl 750 3 
                    Joseph -> Appl 780 0  
                    Latha  -> Appl 650 5    
                    Darrius-> Appl 660 2  

-- interview :: Rec Applicant AInfo
-- interview = mkProfile 
