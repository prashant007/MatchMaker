{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}

import DataType
import Info 
import MatchDatatype
import qualified Data.Map as M 

data Donor   = Alice | Bob | Dan | Dillon  deriving (Eq,Show,Ord,Enum,Bounded,Set) 
data Patient = Tom   | Rob | Meg | Jon deriving (Eq,Show,Ord,Enum,Bounded,Set) 

data DInfo  =  DInfo {age :: NDouble, overAllHealth:: Double, tissueCompat:: Double}

donorProfile :: Donor -> Double -> DInfo
donorProfile a = case a of 
                    Alice -> DInfo (ND 30) 9 
                    Bob   -> DInfo (ND 40) 7 
                    Dan   -> DInfo (ND 20) 8  
                    Dillon-> DInfo (ND 60) 6   
 
tissueScore :: Info Patient Donor Double
tissueScore = info [Tom --> [Bob   --> 8, Dan --> 9, Dillon --> 1],
                    Rob --> [Alice --> 2, Dan --> 9, Dillon --> 6], 
                    Meg --> [Alice --> 8, Bob --> 6, Dillon --> 7],
                    Jon --> [Alice --> 4, Bob --> 7, Dan    --> 10]]

             
instance Relate Patient Donor DInfo where
    gather = donorProfile `completeWith` tissueScore

instance Norm DInfo where 
    components (DInfo e x i) = [norm $ e `with` 20, norm $ x `with` 10, norm $ i `with` 10]

instance Weights Patient where
    weights = every [0.3,0.3,0.4]






