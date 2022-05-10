{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}

import DataType
import Info 
import MatchDatatype
import qualified Data.Map as M 

data Student  = S1 | S2 | S3 | S4  deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights) 
data Lecturer = L1 | L2 deriving (Eq,Show,Ord,Enum,Bounded,Weights) 
data Project  = P1 | P2 | P3 | P4 deriving (Eq,Show,Ord,Enum,Bounded) 

instance Set Lecturer where
    capacity = every 2 

instance Set Project where
    capacity x
        | elem x [P1,P3] = 2
        | otherwise = 1
 
instance Preference Student Project Rank where
    gather = choices [S1 --> [P1,P2,P3,P4],S2 --> [P1,P4,P3,P2], 
                      S3 --> [P3,P1,P2,P4],S4 --> [P3,P2,P1,P4]]

instance Preference Lecturer Student Rank where
    gather = choices [L1 --> [S3,S4,S1,S2],L2 --> [S1,S2,S3,S4]] 

instance Project Lecturer Project Rank where
    gather = choices [L1 --> [P1,P2], L2 --> [P3,P4]]


project_SPAS :: (Preference a b c,Preference d a e,Preference d b Rank) =>  Match a b 
project_SPAS = undefined 

project_SPAP :: (Preference a b c,Preference d b e) =>  Match a b 
project_SPAP = undefined 