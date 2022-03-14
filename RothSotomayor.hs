{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}


module RothSotomayor where

import MatchDatatype 
import DataType 


data Men   = M1 | M2 | M3 | M4 deriving (Eq,Show,Ord,Enum,Bounded,Set)
data Women = W1 | W2 | W3 | W4 deriving (Eq,Show,Ord,Enum,Bounded,Set)


instance Relate Men Women Rank where
    gather = choices [M1 --> [W1,W2,W3,W4], 
                      M2 --> [W2,W1,W4,W3],
                      M3 --> [W3,W4,W1,W2],
                      M4 --> [W4,W3,W2,W1]]  

instance Relate Women Men Rank where
    gather = choices [W1 --> [M4,M3,M2,M1], 
                      W2 --> [M3,M4,M1,M2], 
                      W3 --> [M2,M1,M4,M3],
                      W4 --> [M1,M2,M3,M4]]  
                   
