{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, StandaloneDeriving #-}

module NRMPExample where

import MatchDatatype 
import DataType 
import Info 


class (Show a,Ord a,Enum a,Bounded a) => EnumBounded a 

-- data Men = M1 | M2 | M3 | M4 | M5 | M6 deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

-- data Women =  W1 | W2 | W3 | W4 | W5 | W6 deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

-- instance Relate Men Women Rank where
--     gather = choices [M1 --> [W1,W3], M2 --> [W2,W4], M3 --> [W4,W3],
--                       M4 --> [W3,W4], M5 --> [W5], M6 --> [W1,W4]]  

-- instance Relate Women Men Rank where
--     gather = choices [W1 --> [M2,M1,M6], W2 --> [M6,M1,M2], W3 --> [M3,M4,M1,M5],
--                       W4 --> [M4,M3,M2], W5 --> [M5]]  

-- newInfo :: Info Men Women Rank
-- newInfo = gather `modWith` (choices [M1 --> [W1,W3,W2], M2 --> [W2,W4,W1], M3 --> [W4,W3,W2],
--                                      M5 --> [W5,W3], M6 --> [W1,W4,W2]])  

-- newInfo1 :: Info Men Women Rank
-- newInfo1 = gather `modWithInfo` (choices [M1 --> [W6,W1,W3,W2], M2 --> [W2,W4,W1],
--                                           M3 --> [W4,W3,W6,W2], M5 --> [W5,W3,W6],
--                                           M6 --> [W1,W4,W2]])  

-- newInfo2 :: Info Women Men Rank
-- newInfo2 = gather `modWithRanks` (W6 --> [M1,M2,M3]) 

data Men = M1 | M2 | M3  deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

data Women =  W1 | W2 | W3 | W4  deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

instance Relate Men Women Rank where
    gather = choices [M1 --> [W1,W3], M2 --> [W3,W2], M3 --> [W1,W3]]  

instance Relate Women Men Rank where
    gather = choices [W1 --> [M1,M3], W2 --> [M2], W3 --> [M3,M2] ]  

newInfoMen = gather `modWithRanks` (M1 --> [W4,W1,W3])
newInfoWomen = gather `modWithRanks` (W4 --> [M2,M1]) 
