{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}


module StableMarriageExample where

import MatchDatatype 
import DataType 


-- data Men   = M1 | M2 | M3 | M4 | M5 deriving (Eq,Show,Ord,Enum,Bounded,Set)
-- data Women = W1 | W2 | W3 | W4 | W5 deriving (Eq,Show,Ord,Enum,Bounded,Set)


-- instance Relate Men Women Rank where
--     gather = choices [M1 --> [W3,W2,W5,W1,W4], 
--                       M2 --> [W1,W2,W5,W3,W4],
--                       M3 --> [W4,W3,W2,W1,W5],
--                       M4 --> [W1,W3,W4,W2,W5],
--                       M5 --> [W1,W2,W4,W5,W3]]  

-- instance Relate Women Men Rank where
--     gather = choices [W1 --> [M3,M5,M2,M1,M4], 
--                       W2 --> [M5,M2,M1,M4,M3], 
--                       W3 --> [M4,M3,M5,M1,M2],
--                       W4 --> [M1,M2,M3,M4,M5],
--                       W5 --> [M2,M3,M4,M1,M5]]  
                   


data Men = C | E | J | R deriving (Eq,Show,Ord,Enum,Bounded,Set) 

data Women = A | G | M | I deriving (Eq,Show,Ord,Enum,Bounded,Set)


instance Relate Men Women Rank where
    gather = choices [C --> [G,M,I,A], 
                      E --> [M,A,G,I],
                      J --> [A,I,M,G],
                      R --> [I,G,A,M]]  

instance Relate Women Men Rank where
    gather = choices [A --> [E,C,R,J], 
                      G --> [E,J,C,R], 
                      M --> [J,C,E,R],
                      I --> [R,E,J,C]]  
                   


*StableMarriageExample> twoWay :: Match Men Women
{R --> [I], J --> [A], E --> [M], C --> [G]}


*StableMarriageExample> twoWayExpl J M
{R --> [I], E --> [A], C --> [G], J --> [M]}

-- data Men   = M1 | M2 | M3  deriving (Eq,Show,Ord,Enum,Bounded,Set)
-- data Women = W1 | W2 | W3  deriving (Eq,Show,Ord,Enum,Bounded,Set)


-- instance Relate Men Women Rank where
--     gather = choices [M1 --> [W1,W2,W3], 
--                       M2 --> [W1,W3,W2],
--                       M3 --> [W3,W1,W2]]  

-- instance Relate Women Men Rank where
--     gather = choices [W1 --> [M1,M2,M3], 
--                       W2 --> [M2,M1,M3], 
--                       W3 --> [M3,M1,M2]]  
                   



-- *StableMarriageExample> twoWay :: Match Men Women
-- {M1 --> [W5], M2 --> [W2], M5 --> [W1], M4 --> [W3], M3 --> [W4]}

-- *StableMarriageExample> twoWay :: Match Women Men
-- {W5 --> [M2], W4 --> [M1], W3 --> [M4], W2 --> [M5], W1 --> [M3]}



-- {M4 --> [W5], M2 --> [W2], M5 --> [W1], M3 --> [W4], M1 --> [W3]}