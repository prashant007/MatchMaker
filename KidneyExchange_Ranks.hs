{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}

import TypeClasses
import Info 
import MatchType
import Combinators 
import MatchingFunctions
import OneWayMatching

data Donor   = Alice | Bob | Dan | Dillon  deriving (Eq,Show,Ord,Enum,Bounded,Set) 
data Patient = P1 | P2 | P3 | P4 deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights) 
             
instance Preference Patient Donor Rank where
    gather = choices [P1 --> [Bob,Dan,Dillon],  P2 --> [Alice,Dan,Dillon], 
                      P3 --> [Alice,Bob,Dillon],P4 --> [Alice,Dillon,Dan]]


x1 = oneWay' [P3,P4,P2,P1] :: Match Patient Donor 


instance Exchange Patient Donor where
    endowment = assign [P1 --> Bob, P2 --> Dillon,
                        P3 --> Alice, P4 --> Dan]


data Members = Anju | Prashant | Surabhi deriving  (Eq,Show,Ord,Enum,Bounded,Set,Weights)  
data Items = Computer | Freeze | Bag deriving (Eq,Show,Ord,Enum,Bounded,Set) 

instance Preference Members Items Rank where
    gather = choices [Prashant --> [Freeze,Computer], 
                      Anju     --> [Bag,Freeze], 
                      Surabhi  --> [Computer,Bag]]


instance Exchange Members Items where
    endowment = assign [Prashant --> Computer, Anju --> Freeze, Surabhi --> Bag]



data Agents = I1 | I2 | I3 | I4 deriving  (Eq,Show,Ord,Enum,Bounded,Set,Weights)  
data Houses = H1 | H2 | H3 | H4 deriving (Eq,Show,Ord,Enum,Bounded,Set) 

instance Preference Agents Houses Rank where
    gather = choices [I1 --> [H1,H3,H4,H2], 
                      I2 --> [H1,H3,H4,H2], 
                      I3 --> [H2,H4,H3,H1],
                      I4 --> [H2,H3,H4,H1]]


instance Exchange Agents Houses where
    endowment = assign [I1 --> H1, I2 --> H2, I3 --> H3, I4 --> H4]

