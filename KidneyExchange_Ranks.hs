{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}

import TypeClasses
import Info 
import MatchType
import Combinators 
import MatchingFunctions

data Donor   = Alice | Bob | Dan | Dillon  deriving (Eq,Show,Ord,Enum,Bounded,Set) 
data Patient = P1 | P2 | P3 | P4 deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights) 
             
instance Preference Patient Donor Rank where
    gather = choices [P1 --> [Bob,Dan,Dillon],  P2 --> [Alice,Dan,Dillon], 
                      P3 --> [Alice,Bob,Dillon],P4 --> [Alice,Bob,Dan]]


x1 = oneWay' [P3,P4,P2,P1] :: Match Patient Donor 

instance Exchange Patient Donor Rank where
    endowment = [P1 --> Bob, P2 --> Dan, P3 --> Alice, P4 --> Dillon]


