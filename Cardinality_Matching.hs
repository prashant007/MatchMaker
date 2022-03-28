{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, FlexibleInstances #-}


import DataType
import Info 
import MatchDatatype
import qualified Data.Map as M 


data LSet = A | B | C | D | E deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)
data RSet = F | G | H | I | J deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

instance Relate LSet RSet Rank where
    gather = choices [A --> [F,H,J], B --> [G,H,I], C --> [F,G], D --> [J], E --> [G,I]]  


data Buyers = Bayer | Glazer | Qatar | RedBull deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)
data Teams = LasVegas | Vancouver | Seattle | Dayton deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)  

type Bid = Double

instance Relate Buyers Teams Bid where
    gather = info [Bayer   --> [LasVegas --> 80, Vancouver --> 65],
                   Glazer  --> [LasVegas --> 60, Vancouver --> 40, Seattle --> 50], 
                   Qatar   --> [Seattle  --> 50, Dayton --> 4], 
                   RedBull --> [Seattle  --> 48, Dayton --> 2]]  