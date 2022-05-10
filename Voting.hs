{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, StandaloneDeriving #-}

module NRMPExample where

import MatchDatatype 
import DataType 


data Votes = Votes Int deriving (Eq,Show,Ord,Weights)

data Candidates = A | B | C | D deriving (Eq,Show,Ord,Enum,Bounded)

instance Relate Votes Candidates Rank where
    gather = choices [Votes 3  --> [A,B,C,D], Votes 5  --> [A,C,B,D], Votes 7  --> [B,D,C,A], Votes 6 --> [C,B,D,A]]  

