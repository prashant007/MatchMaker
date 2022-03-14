{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, StandaloneDeriving #-}

module NRMPExample where

import MatchDatatype 
import DataType 


class (Show a,Ord a,Enum a,Bounded a) => EnumBounded a 


-- Roth 142 

data Applicant = S1 | S2 | S3 | S4 deriving (Eq,Show,Ord,Enum,Bounded,Set)
data Hospital  = H1 | H2 | H3 | H4 deriving (Eq,Show,Ord,Enum,Bounded,Set)



instance Set Hospital where
    capacity = every 2 

instance Relate Hospital Applicant Rank where
    gather = choices [H1 --> [S4,S2,S1,S3], 
                      H2 --> [S4,S3,S2,S1],
                      H3 --> [S2,S3,S1,S4],
                      H4 --> [S2,S4,S1,S3]]  

instance Relate Applicant Hospital Rank where
    gather = choicesL [[S1,S2] --> [[H1,H2],[H4,H2],[H4,H3],[H4,H2],[H1,H4],[H1,H3]
                                    [H3,H4],[H3,H1],[H3,H2],[H2,H3],[H2,H4],[H2,H1]], 
                       [S3,S4] --> [[H4,H2],[H4,H3],[H4,H1],[H3,H1],[H3,H2],[H3,H4],
                                    [H2,H4],[H2,H1],[H2,H3],[H1,H2],[H1,H4],[H1,H3]]]  


instance Relate Applicant Hospital Rank where
    gather = choicesL [s12 --> [h12,h42,h43,h42,h14,h13,h34,h31,h32,h23,h24,h21],
                       s34 --> [h42,h43,h41,h31,h32,h34,h24,h21,h23,h12,h14,h13]]  




s12 = [S1,S2]
s34 = [S3,S4]
[h12,h13,h14,h21,h23,h24,h31,h32,h34,h41,h42,h43] = [[x,y] | x <- member, y <- member, x \= y]

