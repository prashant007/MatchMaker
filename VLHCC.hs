{-# LANGUAGE MultiParamTypeClasses #-}

module NRMPExample where

import DataType 
import Info 

data Reviewer = Divya | Prashant | Kai | Daniel | Ben | Alex | Jack deriving (Eq,Show,Ord,Enum,Bounded) 
data Paper = Paper09 | Paper18 | Paper35 | Paper48 | Paper54 | Paper70 deriving (Eq,Show,Ord,Enum,Bounded)

-- Every reviewers is assigned 2 papers
instance Set Reviewer where
    capacity = every 2

-- Every paper is assigned 2 reviewers
instance Set Paper where
    capacity = every 1 

-- =============================================================================================



instance Relate Reviewer Paper Rank where
    gather = choices [Divya    --> [Paper54,Paper48,Paper09,Paper35,Paper18,Paper70], 
                      Prashant --> [Paper09,Paper54,Paper48,Paper18,Paper35,Paper70],
                      Kai      --> [Paper18,Paper09,Paper35,Paper54,Paper70,Paper48],
                      Daniel   --> [Paper54,Paper09,Paper18,Paper70,Paper35,Paper48],
                      Ben      --> [Paper35,Paper54,Paper18,Paper09,Paper48,Paper70],
                      Alex     --> [Paper54,Paper09,Paper70,Paper35,Paper18,Paper48],
                      Jack     --> [Paper35,Paper54,Paper70,Paper09,Paper48,Paper18]
                     ]  

instance Relate Paper Reviewer Rank where
    gather = choices [Paper09 --> [Prashant,Daniel,Kai,Alex,Divya,Ben,Jack], 
                      Paper18 --> [Kai,Daniel,Ben,Prashant,Divya,Alex,Jack],
                      Paper35 --> [Ben,Jack,Kai,Divya,Alex,Prashant,Daniel],
                      Paper48 --> [Divya,Prashant,Ben,Jack,Kai,Daniel,Alex],
                      Paper54 --> [Daniel,Alex,Divya,Prashant,Jack,Ben,Kai],
                      Paper70 --> [Jack,Alex,Daniel,Kai,Divya,Ben,Prashant]
                     ]  


instance TwowayMatchWithRank Reviewer Paper  
instance TwowayMatchWithRank Paper Reviewer 