{-# LANGUAGE MultiParamTypeClasses,DeriveAnyClass #-}

module OneWayMatchExample where

import MatchDatatype 
import DataType 


data Person = Prashant | Daniel | Jack deriving (Eq,Show,Ord,Enum,Bounded,Set)
data Paper  = Paper1 | Paper2 | Paper3 deriving (Eq,Show,Ord,Enum,Bounded,Set)

instance Relate Person Paper Rank where
     gather = choices [Prashant --> [Paper1,Paper2,Paper3], 
                       Daniel   --> [Paper2,Paper1,Paper3],
                       Jack     --> [Paper1,Paper3,Paper2]]  


instance Relate Person Paper Rank where
     endowment = [Prashant --> Paper2, Daniel --> Paper1, Jack --> Paper3]

     gather = choices [Prashant --> [Paper1,Paper2,Paper3], 
                       Daniel   --> [Paper2,Paper1,Paper3],
                       Jack     --> [Paper1,Paper3,Paper2]]


data Person = Prashant | Daniel | Jack | Iain | Nick deriving (Eq,Show,Ord,Enum,Bounded,Set)
data Paper  = Paper1 | Paper2 | Paper3 | Paper4 | Paper5 deriving (Eq,Show,Ord,Enum,Bounded,Set)

instance Relate Person Paper Rank where
     endowment = [Prashant --> Paper2, Daniel --> Paper1, Jack --> Paper3]

     gather = choices [Prashant --> [Paper5,Paper1,Paper2,Paper3,Paper4], 
                       Daniel   --> [Paper2,Paper1,Paper3,Paper5,Paper4],
                       Jack     --> [Paper1,Paper3,Paper2,Paper4,Paper5],
                       Iain     --> [Paper1,Paper3,Paper2,Paper4,Paper5],
                       Nick     --> [Paper1,Paper2,Paper3,Paper4,Paper5]]