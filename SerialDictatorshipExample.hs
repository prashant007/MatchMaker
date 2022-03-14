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






   -- endowment = [Prashant --> Paper2, Daniel --> Paper1, Jack --> Paper3]

-- *OneWayMatchExample> oneWayMatch :: Match Person Paper
-- Prashant:
--                  Matched with [Paper1]
--                  Remaining capacity: 0
-- Daniel:
--                  Matched with [Paper2]
--                  Remaining capacity: 0
-- Jack:
--                  Matched with [Paper3]
--                  Remaining capacity: 0