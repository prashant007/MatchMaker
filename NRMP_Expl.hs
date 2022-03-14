{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}

module NRMPExample where

import MatchDatatype 
import DataType 


data Undergrads = Anton | Bob | Javier | Lindell | Donatello deriving (Eq,Show,Ord,Enum,Bounded,Set)
data Grads      = Chekov | Morientes | Giavani deriving (Eq,Show,Ord,Enum,Bounded)

instance Set Grads where
    capacity = every 2 

instance Relate Grads Undergrads Rank where
    gather = choices [Morientes --> [Donatello,Javier], 
                      Chekov    --> [Donatello,Anton,Bob,Lindell,Javier],
                      Giavani   --> [Donatello,Anton,Javier,Lindell]]  

instance Relate Undergrads Grads Rank where
    gather = choices [Anton   -->   [Chekov], 
                      Bob      -->  [Chekov,Morientes], 
                      Javier    --> [Chekov,Giavani,Morientes],
                      Lindell   --> [Morientes,Chekov,Giavani],
                      Donatello --> [Chekov,Morientes,Giavani]]  
                    


-- *NRMPExample> twoWayMatch :: Match Grads Undergrads
-- Giavani:
--                  Matched with [Lindell,Javier]
--                  Remaining capacity: 0
-- Morientes:
--                  Matched with []
--                  Remaining capacity: 2
-- Chekov:
--                  Matched with [Anton,Donatello]
--                  Remaining capacity: 0


-- Question: why is Donatello not assigned to me (Morientes)? 

-- Answer: Donatello likes Chekov    more than Morientes  
--         Chekov    likes Donatello more than Bob
--         this creates an unstability as they can leave their pairings

-- *NRMPExample> twoWayExpl Morientes Donatello
-- Giavani:
--                  Matched with [Lindell,Javier]
--                  Remaining capacity: 0
-- Morientes:
--                  Matched with [Donatello]
--                  Remaining capacity: 1
-- Chekov:
--                  Matched with [Bob,Anton]
--                  Remaining capacity: 0


-- *NRMPExample> twoWayMatch :: Match Undergrads Grads
-- Bob:
--                  Matched with []
--                  Remaining capacity: 1
-- Donatello:
--                  Matched with [Chekov]
--                  Remaining capacity: 0
-- Lindell:
--                  Matched with [Giavani]
--                  Remaining capacity: 0
-- Javier:
--                  Matched with [Giavani]
--                  Remaining capacity: 0
-- Anton:
--                  Matched with [Chekov]
--                  Remaining capacity: 0




-- Question: why am I (Lindell) not assigned to Chekov? 

-- Answer  :  Chekov likes Anton more than Lindell 
--            Anton is not assigned to anyone currenly
--            this creates an unstability as they can leave their pairings 


-- *NRMPExample> twoWayExpl Lindell Chekov
-- Donatello:
--                  Matched with [Chekov]
--                  Remaining capacity: 0
-- Anton:
--                  Matched with []
--                  Remaining capacity: 1
-- Javier:
--                  Matched with [Giavani]
--                  Remaining capacity: 0
-- Bob:
--                  Matched with []
--                  Remaining capacity: 1
-- Lindell:
--                  Matched with [Chekov]
--                  Remaining capacity: 0 


                 