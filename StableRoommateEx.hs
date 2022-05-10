{-# LANGUAGE MultiParamTypeClasses,DeriveAnyClass #-}

import DataType 
import Info 
import MatchDatatype


-- https://www.youtube.com/watch?v=9Lo7TFAkohE&ab_channel=OscarRobertson

-- data Student = Charlie | Peter | Elise | Paul | Kelly | Sam deriving (Eq,Show,Ord,Enum,Bounded,Set) 

-- instance Relate Student Student Rank where
--     gather = choices [Charlie--> [Peter,Paul,Sam,Kelly,Elise], 
--                       Peter  --> [Kelly,Elise,Sam,Paul,Charlie], 
--                       Elise  --> [Peter,Sam,Kelly,Charlie,Paul],
--                       Paul   --> [Elise,Charlie,Sam,Peter,Kelly] ,
--                       Kelly  --> [Peter,Charlie,Sam,Elise,Paul],
--                       Sam    --> [Charlie,Paul,Kelly,Elise,Peter]] 

data Student = Charlie | Peter | Kelly | Sam deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights) 

instance Preference Student Student Rank where
    gather = choices [Charlie--> [Peter,Sam,Kelly], 
                      Peter  --> [Kelly,Sam,Charlie], 
                      Kelly  --> [Peter,Charlie,Sam],
                      Sam    --> [Charlie,Kelly,Peter]] 


-- > sameSetMatch :: SameSetMatch Student
