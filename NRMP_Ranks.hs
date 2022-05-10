{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}

module NRMPExample where

import TypeClasses
import Info 
import MatchType
import Combinators 
import MatchingFunctions

class (Show a,Ord a,Enum a,Bounded a) => EnumBounded a 

data Applicant = Arthur | Sunny | Joseph | Latha | Darrius | Bob deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

data Hospital = City | Mercy | General deriving (Eq,Show,Ord,Enum,Bounded,Weights)

instance Set Hospital where
    capacity = every 2 

instance Preference Hospital Applicant Rank where
    gather = choices [Mercy   --> [Darrius,Joseph], 
                      City    --> [Darrius,Arthur,Sunny,Latha,Joseph],
                      General --> [Darrius,Arthur,Joseph,Latha]]  

instance Preference Applicant Hospital Rank where
    gather = choices [Arthur  --> [City], 
                      Sunny   --> [City,Mercy], 
                      Joseph  --> [City,General,Mercy],
                      Latha   --> [Mercy,City,General],
                      Darrius --> [City,Mercy,General]]  




-- updatedHosp = gather `modWithRanks` (Mercy --> [Darrius,Joseph,Sunny,Arthur,Latha])
-- updatedApp  = gather `modWithRanks` (Sunny --> [City,Mercy,General])
--                   `modWithRanks` (Latha --> [City,Mercy,General])
-- deltaInfo = choices [Sunny  --> [City,Mercy,General], 
--                      Latha --> [City,Mercy,General]]
-- updateA = gather `modWithInfo` deltaInfo


updatedApp  = gather `modWithRanks` (Bob --> [Mercy,General,City])
updatedHosp = gather `modWithRanks` (Mercy  --> [Darrius,Joseph,Sunny])
                     `modWithRanks` (General--> [Darrius,Arthur,Joseph,Latha])


deltaInfo = choices [Mercy  --> [Darrius,Bob,Joseph], 
                     General--> [Bob,Darrius,Arthur,Joseph,Latha]]
updateA = gather `modWithInfo` deltaInfo



-- newInfoMen = gather `modWithRanks` (M1 --> [W4,W1,W3])
-- newInfoWomen = gather `modWithRanks` (W4 --> [M2,M1]) 


-- *NRMPExample> twoWay :: Match Hospital Applicant
-- {General --> [Latha,Joseph], Mercy --> [], City --> [Arthur,Darrius]}

