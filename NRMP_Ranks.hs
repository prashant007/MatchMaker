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
    quota = forall 2 

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

updatedApp  = gather `modWithRanks` (Bob --> [Mercy,General,City])
updatedHosp = gather `modWithRanks` (Mercy  --> [Darrius,Joseph,Sunny])
                     `modWithRanks` (General--> [Darrius,Arthur,Joseph,Latha])


deltaInfo = choices [Mercy  --> [Darrius,Bob,Joseph], 
                     General--> [Bob,Darrius,Arthur,Joseph,Latha]]
updateA = gather `modWithInfo` deltaInfo

-- ============== matching results =======================================
-- *NRMPExample> twoWay :: Match Hospital Applicant
-- {General --> [Latha,Joseph], Mercy --> [], City --> [Arthur,Darrius]}

-- *NRMPExample> twoWayWithPref updatedHosp updatedApp
-- {City --> [Arthur,Darrius], Mercy --> [Sunny], General --> [Latha,Joseph]}