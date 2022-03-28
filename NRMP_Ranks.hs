{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, StandaloneDeriving #-}

module NRMPExample where

import MatchDatatype 
import DataType 


class (Show a,Ord a,Enum a,Bounded a) => EnumBounded a 

data Applicant = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

data Hospital = City | Mercy | General deriving (Eq,Show,Ord,Enum,Bounded,Weights)



instance Set Hospital where
    capacity = every 2 

instance Relate Hospital Applicant Rank where
    gather = choices [Mercy   --> [Darrius,Joseph], 
                      City    --> [Darrius,Arthur,Sunny,Latha,Joseph],
                      General --> [Darrius,Arthur,Joseph,Latha]]  

instance Relate Applicant Hospital Rank where
    gather = choices [Arthur  --> [City], 
                      Sunny   --> [City,Mercy], 
                      Joseph  --> [City,General,Mercy],
                      Latha   --> [Mercy,City,General],
                      Darrius --> [City,Mercy,General]]  

*NRMPExample> twoWay :: Match Hospital Applicant
{General --> [Latha,Joseph], Mercy --> [], City --> [Arthur,Darrius]}

*NRMPExample> twoWay::Match Applicant Hospital
{Sunny --> [], Darrius --> [City], Latha --> [General], Joseph --> [General], Arthur --> [City]}

-- ======================================================================

-- *NRMPExample> twoWay::Match Applicant Hospital
-- {Sunny --> [], Darrius --> [City], Latha --> [General], Joseph --> [General], Arthur --> [City]}

-- *NRMPExample> oneWay :: Match Applicant Hospital
-- {Arthur --> [City], Sunny --> [Mercy], Joseph --> [General], Latha --> [], Darrius --> []}

-- *NRMPExample> twoWayExpl Mercy Darrius
-- {General --> [Latha,Joseph], Mercy --> [Darrius], City --> [Sunny,Arthur]}