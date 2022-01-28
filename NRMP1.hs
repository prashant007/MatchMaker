--{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances #-}
--{-# LANGUAGE TypeSynonymInstances, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module NRMPExample where

import DataType 
import Info 

data Applicant = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show,Ord,Enum,Bounded) 
data Hospital = City | Mercy | General deriving (Eq,Show,Ord,Enum,Bounded)

instance Set Applicant 
instance Set Hospital where
    capacity _ = 2 

instance Relate Hospital Applicant Rank where
    gather = choices [Mercy   --> [Darrius,Joseph], City --> [Darrius,Arthur,Sunny,Latha,Joseph],
                      General --> [Darrius,Arthur,Joseph,Latha]]  

instance Relate Applicant Hospital Rank where
    gather = choices [Arthur --> [City], Sunny --> [City,Mercy], Joseph --> [City,General,Mercy],
                      Latha  --> [Mercy,City,General], Darrius --> [City,Mercy,General]]  
                    
instance TwowayMatchWithRank Applicant Hospital   
instance TwowayMatchWithRank Hospital Applicant

-- instance Relate Hospital Applicant Rank where
--     gather = choices [Mercy   --> [Darrius,Joseph], City --> [Darrius,Arthur,Sunny,Joseph],
--                       General --> [Darrius,Arthur,Joseph]
--                      ]  

-- instance Relate Applicant Hospital Rank where
--     gather = choices [Arthur --> [City,General], Sunny --> [City,Mercy], Joseph --> [City,General,Mercy],
--                       Darrius --> [City,Mercy,General]
--                      ]  

