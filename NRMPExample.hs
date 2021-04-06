{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,  InstanceSigs, LambdaCase, DefaultSignatures #-}

module NRMPExample where

import DataType

data Candidate = Arthur | Sunny | Joseph | Latha | Darrius deriving (Eq,Show) 
data Hospital = City | Mercy | General deriving (Eq,Show) 

instance Set Candidate where
    members = [Arthur,Sunny,Joseph,Latha,Darrius]

instance MatchSet Hospital where
    members = [City,Mercy,General]
    capacity _ = 2 


data CChoice = C {lctnPref::Rating,salary::Double,clgIntrst ::Level,specReput::Level}
data HChoice = H {examScore::Int, intervPerf::Level, prevRelExpr::Bool}

-- =============================================================================================

cch :: Rating -> Double -> Level -> Level -> Maybe CChoice
cch l s c r = return $ C {lctnPref=l,salary=s,clgIntrst=c,specReput=r}

candidateCh = expressCh [City,Mercy,General] 

arthursChoice = candidateCh [cch 5 90000 High VHigh,Nothing,Nothing]
sunnysChoice  = candidateCh [cch 6 90000 High Med,cch 6 80000 Med High,Nothing]
josephsChoice = candidateCh [cch 8 90000 High VHigh,cch 2 100000 Low Low,cch 5 90000 High High]
lathasChoice  = candidateCh [cch 7 100000 High VHigh,cch 8 120000 VHigh VHigh,cch 6 95000 High VHigh]
darriusChoice = candidateCh [cch 7 110000 VHigh High,cch 5 100000 High Med,cch 6 90000 VLow Med]

instance Relate Candidate Hospital CChoice where
    assignVal = \case {Arthur -> arthursChoice;Sunny -> sunnysChoice;Joseph -> josephsChoice;
                       Latha  -> lathasChoice;Darrius -> darriusChoice}

-- =============================================================================================
hch :: Int -> Level -> Bool -> Maybe HChoice
hch e i p = return $ H {examScore = e,intervPerf=i,prevRelExpr=p}

hospitalCh = expressCh [Arthur,Sunny,Joseph,Latha,Darrius]

mercysChoice = hospitalCh [Nothing,Nothing,hch 700 Med True,Nothing,hch 770 High True]
citysChoice = hospitalCh [hch 790 Med True,hch 750 Med True,hch 690 Low False,hch 750 Low True,hch 800 VHigh True]
generalsChoice = hospitalCh [hch 790 Med True,Nothing,hch 780 VLow False,hch 690 VLow False,hch 770 VHigh True]

instance Relate Hospital Candidate HChoice where
    assignVal = \case {Mercy -> mercysChoice; City -> citysChoice;General -> generalsChoice}  

instance Evaluable CChoice where
    eval (C l s c r) = madm [0.2,0.3,0.3,0.2] [evalR l,g s,evalL c,evalL r]
         where
            g = \v -> v/120000.0

instance Evaluable HChoice where
    eval (H m i p) = madm [0.4,0.3,0.3] [g m,evalL i,evalB p]
         where
            g = \v -> (fromIntegral v)/800.0
 
 
instance StableMarriage Candidate Hospital CChoice HChoice 
instance StableMarriage Hospital Candidate HChoice CChoice 


