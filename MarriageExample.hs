{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies, InstanceSigs,LambdaCase,DefaultSignatures #-}

module MarraigeExample where

import DataType
import Data.List 
import Data.Maybe

data Man = Bob | Ben | Jack deriving (Eq,Show) 

data Woman = Alice | Jane | Jill | Eli | Ann deriving (Eq,Show)

instance MatchSet Man where
    members = [Bob,Ben,Jack]

instance MatchSet Woman where
    members = [Alice,Jane,Jill,Eli,Ann]

-- =============================================================================================

-- culturalSimi => cultural similarity,ageCompt => age comptability
data MChoice = M {culturalSimi::Level,ageCompt::Rating,mlooks :: Rating} deriving Show

-- politicsSimi => similarity of politics,intcompt => intellectual compatibiltiy
data WChoice = W {politicsSimi :: Bool,intcompt :: Rating,wlooks::Rating} deriving Show 

-- =============================================================================================

mch :: Level -> Rating -> Rating -> Maybe MChoice
mch l a m = return $ M {culturalSimi = l, ageCompt = a, mlooks = m}

mensCh = expressCh [Alice,Jane,Jill,Eli,Ann] 

bobsChoice = mensCh [mch High 10 10,Nothing,mch High 8 10,mch VHigh 9 8, mch Med 10 10]
bensChoice = mensCh [mch High 8 6,mch VHigh 10 10,mch High 6 8,mch High 4 6,mch Med 7 6]
jacksChoice = mensCh [mch Low 7 8,mch Low 2 3,mch Med 6 8,mch VHigh 9 8,mch Med 9 10]

instance Relate Man Woman MChoice  where
    assignVal = \case {Bob -> bobsChoice;Ben -> bensChoice;Jack -> jacksChoice} 

-- =============================================================================================

wch :: Bool -> Rating -> Rating -> Maybe WChoice 
wch b i w = return $ W {politicsSimi=b,intcompt=i,wlooks=w}

womensCh = expressCh [Bob,Ben,Jack]

alicesChoice= womensCh [wch False 7 7,wch True 10 10,wch True 8 7]
janesChoice = womensCh [wch True 10 7,wch True 10 10,wch True 8 7]
jillsChoice = womensCh [wch True 8 10,wch True 9 9,wch False 10 9]
elisChoice = womensCh [wch True 7 10,wch False 10 4,wch True 4 9]
annsChoice = womensCh [wch True 8 9, wch True 7 9,wch False 9 9]

instance Relate Woman Man WChoice  where
    assignVal = \case {Alice -> alicesChoice;Jane -> janesChoice;Jill -> jillsChoice;Eli -> elisChoice;Ann -> annsChoice}

-- =============================================================================================

-- take a choice made by a man and convert it to a number using linear MADM 
instance Evaluable MChoice where
    eval (M x y z) = madm [0.2,0.4,0.4] [evalL x,evalR y,evalR z]

instance Evaluable WChoice where
    eval (W x y z) = madm [0.3,0.3,0.4] [evalB x,evalR y,evalR z]


instance StableMarriage Man Woman MChoice WChoice         
instance StableMarriage Woman Man WChoice MChoice         

