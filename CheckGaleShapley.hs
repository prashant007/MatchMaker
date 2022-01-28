{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

import DataType 
import Info 

instance Set Men 
instance Set Women 

data Men = Cole | Jack | Ken | Larry deriving (Eq,Show,Ord,Enum,Bounded) 
data Women = Gail | Heather | Jane | Maggie deriving (Eq,Show,Ord,Enum,Bounded) 

instance Relate Men Women Rank where
    gather = info [Cole --> colesChoice, Jack --> jacksChoice, 
                   Ken  --> kensChoice, Larry --> larrysChoice]

colesChoice = [Gail --> rank 4, Heather --> rank 1, Jane --> rank 2, Maggie --> rank 3]
jacksChoice = [Gail --> rank 1, Heather --> rank 3, Jane --> rank 2, Maggie --> rank 4]
kensChoice  = [Gail --> rank 1, Heather --> rank 4, Jane --> rank 3, Maggie --> rank 2]
larrysChoice= [Gail --> rank 3, Heather --> rank 2, Jane --> rank 2, Maggie --> rank 1]

instance Relate Women Men Rank where
    gather = info [Gail --> gailsChoice, Heather --> heatherChoice, 
                   Jane --> janesChoice,  Maggie --> maggiesChoice]

gailsChoice  = [Cole --> rank 1, Jack --> rank 4, Ken --> rank 3, Larry --> rank 2] 
heatherChoice= [Cole --> rank 2, Jack --> rank 1, Ken --> rank 4, Larry --> rank 3] 
janesChoice  = [Cole --> rank 4, Jack --> rank 2, Ken --> rank 1, Larry --> rank 2] 
maggiesChoice= [Cole --> rank 4, Jack --> rank 3, Ken --> rank 1, Larry --> rank 2] 

instance TwowayMatch1 Men Women
instance TwowayMatch1 Women Men 

data Boys = Joe | Derek | Zack | Dave deriving (Eq,Show,Ord,Enum,Bounded)   
data Girls= Karen | Judy | Penny | Beatrice deriving (Eq,Show,Ord,Enum,Bounded) 

instance Set Boys 
instance Set Girls 

instance Relate Boys Girls Rank where
    gather = info [Joe  --> joesChoice, Derek --> dereksChoice, 
                   Zack --> zacksChoice, Dave --> davesChoice]

joesChoice   = [Karen --> rank 1, Judy --> rank 2, Penny --> rank 3, Beatrice --> rank 4]
dereksChoice = [Karen --> rank 2, Judy --> rank 1, Penny --> rank 3, Beatrice --> rank 4]
zacksChoice  = [Karen --> rank 3, Judy --> rank 4, Penny --> rank 1, Beatrice --> rank 2]
davesChoice  = [Karen --> rank 4, Judy --> rank 3, Penny --> rank 2, Beatrice --> rank 1]

instance Relate Girls Boys Rank where
    gather = info [Karen --> karensChoice, Judy --> judysChoice, 
                   Penny --> pennysChoice, Beatrice --> beatricesChoice]

beatricesChoice = [Joe --> rank 2, Derek --> rank 1, Zack --> rank 4, Dave --> rank 3] 
judysChoice  = [Joe --> rank 2, Derek --> rank 4, Zack --> rank 1, Dave --> rank 3] 
karensChoice = [Joe --> rank 3, Derek --> rank 2, Zack --> rank 4, Dave --> rank 1] 
pennysChoice = [Joe --> rank 1, Derek --> rank 3, Zack --> rank 2, Dave --> rank 4]  

instance TwowayMatch1 Girls Boys
instance TwowayMatch1 Boys Girls 