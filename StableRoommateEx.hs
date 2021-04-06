{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances, DeriveAnyClass, LambdaCase, DefaultSignatures,TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}

import DataType 
import Info 

-- instance Set Student 

data Student = Bob | Joe | Zack | Dave | Will | Tony deriving (Eq,Show,Ord,Enum,Bounded,Set) 

instance Relate Student Student Rank where
    gather = info [Bob --> bobsChoice,  Joe --> joesChoice, 
                   Zack --> zacksChoice,Dave --> davesChoice,
                   Will --> willsChoice, Tony --> tonysChoice] 

bobsChoice, joesChoice, zacksChoice, davesChoice, willsChoice, tonysChoice :: [(Student,Maybe Rank)]
bobsChoice  = [Zack --> rank 1,Dave --> rank 2,Joe --> rank 3,
               Tony --> rank 4,Will --> rank 5]

joesChoice  = [Tony --> rank 1,Will --> rank 2,Dave --> rank 3,
               Bob --> rank 4, Zack --> rank 5]

zacksChoice = [Joe  --> rank 1,Dave --> rank 2,Will --> rank 3,
               Bob --> rank 4,Tony --> rank 5]

davesChoice = [Will --> rank 1, Joe --> rank 2,Zack --> rank 3,
               Tony --> rank 4, Bob --> rank 5]

willsChoice = [Zack --> rank 1,Bob --> rank 2,Joe --> rank 3,
               Dave --> rank 4, Tony --> rank 5]

tonysChoice = [Will --> rank 1,Bob --> rank 2,Zack --> rank 3,
               Dave --> rank 4, Joe --> rank 5]

instance TwowayMatch1 Student Student 
instance TwowayMatch Student Student Rank Rank  

