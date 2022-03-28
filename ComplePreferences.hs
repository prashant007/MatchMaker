{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, StandaloneDeriving, FlexibleInstances #-}

module ComplexPreferences where

import MatchDatatype 
import DataType 
import Info 

data Firm = I | J deriving (Eq,Show,Ord,Enum,Bounded,Weights)

data Worker = W1 | W2 | W3 deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

instance Set Firm where
    capacity = every 3

[w1,w2,w3] = map (\x -> x:[]) [W1,W2,W3]
[w12,w13,w23] = [[W1,W2],[W1,W3],[W2,W3]]
w123 = [W1,W2,W3]

type Money = Double 

instance Relate Firm [Worker] Money where
    gather = info [I --> [w1  --> 700,  w2  --> 1400, w3   --> 800, w12 --> 2100,
                          w13 --> 1500, w23 --> 2200, w123 --> 3200],

                   J --> [w1  --> 600,  w2  --> 1500, w3   --> 1100, w12 --> 2100,
                          w13 --> 1700, w23 --> 2600, w123 --> 2900]]  

class Relate a [b] c => Acceptable a b c where
    acceptable :: Info a b c 

instance Acceptable Firm Worker Money where
    acceptable = info [ I --> [W1 --> 400, W2 --> 700,  W3 --> 400],
                        J --> [W1 --> 300, W2 --> 1000, W3 --> 700]]





                        