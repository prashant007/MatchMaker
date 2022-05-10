{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

module BayesianExample where

import MatchDatatype 
import DataType 
import Info 

cpt :: Ord a => [(a,b)] -> Rec a b 
cpt = toRec . map (\(x,y) -> (x,Just y))

type Prob = Double
type CPT a = Rec [a] Prob 

t = [True]
f = [False]
[tt,tf,ft,ff] =  [[x,y] | x <- [True,False],y <- [True,False]]
[ttt,ttf,tft,tff,ftt,ftf,fft,fff] =  [[x,y,z] | x <- [True,False],y <- [True,False],z <- [True,False]]


data Node = Cloudy | Rain | Wetgrass | Sprinkler | None deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

none = []
cloudy = [Cloudy]
sprinklerNRain = [Sprinkler,Rain]


instance Relate Node [Node] (CPT Bool) where
    gather = info [Cloudy    --> [none           --> cptCloudy],
                   Sprinkler --> [cloudy         --> cptSprinkler], 
                   Rain      --> [cloudy         --> cptRain],
                   Wetgrass  --> [sprinklerNRain --> cptWetgrass]]


cptCloudy :: CPT Bool  
cptCloudy = cpt [t --> 0.5,f --> 0.5]

cptSprinkler :: CPT Bool 
cptSprinkler = cpt [tt --> 0.9, tf --> 0.1,
                    ft --> 0.5, ff --> 0.5]

cptRain :: CPT Bool 
cptRain = cpt [tt --> 0.8, tf --> 0.2,
               ft --> 0.2, ff --> 0.8]

cptWetgrass :: CPT Bool 
cptWetgrass = cpt [ttt --> 0.99, ttf --> 0.01, 
                   tft --> 0.90, tff --> 0.10,
                   ftt --> 0.90, ftf --> 0.10,
                   fft --> 0.01, fff --> 0.99]



                   