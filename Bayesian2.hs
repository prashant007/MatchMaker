{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, StandaloneDeriving, TypeSynonymInstances, FunctionalDependencies, FlexibleInstances #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


data Cloudy = Yes | No 

data Sprinkler = On | Off 

-- type Wetgrass  = Cloudy 
-- type Rain      = Cloudy 

data Rain = Rain | NoRain
data Wetgrass = Wet | NotWet 

data None = None 

class Parents a b | a -> b where
  cpt :: a -> b -> Double 


instance Parents Cloudy None where
  cpt x None = case x of 
                   Yes -> 0.5
                   No  -> 0.5  

instance Parents Sprinkler Cloudy where
  cpt x y = case (x,y) of 
                (On,Yes) -> 0.9
                (Off,Yes)-> 0.1
                (On,No)  -> 0.5  
                (Off,No) -> 0.5 

instance Parents Rain Cloudy where
  cpt x y = case (x,y) of 
                (Rain,Yes)   -> 0.8
                (NoRain,Yes) -> 0.2
                (Rain,No)    -> 0.2  
                (NoRain,No)  -> 0.8

instance Parents Wetgrass (Sprinkler,Rain) where
  cpt x y = case x of 
              Wet    -> case y of 
                         (On,NoRain) -> 0.90
                         (On,Rain)   -> 0.99
                         (Off,NoRain)-> 0.01
                         (Off,Rain)  -> 0.90 
              NotWet ->  case y of 
                         (On,NoRain) -> 0.10
                         (On,Rain)   -> 0.01
                         (Off,NoRain)-> 0.99
                         (Off,Rain)  -> 0.10 


-- type family Parents1 x where
--   Parents1 Cloudy   = '[None]
--   Parents1 Sprinkler= '[Cloudy]
--   Parents1 Rain     = '[Cloudy]
--   Parents1 Wetgrass = '[Sprinkler,Rain]


-- x =  Parents1 Cloudy 


-- class Query a b where
--   query :: a -> b 