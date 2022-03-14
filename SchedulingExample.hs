{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}

module SchedulingExample where

import MatchDatatype 
import DataType 


data Professors = Martin | Eric | Alan | Prasad | Margaret | Rakesh deriving (Eq,Ord)

data TimeSlots  = Mon_11_12 | Mon_2_3 | Wed_1_2 | Fri_12_1 deriving (Eq,Ord)

instance Schedule Professors TimeSlots where
    constraints  = [LeastOne --> [Martin,Eric],
                    All      --> [Alan,Prasad]]

    availability = choices [Martin   --> [Mon_11_12,Wed_1_2,Fri_12_1], 
                            Eric     --> [Mon_2_3  ,Wed_1_2,Fri_12_1],
                            Alan     --> [Mon_11_12,Mon_2_3,Wed_1_2],
                            Prasad   --> [Mon_11_12,Wed_1_2,Fri_12_1],
                            Margaret --> [Mon_2_3  ,Wed_1_2,Fri_12_1],
                            Rakesh   --> [Mon_11_12,Mon_2_3,Fri_12_1]]  