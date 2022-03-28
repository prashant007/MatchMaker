{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

module MDP where

import MatchDatatype 
import DataType 
import Info

-- https://towardsdatascience.com/markov-decision-processes-and-bellman-equations-45234cce9d25


type TransProb = Double 
type Reward    = Int 

data State = Start | S1 | S2 | S3 | S4 | Exit deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

zipInfo = undefined 

type TransTable a = Info a a TransProb
type RewardsTable a = Info a a Reward

transProbs :: TransTable State 
transProbs = trans [Start --> [S1 --> 0.5, S3   --> 0.5],
                    S1    --> [S2 --> 0.4, S4   --> 0.3, Start --> 0.3], 
                    S2    --> [S1 --> 0.2, Exit --> 0.8],
                    S3    --> [S4 --> 0.8, Start--> 0.2],
                    S4    --> [S1 --> 0.3, S3   --> 0.3, Exit --> 0.4],
                    Exit  --> []]

stateRewards :: RewardsTable State
stateRewards = rewards [Start --> [S1 --> 4, S3   --> 2],
                        S1    --> [S2 --> 2, S4   --> 4, Start --> 1], 
                        S2    --> [S1 --> 4, Exit --> 1],
                        S3    --> [S4 --> 2, Start--> 2],
                        S4    --> [S1 --> 4, S3   --> 2, Exit --> 1],
                        Exit  --> []]

instance Relate State State (TransProb,Reward) where
    gather = transProbs `zipInfo` stateRewards
