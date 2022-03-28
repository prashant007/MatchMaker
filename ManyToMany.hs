{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, FlexibleInstances #-}


import DataType
import Info 
import MatchDatatype
import qualified Data.Map as M 

-- Roth 154 

data Worker  = W1 | W2 | W3 | W4 deriving (Eq,Show,Ord,Enum,Bounded,Weights)
data Factory = F1 | F2 | F3 | F4 deriving (Eq,Show,Ord,Enum,Bounded,Weights)

instance Set Worker where
    capacity = every 2 

instance Set Factory where
    capacity = every 2 


[f1,f2,f3,f4] = map (\x -> [x]) [F1,F2,F3,F4]
[w1,w2,w3,w4] = map (\x -> [x]) [W1,W2,W3,W4]


[w12,w13,w14,w21,w23,w24,w31,w32,w34,w41,w42,w43] = groupings :: [[Worker]] 
[f12,f13,f14,f21,f23,f24,f31,f32,f34,f41,f42,f43] = groupings :: [[Factory]]


instance Relate Worker [Factory] Rank where
    gather = choices [W1 --> [f12,f13,f14,f23,f24,f34,f1,f2,f3,f4],
                      W2 --> [f21,f24,f23,f14,f13,f43,f2,f1,f4,f3],
                      W3 --> [f34,f31,f32,f41,f42,f12,f3,f4,f1,f2],
                      W4 --> [f43,f42,f41,f32,f31,f21,f4,f3,f2,f1]]  


instance Relate Factory [Worker] Rank where
    gather = choices [F1 --> [w34,w31,w32,w41,w42,w12,w3,w4,w1,w2],
                      F2 --> [w43,w42,w41,w32,w31,w21,w4,w3,w2,w1],
                      F3 --> [w21,w24,w23,w14,w13,w43,w2,w1,w4,w3],
                      F4 --> [w12,w13,w14,w23,w24,w34,w1,w2,w3,w4]]  



-- [(1.0, 250)],(edges,compl_edges,compl_ratio,mds_comps_comp,mds_ratio_comp,
-- tot_agg,tot_agg_ratio,mds_comps_agg,mds_ratio_agg): (62500, 26.3889, 0.0987, 26.3889, 0.0987, 12.0906, 0.0452, 12.0422, 0.045)

-- [(1.0, 270)],(edges,compl_edges,compl_ratio,mds_comps_comp,mds_ratio_comp,
-- tot_agg,tot_agg_ratio,mds_comps_agg,mds_ratio_agg): (72900, 27.9533, 0.0969, 27.9533, 0.0969, 12.7778, 0.0442, 12.7383, 0.0441)

-- [(1.0, 290)],(edges,compl_edges,compl_ratio,mds_comps_comp,mds_ratio_comp,tot_agg,
-- tot_agg_ratio,mds_comps_agg,mds_ratio_agg): (84100, 29.0378, 0.094, 29.0378, 0.094, 13.25, 0.0428, 13.2183, 0.0427)