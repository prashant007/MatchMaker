{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass, FlexibleInstances #-}


import DataType
import Info 
import MatchDatatype
import qualified Data.Map as M 

-- data Jobs = J1 | J2 | J3 | J4 | J5   deriving (Eq,Show,Ord,Enum,Bounded,Set)
-- data Machines = M1 | M2 deriving (Eq,Show,Ord,Enum,Bounded,Set)


-- type Duration = Int  

-- class JobShop a b | a -> b where 
--     duration :: Info a b Duration 

-- instance JobShop Jobs Machines where
--     gather = duration [J1 --> [M1 --> 4, M2 --> 3], 
--                        J2 --> [M1 --> 1, M2 --> 2],
--                        J3 --> [M1 --> 5, M2 --> 4],
--                        J4 --> [M1 --> 2, M2 --> 3],
--                        J5 --> [M1 --> 5, M2 --> 6]]  


-- data Jobs = J1 | J2 | J3 | J4 | J5 | J6   deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)
-- data Machines = M1 deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

-- data EvalJob = Eval {profit::Double, time::Double}

-- eval = Eval 

-- instance Relate Jobs Machines EvalJob where
--     gather = info [J1 --> [M1 --> eval 10 5], 
--                    J2 --> [M1 --> eval 6 10],
--                    J3 --> [M1 --> eval 5 5],
--                    J4 --> [M1 --> eval 4 1],
--                    J5 --> [M1 --> eval 2 3],
--                    J6 --> [M1 --> eval 8 5]]  

data Jobs = J1 | J2 | J3 | J4 | J5 deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)
data Machines = M1 | M2 deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

type Time = Int  

instance Relate Jobs Machines Time where
    gather = info [J1 --> [M1 --> 4, M2 --> 3], 
                   J2 --> [M1 --> 1, M2 --> 2],
                   J3 --> [M1 --> 5, M2 --> 4],
                   J4 --> [M1 --> 2, M2 --> 3],
                   J5 --> [M1 --> 5, M2 --> 6]]  
