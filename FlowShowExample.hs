
data Jobs = J1 | J2 | J3 | J4 | J5   deriving (Eq,Show,Ord,Enum,Bounded,Set)
data Machines = M1 | M2 deriving (Eq,Show,Ord,Enum,Bounded,Set)


type Duration = Int  

class JobShop a b | a -> b where 
    duration :: Info a b Duration 

instance JobShop Jobs Machines where
    gather = duration [J1 --> [M1 --> 4, M2 --> 3], 
                       J2 --> [M1 --> 1, M2 --> 2],
                       J3 --> [M1 --> 5, M2 --> 4],
                       J4 --> [M1 --> 2, M2 --> 3],
                       J5 --> [M1 --> 5, M2 --> 6]  

