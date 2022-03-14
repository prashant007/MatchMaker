-- Roth 154 

data Worker  = Worker1 | Worker2 | Worker3 | Worker4 deriving (Eq,Show,Ord,Enum,Bounded)
data Factory = Factory1 | Factory2 | Factory3 | Factory4 deriving (Eq,Show,Ord,Enum,Bounded)

instance Set Worker where
    capacity = every 2 

instance Set Factory where
    capacity = every 2 

instance Relate Applicant Hospital Rank where
    gather = choicesM [s12 --> [h12,h42,h43,h42,h14,h13,h34,h31,h32,h23,h24,h21],
                       s34 --> [h42,h43,h41,h31,h32,h34,h24,h21,h23,h12,h14,h13]]  