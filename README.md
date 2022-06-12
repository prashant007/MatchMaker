# MatchMaker: A DSL for Game-Theoretic Matching
Many real-world problems are instances of game-theoretic matching problems and can be solved, in principle, using the algorithms developed by economists and computer scientists. However, existing tools are limited in their expressiveness and are difficult to employ. Specifically, most current libraries for solving matching problems require the problem representation to be encoded in strings, which affects their readability and maintenance and are prone to errors.

We introduce MatchMaker, a domain-specific language (DSL), embedded in Haskell, which supports the direct, high-level representation of matching problems. Haskell's type system, especially the use of multi-parameter type classes, facilitates the definition of a very general interface to matching problems, which can be quickly instantiated to a wide variety of different matching applications. Another novel aspect of Match-Maker is that it provides combinators for dynamically updating and modifying problem representations as well as for analyzing matching results. 


## Installing the MatchMaker Library
This is the accompanying code base for our paper. The Match-Maker library can be installed as follows:

1) Download the package and navigate to the folder of the package containing the cabal file.
2) Execute "cabal configure".
3) Execute "cabal install".
4) The package is now installed and can be used in a Haskell file like a normal import. For example - a programmer can use the files from our module to implement the NRMP example in the following way in any folder of their choice.  

```haskell
{-# LANGUAGE MultiParamTypeClasses, DeriveAnyClass #-}

import TypeClasses
import Info 
import MatchType
import Combinators 
import MatchingFunctions

data Applicant = Arthur | Sunny | Joseph | Latha | Darrius | Bob 
                 deriving (Eq,Show,Ord,Enum,Bounded,Set,Weights)

data Hospital = City | Mercy | General deriving (Eq,Show,Ord,Enum,Bounded,Weights)

instance Set Hospital where
    quota = forall 2 

instance Preference Hospital Applicant Rank where
    gather = choices [Mercy   --> [Darrius,Joseph], 
                      City    --> [Darrius,Arthur,Sunny,Latha,Joseph],
                      General --> [Darrius,Arthur,Joseph,Latha]]  

instance Preference Applicant Hospital Rank where
    gather = choices [Arthur  --> [City], 
                      Sunny   --> [City,Mercy], 
                      Joseph  --> [City,General,Mercy],
                      Latha   --> [Mercy,City,General],
                      Darrius --> [City,Mercy,General]]  
```

===================================================

The various examples shown in the paper are present in the following files:

- **NRMP_Ranks.hs** - Example file with the above encoding with concrete ranks for the NRMP matching discussed in the paper.

- **NRMP_ADT.hs**  - Example file with reprsentational rank encoding with concrete ranks for the NRMP matching discussed in the paper.

- **KidneyExchange_Ranks.hs** - Kidney exchange example with concrete ranks (both one way and with exchange). 

- **KidneyExchange_ADT.hs**  - Kidney exchange example with representational ranks (only one way). 

- **Project_allocation.hs** - Project Allocation example. 

- **StableRoommateEx.hs** - Stable Rommate example.
