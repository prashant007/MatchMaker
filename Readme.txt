
-- ========================================================================

Running the NRMP (Doctor-Hospital Example)

- ghci NRMPExample.hs

- getting the stable matching 
> showMatch(solveP :: Match Candidate Hospital)
Sunny:
                 Matched with []
                 Remaining capacity: 1

Darrius:
                 Matched with [City]
                 Remaining capacity: 0

Latha:
                 Matched with [General]
                 Remaining capacity: 0

Joseph:
                 Matched with [General]
                 Remaining capacity: 0

Arthur:
                 Matched with [City]
                 Remaining capacity: 0


*NRMPExample> showMatch(solveP :: Match Hospital Candidate)
General:
                 Matched with [Latha,Joseph]
                 Remaining capacity: 0

Mercy:
                 Matched with []
                 Remaining capacity: 2

City:
                 Matched with [Arthur,Darrius]
                 Remaining capacity: 0

-- ========================================================================

Running the Marriage Example 

> showMatch(solveP :: Match Man Woman)
Jack:
                 Matched with [Ann]
                 Remaining capacity: 0

Ben:
                 Matched with [Jane]
                 Remaining capacity: 0

Bob:
                 Matched with [Alice]
                 Remaining capacity: 0

> showMatch(solveP :: Match Woman Man)
Eli:
                 Matched with []
                 Remaining capacity: 1

Ann:
                 Matched with [Jack]
                 Remaining capacity: 0

Alice:
                 Matched with [Bob]
                 Remaining capacity: 0

Jill:
                 Matched with []
                 Remaining capacity: 1

Jane:
                 Matched with [Ben]
                 Remaining capacity: 0