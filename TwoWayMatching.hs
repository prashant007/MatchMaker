module TwoWayMatching where

import Data.Maybe 
import Data.List 
import MatchType
  
type RCapacity = Int 
type Options b = [b] 
type Matches b = [b]
type MRank a b = [(a,(Options b,Matches b,Capacity,RCapacity))]

del xv  = filter (\a -> fst a /= xv) 

cond :: Eq b => (a,(Options b,Matches b,Capacity,RCapacity)) -> Bool 
cond = (\(_,(x,y,c,r)) -> x == [] || r == 0)

-- while there exist a free man m who still has a woman w to propose to 
galeShapley :: (Eq b, Eq a) => MRank a b -> MRank b a -> MRank a b 
galeShapley x y = gsHelper x y x 
    where 
        gsHelper :: (Eq b, Eq a) => MRank a b -> MRank b a -> MRank a b -> MRank a b 
        gsHelper [] ys xs' =  
            let f = and.map cond 
            in case f xs' of 
                 {True  -> xs'; False -> gsHelper xs' ys xs'}

        gsHelper (x:xs) ys xs' =  
            case cond x of 
              True  -> gsHelper xs ys xs'
              False -> let (nys,nxs') = g x (ys,xs') in gsHelper xs nys nxs'


g :: (Eq b, Eq a) => (a,(Options b, Matches b,Capacity,RCapacity)) -> 
                     (MRank b a,MRank a b) -> (MRank b a,MRank a b)
g (xv,(xo,xm,xc,xr)) yxl@(yl,xl) 
    | xr == 0 || xo == [] = (yl,(xv,(xo,xm,xc,xr)):del xv xl)
    | otherwise = if elem xv yo 
        then case yr == 0 of -- no capacity left in y for match
        -- check if an existing match is lower ranked for y and be replaced 
            True -> case replace xv (yo,ym) [] of 
                Nothing  -> g (xv,(xo',xm,xc,xr)) yxl
                Just (pv,ym') -> g (xv,(xo',yv:xm,xc,xr-1)) (yl',xl')
                    where (po,pm,pc,pr) = (fromJust.lookup pv) xl 
                          yl' = (yv,(yo,ym',yc,yr)):del yv yl
                          xl' = (pv,(po,pm \\ [yv],pc,pr+1)):del pv xl  

            False -> let yl' = (yv,(yo,xv:ym,yc,yr-1)):del yv yl 
                     in g (xv,(xo',yv:xm,xc,xr-1)) (yl',xl)
        else g (xv,(xo',xm,xc,xr)) yxl
        where yv = head xo 
              ys@(yo,ym,yc,yr) = (fromJust.lookup yv) yl  
              y  = (yv,ys)
              xo'= tail xo 


replace :: Eq a => a -> (Options a,Matches a) -> Matches a -> Maybe (a,Matches a) 
replace _ (_,[]) ym' = Nothing 
replace xv (yo,y:ys) ym' 
  | isHigher xv y yo = return $ (y,xv:ys ++ ym')
  | otherwise = replace xv (yo,ys) (y:ym')


isHigher :: Eq a => a -> a -> [a] -> Bool 
isHigher x y ls = case (elemIndex x ls,elemIndex y ls) of 
                    (Just p, Just q) -> p < q 
                    _ -> False




