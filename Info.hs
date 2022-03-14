module Info where

import qualified Data.Map as M 
import Data.Maybe 

data Rec a b = Rec {unRec :: M.Map a (Maybe b)} deriving Show 

lookupRec :: Ord a => a -> Rec a b -> Maybe b 
lookupRec x  = fromJust . M.lookup x . unRec 

toRec :: Ord a => [(a,Maybe b)] -> Rec a b  
toRec = Rec . M.fromList 

fromRec :: Ord a => Rec a b -> [(a,Maybe b)] 
fromRec = M.toList . unRec 

onRec :: (M.Map a (Maybe b) -> M.Map c (Maybe d)) -> Rec a b -> Rec c d 
onRec f = Rec . f . unRec 

mapRec :: (b -> c) -> Rec a b -> Rec a c 
mapRec f = onRec (M.map (fmap f)) 

mapRecWithKey :: (a -> b -> c) -> Rec a b -> Rec a c 
mapRecWithKey f = onRec (M.mapWithKey (\x -> fmap (f x))) 

filterRec :: (Maybe b -> Bool) -> Rec a b -> Rec a b 
filterRec f = onRec (M.filter f)  

data Info a b c = Info {unInfo :: M.Map a (Rec b c)} deriving Show 

lookupInfo :: (Ord a,Ord b) => (a,b) -> Info a b c -> Maybe c 
lookupInfo (x,y) = lookupRec y. fromJust . M.lookup x . unInfo


fromInfo :: Ord b => Info a b c -> [(a,[(b,Maybe c)])] 
fromInfo = M.toList . M.map fromRec . unInfo

-- info :: (Ord a, Ord b) => [(a,Rec b c)] -> Info a b c 
-- info = Info . M.fromList 


info :: (Ord a, Ord b) => [(a,[(b,c)])] -> Info a b c 
info = Info . M.fromList. map (\(x,y) -> (x, toRec . map (\(a,b) -> (a,Just b)) $ y)) 


onInfo :: (M.Map a (Rec b c) -> M.Map d (Rec e f)) -> Info a b c -> Info d e f 
onInfo f = Info .  f . unInfo 

mapInfo :: (c -> d) -> Info a b c -> Info a b d 
mapInfo f = onInfo (M.map (mapRec f))

filterInfo :: (c -> Bool) -> Info a b c -> Info a b c 
filterInfo f = onInfo (M.map (filterRec (convFun f)))
    where
        convFun :: (c -> Bool) -> Maybe c -> Bool 
        convFun g (Just v) = g v
        convFun g _        = False  

    
completeWith :: Ord a => (b -> c -> d) -> Info a b c  -> Info a b d  
completeWith f = onInfo (M.map (mapRecWithKey f))

completeWith2 :: Ord a => (b -> c -> d -> e) -> Info a b (c,d)  -> Info a b e 
completeWith2 f = onInfo (M.map (mapRecWithKey (\k (x,y) -> f k x y)))