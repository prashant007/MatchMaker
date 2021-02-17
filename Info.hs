module Info where

import qualified Data.Map as M 

data Rec a b = Rec {unRec :: M.Map a (Maybe b)}

toRec :: Ord a => [(a,Maybe b)] -> Rec a b  
toRec = Rec . M.fromList 

fromRec :: Ord a => Rec a b -> [(a,Maybe b)] 
fromRec = M.toList . unRec 

onRec :: (M.Map a (Maybe b) -> M.Map c (Maybe d)) -> Rec a b -> Rec c d 
onRec f = Rec . f . unRec 

mapRec :: (b -> c) -> Rec a b -> Rec a c 
mapRec f = onRec (M.map (fmap f)) 

filterRec :: (Maybe b -> Bool) -> Rec a b -> Rec a b 
filterRec f = onRec (M.filter f)  


data Info a b c = Info {unInfo :: M.Map a (Rec b c)}

fromInfo :: Ord b => Info a b c -> [(a,[(b,Maybe c)])] 
fromInfo = M.toList . M.map fromRec . unInfo



(-->) :: a -> b -> (a,b)
(-->) x y = (x,y)

info :: (Ord a, Ord b) => [(a,[(b,Maybe c)])] -> Info a b c 
info = Info . M.fromList . map (\(x,y) -> (x,toRec y)) 

onInfo :: (M.Map a (Rec b c) -> M.Map d (Rec e f)) -> Info a b c -> Info d e f 
onInfo f = Info .  f . unInfo 

mapInfo2 :: (c -> d) -> Info a b c -> Info a b d 
mapInfo2 f = onInfo (M.map (mapRec f))

filterInfo :: (c -> Bool) -> Info a b c -> Info a b c 
filterInfo f = onInfo (M.map (filterRec (convFun f)))
    where
        convFun :: (c -> Bool) -> Maybe c -> Bool 
        convFun g (Just v) = g v
        convFun g _        = False  
