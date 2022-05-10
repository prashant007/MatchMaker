module Info where

import qualified Data.Map as M 
import Data.Maybe 
import Data.List 
import MatchType

data Rec a b = Rec {unRec :: M.Map a (Maybe b)} 

instance (Show a,Show b) => Show (Rec a b) where
    show (Rec x) = show . M.toList . M.map (fromJust) $ x 

lookupRec :: Ord a => a -> Rec a b -> Maybe b 
lookupRec x  = fromJust . M.lookup x . unRec 

toRec :: Ord a => [(a,Maybe b)] -> Rec a b  
toRec = Rec . M.fromList 

combineRec :: Ord a => Rec a b -> Rec a b -> Rec a b 
combineRec as bs = toRec $ bs' ++ filter (g bs1) as' 
    where [as',bs'] = map fromRec [as,bs]
          bs1 = map fst bs'  
          g xs a =  not . elem (fst a) $ xs 

mkRec :: Ord a => [(a,b)] -> Rec a b  
mkRec = toRec. map (\(x,y) -> (x,Just y))

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

data Info a b c = Info {unInfo :: M.Map a (Rec b c)} 

instance Show3 a b c => Show (Info a b c) where
    show (Info x) = concat . map snd . M.toList . M.mapWithKey f $ x 
        where f = \x y -> "\t" ++ show x ++ " --> " ++ show y ++  "\n"

lookupInfo :: Ord2 a b => a -> b -> Info a b c -> c 
lookupInfo x y =  fromJust. lookupRec y. fromJust . M.lookup x . unInfo

fromInfo :: Ord b => Info a b c -> [(a,[(b,Maybe c)])] 
fromInfo = M.toList . M.map fromRec . unInfo

info :: Ord2 a b => [(a,[(b,c)])] -> Info a b c 
info = Info . M.fromList. map (\(x,y) -> (x, toRec . map (\(a,b) -> (a,Just b)) $ y)) 

onInfo :: (M.Map a (Rec b c) -> M.Map d (Rec e f)) -> Info a b c -> Info d e f 
onInfo f = Info .  f . unInfo 

mapInfo :: (c -> d) -> Info a b c -> Info a b d 
mapInfo f = onInfo (M.map (mapRec f))

mapInfoWithKey1 :: (a -> c -> d) -> Info a b c -> Info a b d 
mapInfoWithKey1 f = onInfo (M.mapWithKey (\x -> mapRec (f x)))

mapInfoWithKey :: (a -> b -> c -> d) -> Info a b c -> Info a b d 
mapInfoWithKey f = onInfo (M.mapWithKey (\x -> mapRecWithKey (f x)))

filterInfo :: (c -> Bool) -> Info a b c -> Info a b c 
filterInfo f = onInfo (M.map (filterRec (convFun f)))
    where
        convFun :: (c -> Bool) -> Maybe c -> Bool 
        convFun g (Just v) = g v
        convFun g _        = False  

choices :: Ord2 a b => [(a,[b])] -> Info a b Rank
choices = info . map (\(x,ys) -> (x,assocRanks ys))
    where assocRanks =  zipWith (\q p -> p --> Rank q) [1..] 


getRank :: Eq2 a b => a -> b -> Match a b -> Rank 
getRank a b = Rank . (\x -> fromJust x + 1) . elemIndex b . getpreferences a 

toCompRanks :: Eq2 a b => Match a b -> CompMatch a b -> CompRanks a b
toCompRanks m = CompRanks .  map f . unCompMatch
    where f (x,ys,zs) = (x,map (g x) ys,map (g x) zs)
          g p q = (q,getRank p q m)
    

