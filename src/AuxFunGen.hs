module AuxFunGen where


-- ------|---------|-----     auxiliaries    ----|---------|---------|

remv :: Eq a => a -> [(a,b)] -> [(a,b)]
remv _ [] = []
remv x (e:es)
   | x == (fst e) = es
   | otherwise = e:(remv x es)


lkUp :: Eq a => a -> [(a,[b])] -> [b] 
lkUp _ [] = []
lkUp x (e:es)
   | x == (fst e) = snd e
   | otherwise = lkUp x es


conc :: Eq a => [(a,[b])] -> [(a,[b])] -> [(a,[b])] 
conc [] aspcs = aspcs
conc ((vn,cs):dcs) aspcs = 
   conc dcs ((vn,(lkUp vn aspcs) ++ cs):(remv vn aspcs))


lkUpSure :: (Eq a,Show a) => a -> [(a,b)] -> b 
lkUpSure x [] =
   error ("lkUpSure: " ++ show x ++ " must be existing key")
lkUpSure x (e:es)
   | x == (fst e) = snd e
   | otherwise = lkUpSure x es


lkUpSureS :: (Eq b,Show b) => b -> [(a,b)] -> a 
lkUpSureS x [] = 
   error ("lkUpSureS: " ++ show x ++ " must be existing key")
lkUpSureS x (e:es)
   | x == (snd e) = fst e
   | otherwise = lkUpSureS x es


fst3 :: (a,b,c) -> a
fst3 (f,_,_) = f

snd3 :: (a,b,c) -> b
snd3 (_,s,_) = s

trd3 :: (a,b,c) -> c
trd3 (_,_,t) = t

mkTrpl :: a -> b -> c -> (a,b,c)
mkTrpl f s t = (f,s,t)

-- ------|---------|---------|---------|---------|---------|---------|