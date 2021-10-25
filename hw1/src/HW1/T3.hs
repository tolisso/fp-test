module HW1.T3 where

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a)

-- instance Show a => Show (Tree a) where 
    -- show Leaf = ""
    -- show (Branch _ l x r) = "(" ++ show l ++ " /" ++ show x ++ "/" ++ show (max (tdepth l) (tdepth r) + 1) ++ "\\ " ++ show r ++ ")"

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch info _ _ _) = fst info

tdepth :: Tree a -> Int

tdepth Leaf = 0
tdepth (Branch info _ _ _) = snd info 

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l a r) =
    (x == a) || (if x < a then tmember x l else tmember x r)

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l a r = Branch 
    (tsize l + 1 + tsize r, max (tdepth l) (tdepth r) + 1) l a r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x a = if tmember x a then a else tinpush x a

data Side = LeftSide | RightSide deriving Eq

trotateLR :: Tree a -> Tree a
trotateLR (Branch _ (Branch _ ll y (Branch _ lrl x lrr)) z r) = 
    mkBranch (mkBranch ll y lrl) x (mkBranch lrr z r)
trotateLR t = t

trotateRL :: Tree a -> Tree a
trotateRL (Branch _ l z (Branch _ (Branch _ rll x rlr) y rr)) =
    mkBranch (mkBranch l z rll) x (mkBranch rlr y rr) 
trotateRL t = t

tbalance :: Tree a -> Tree a
tbalance (Branch _ (Branch _ ll x lr) y r) 
    | tdepth ll - tdepth r > 0 
        = mkBranch ll x $ mkBranch lr y r
tbalance (Branch _ l x (Branch _ rl y rr)) 
    | tdepth rr - tdepth l > 0 
        = mkBranch (mkBranch l x rl) y rr
tbalance (Branch _ l x r) 
    | tdepth l - tdepth r > 1 
        = trotateLR (mkBranch l x r)
tbalance (Branch _ l x r) 
    | tdepth r - tdepth l > 1 
        = trotateRL (mkBranch l x r)
tbalance t = t

-- dl = ll + 1
-- dl - dr > 1
-- ll - dr > 0
-- tbalance :: Tree a -> Tree a
-- tbalance t = foldl (\tr (s1, s2, method) -> 
--     if tbalanced tr s1 s2 
--         then tr 
--         else method tr) t 
--     [(LeftSide, LeftSide, trotateR), (RightSide, RightSide, trotateL),
--     (RightSide, LeftSide, trotateLR), (LeftSide, RightSide, trotateRL)]
    

tinpush :: Ord a => a -> Tree a -> Tree a
tinpush x Leaf = mkBranch Leaf x Leaf
tinpush x (Branch _ l a r) = tbalance $ if x < a 
    then mkBranch (tinpush x l) a r  
    else mkBranch l a (tinpush x r)

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf



 
