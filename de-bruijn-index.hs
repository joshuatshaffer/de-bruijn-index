
module Main where

-- De Bruijn index
data LambExp a = Var a | Abs (LambExp a) | App (LambExp a) (LambExp a)

raise :: (Enum a, Ord a) => LambExp a -> a -> LambExp a
raise (Var x) d = if x > d then Var (pred x) else Var x
raise (App x y) d = App (raise x d) (raise y d)
raise (Abs x) d = Abs (raise x (succ d))

lower :: (Enum a, Ord a) => LambExp a -> a -> LambExp a
lower (Var x) d = if x > d then Var (succ x) else Var x
lower (App x y) d = App (lower x d) (lower y d)
lower (Abs x) d = Abs (lower x (succ d))

bind :: (Enum a, Ord a) => LambExp a -> LambExp a -> a -> LambExp a
bind e (Var x) d = if x == d then e else Var x
bind e (App x y) d = App (bind e x d) (bind e y d)
bind e (Abs x) d = Abs (bind (lower e (toEnum 0)) x (succ d))

reduce :: (Enum a, Ord a) => LambExp a -> LambExp a
reduce (Var x) = Var x
reduce (App (Abs x) y) = raise (bind (lower y (toEnum 0)) x (toEnum 1)) (toEnum 1)
reduce (App x y) = App (reduce x) (reduce y)
reduce (Abs x) = Abs (reduce x)

reduceable :: (Enum a, Ord a) => LambExp a -> Bool
reduceable (Var _) = False
reduceable (App (Abs _) _) = True
reduceable (App x y) = reduceable x || reduceable y
reduceable (Abs x) = reduceable x

fullReduce :: (Enum a, Ord a) => LambExp a -> LambExp a
fullReduce = until (not . reduceable) reduce

instance (Show a) => Show (LambExp a) where
  show (App x@(Abs _) y@(Abs _)) = "(" ++ show x ++ ") (" ++ show y ++ ")"
  show (App x@(Abs _) y) = "(" ++ show x ++ ") " ++ show y
  show (App x y@(Abs _)) = show x ++ " (" ++ show y ++ ")"

  show (App x@(App _ _) y) = show x ++ " " ++ show y
  show (App x y@(App _ _)) = show x ++ " (" ++ show y ++ ")"

  show (App x y) = show x ++ " " ++ show y
  show (Abs x) = "λ " ++ show x
  show (Var x) = show x

instance Functor LambExp where
  fmap f (Var x) = Var (f x)
  fmap f (Abs x) = Abs (fmap f x)
  fmap f (App x y) = App (fmap f x) (fmap f y)

lzero,lsucc,lplus,lmult :: LambExp Int
lzero = Abs $ Abs $ Var 1 -- λ λ 1
lsucc = Abs $ Abs $ Abs $ Var 2 `App` ((Var 3 `App` Var 2) `App` Var 1) -- λn.λf.λx.f (n f x)  λ λ λ 2 (3 2 1)
lplus = Abs $ Abs $ Abs $ Abs $ (Var 4 `App` Var 2) `App` ((Var 3 `App` Var 2) `App` Var 1)--λm.λn.λf.λx.m f (n f x)
lmult = Abs $ Abs $ Abs $ Var 3 `App` (Var 2 `App` Var 1)

lnat :: Int -> LambExp Int
lnat = (!!) $ iterate (fullReduce . App lsucc) lzero
{-
instance (Read a) => Read (LambExp a) where
  readPrec :: ReadPrec (LambExp a)
  readPrec = undefined
    where
      readApp = foldl1 App <$> sepBy1 readTerm (many1 (char ' '))
      readTerm = (readVar +++ between (char '(') (char ')') readExp) <++ readExp
      rb = char 'λ' >> many1 (char ' ') >> rl >>= (\x -> Abs x)

      readVar = many1 . choice $ map char ['0'..'9']
-}

main :: IO ()
main = do
  mapM_ print . take 5 $ iterate (fullReduce . App lsucc) lzero
  print $ fullReduce $ (lmult `App` lnat 3) `App` lnat 2
