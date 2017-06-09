
module Main where

-- De Bruijn index
data LambExp a = Var a | Abs (LambExp a) | App (LambExp a) (LambExp a)
  deriving (Show)

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
reduce (App (Abs x) y) = bind y (raise x (toEnum 0)) (toEnum 1)
reduce (App x y) = App (reduce x) (reduce y)
reduce (Abs x) = Abs (reduce x)

reduceable :: (Enum a, Ord a) => LambExp a -> Bool
reduceable (Var _) = False
reduceable (App (Abs _) _) = True
reduceable (App x y) = reduceable x || reduceable y
reduceable (Abs x) = reduceable x

fullReduce :: (Enum a, Ord a) => LambExp a -> LambExp a
fullReduce = until (not . reduceable) reduce

prittyPrint :: Show a => LambExp a -> String
prittyPrint (App x@(Abs _) y@(Abs _)) = "(" ++ prittyPrint x ++ ") (" ++ prittyPrint y ++ ")"
prittyPrint (App x@(Abs _) y) = "(" ++ prittyPrint x ++ ") " ++ prittyPrint y
prittyPrint (App x y@(Abs _)) = prittyPrint x ++ " (" ++ prittyPrint y ++ ")"

prittyPrint (App x@(App _ _) y) = prittyPrint x ++ " " ++ prittyPrint y
prittyPrint (App x y@(App _ _)) = prittyPrint x ++ " (" ++ prittyPrint y ++ ")"

prittyPrint (App x y) = prittyPrint x ++ " " ++ prittyPrint y
prittyPrint (Abs x) = "λ " ++ prittyPrint x
prittyPrint (Var x) = show x

lsucc = Abs $ Abs $ Abs $ Var 2 `App` ((Var 3 `App` Var 2) `App` Var 1) -- λn.λf.λx.f (n f x)  λ λ λ 2 (3 2 1)
lzero = Abs $ Abs $ Var 1 -- λ λ 1

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
  --putStrLn "(λ λ 4 2 (λ 1 3)) (λ 5 1)"
  mapM_ (putStrLn . prittyPrint) . take 5 . iterate reduce . (!! 1) $ iterate (App lsucc) lzero
  where
    a :: LambExp Integer
    a = Abs (Abs ((Var 4 `App` Var 2) `App` Abs (Var 1 `App` Var 3))) `App` Abs (Var 5 `App` Var 1)
