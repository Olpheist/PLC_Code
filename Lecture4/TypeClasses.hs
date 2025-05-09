-- :type (== 5)
-- :type (== (5 :: Int))

elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y:ys)
    | x == y    = True
    | otherwise = elem x ys

-- partially applying

-- :type (elem (5 :: Int))
-- (elem (5 :: Int)) :: [Int] -> Bool

eitherElem :: Eq a => a -> [a] -> [a] -> Bool
eitherElem x xs ys 
    | elem x xs = True
    | elem x ys = True
    | otherwise = False


class Eq a => ThreeEq a where
    teq :: a -> a -> a -> Bool
    teq x y z
        | x == y && y == z = True
        | otherwise = False
    notteq :: a -> a -> a -> Bool
    notteq x y z = not (teq x y z)

instance ThreeEq Int where
instance ThreeEq Char where
instance Eq a => ThreeEq [a] where
instance ThreeEq Bool where

    -- teq x y z
    -- below can be a default implementation in the class def
        -- | x == y && y == z = True
        -- | otherwise = False
    --below can be a default implementation in the class def
    -- notteq x y z = not (teq x y z)

teq (5 :: Int) 5 5
--True
teq (5 :: Int) 5 6
--False

--hoogle.haskell.org

map (+ 2) [1, 2, 3, 4]
-- [3, 4, 5, 6]

-- :instances []

fmap (+ 2) [1, 2, 3, 4]
-- [3, 4, 5, 6]

fmap (+ 2) (Just 3)
-- Just 5

fmap (+ 2) Nothing
-- Nothing

fmap (+ 2) (* 5)
-- well defined, but no instance for 'Show (Integer -> Integer)'

(fmap (+ 2) (* 5)) 3
-- 17

