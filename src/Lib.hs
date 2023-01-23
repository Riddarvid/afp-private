{-# LANGUAGE InstanceSigs #-}
module Lib ( someFunc, appEx5, appEx6) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data MyMaybe a = MyNothing | MyJust a
  deriving Show

-- MyMabe is a functor

instance Functor MyMaybe where
  fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap _ MyNothing  = MyNothing
  fmap f (MyJust x) = MyJust $ f x

-- <$> is infix version of fmap

funcEx1 :: MyMaybe Integer
funcEx1 = (+1) <$> MyJust 5 -- MyJust 6

funcEx2 :: MyMaybe Integer
funcEx2 = (+1) <$> MyNothing -- MyNothing - we preserve structure

-- MyMaybe is an applicative functor

instance Applicative MyMaybe where
  pure :: a -> MyMaybe a
  pure = MyJust
  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  MyNothing <*> _ = MyNothing
  MyJust f <*> xx = f <$> xx

appEx1 :: MyMaybe (Integer -> Integer -> Integer)
appEx1 = pure (+)

appEx2 :: MyMaybe (Integer -> Integer)
appEx2 = pure (+) <*> MyJust 2

appEx3 :: MyMaybe Integer
appEx3 = pure (+) <*> MyJust 2 <*> MyJust 5

appEx4 :: MyMaybe Integer
appEx4 = pure (+) <*> MyJust 5 <*> MyNothing -- Nothing, since one of the "parameters" is nothing.

appEx5 :: [MyMaybe Integer]
appEx5 = [
    pure (+) <*> MyJust 5 <*> MyNothing,
    MyJust (+) <*> MyJust 5 <*> MyNothing,
    (+) <$> MyJust 5 <*> MyNothing,
    MyJust (5 +) <*> MyNothing,
    (5 +) <$> MyNothing,
    MyNothing]

appEx6 :: [MyMaybe Integer]
appEx6 = [
    pure (+) <*> MyJust 5 <*> MyJust 2,
    MyJust (+) <*> MyJust 5 <*> MyJust 2,
    (+) <$> MyJust 5 <*> MyJust 2,
    MyJust (5 +) <*> MyJust 2,
    (5 +) <$> MyJust 2,
    MyJust (5 + 2),
    MyJust 7]

myLiftA2 :: (a -> b -> c) -> MyMaybe a -> MyMaybe b -> MyMaybe c
myLiftA2 f xx yy = f <$> xx <*> yy

-- Standard Maybe is a monad

maybeSeven :: Integer -> Maybe Integer
maybeSeven 5 = Just 7
maybeSeven _ = Nothing

monadEx1 :: Maybe Integer
monadEx1 = Just 5 >>= maybeSeven

monadEx2 :: IO ()
monadEx2 = putStrLn "Hello" >>= (\_ -> putStrLn "What a nice day")
