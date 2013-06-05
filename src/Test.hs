module Main where

import Test.Framework ( Test, defaultMain, testGroup )

import Test.Framework.Providers.QuickCheck2

import Arbor.Data.Vector

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Vector Group" vectorTestGroup ]

vectorTestGroup :: [Test]
vectorTestGroup = [ testProperty "magnitude" prop_magnitude
                  , testProperty "normalize" prop_normalize
                  , testProperty "unit vector" prop_unit
                  ]

prop_magnitude :: Vector -> Bool
prop_magnitude vec = magnitude vec == sqrt (sum $ map (**2) vec)

prop_normalize :: Vector -> Bool
prop_normalize vec =
    if null vec
    then magnitude vec == 0.0 -- The zero vector.
    else (\n -> abs (n-1.0) < 0.0001) $ magnitude $ normalize vec

prop_unit :: Vector -> Bool
prop_unit = prop_normalize
