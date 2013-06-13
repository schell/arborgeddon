{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Test.Framework ( Test, defaultMain, testGroup )
import Data.Maybe     ( fromJust, isNothing )

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Debug.Trace

import Geometry

import Prelude hiding ( subtract )

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "[Float] Group" vectorTestGroup
        , testGroup "Matrix Group" matrixTestGroup
        ]

vectorTestGroup :: [Test]
vectorTestGroup = [ testProperty "magnitude"   prop_magnitude
                  , testProperty "normalize"   prop_normalize
                  , testProperty "unit vector" prop_unit
                  , testProperty "add"         prop_add
                  , testProperty "subtract"    prop_subtract
                  , testProperty "vecNAt"      prop_vecnat
                  , testProperty "vec2At"      prop_vec2at
                  , testProperty "vec3At"      prop_vec3at
                  , testProperty "vec4At"      prop_vec4at
                  ]
{- [Float] -}
prop_magnitude :: [Float] -> Bool
prop_magnitude vec = magnitude vec == sqrt (sum $ map (**2) vec)

prop_normalize :: [Float] -> Bool
prop_normalize vec =
    if null vec
    then magnitude vec == 0.0 -- The zero vector.
    else (\n -> abs (n-1.0) < 0.0001) $ magnitude $ normalize vec

prop_unit :: [Float] -> Bool
prop_unit = prop_normalize

prop_add :: [Float] -> [Float] -> Bool
prop_add vec1 vec2 = and $ zipWith3 (\a b apb -> a + b == apb) vec1 vec2 $ vec1 `add` vec2

prop_subtract :: [Float] -> [Float] -> Bool
prop_subtract v1 v2 = and $ zipWith3 (\a b amb -> a - b == amb) v1 v2 $ v1 `subtract` v2

prop_vecnat :: Int -> Int -> [Float] -> Bool
prop_vecnat n at v = let vecn = vecNAt abn aba v
                         abn  = abs n
                         aba  = abs at
                     in
                         null vecn ||
                             length vecn == abn &&
                                 head vecn == v !! (aba*abn) &&
                                     last vecn == v !! (aba*abn+(abn-1))

prop_vec2at :: Int -> [Float] -> Bool
prop_vec2at a v = let at = abs a in
    case vec2At at v of
        (0, 0) -> True
        (x, y) -> v !! (2*at) == x && v !! (2*at+1) == y

prop_vec3at :: Int -> [Float] -> Bool
prop_vec3at at v = let vec3 = vec3At at v
                       vecn = vecNAt 3 at v in
    case vecn of
        []        -> vec3 == (0,0,0)
        (x:y:z:[])-> vec3 == (x,y,z)
        _         -> False

prop_vec4at :: Int -> [Float] -> Bool
prop_vec4at at v = let vec4 = vec4At at v
                       vecn = vecNAt 4 at v in
    case vecn of
        []        -> vec4 == (0,0,0,0)
        [x,y,z,w] -> vec4 == (x,y,z,w)
        _         -> False

{- Matrix -}

matrixTestGroup :: [Test]
matrixTestGroup = [ testProperty "transpose"               prop_matrix_transpose
                  , testProperty "deleteColRow"            prop_matrix_del
                  , testProperty "minorAt"                 prop_matrix_minorAt
                  , testProperty "determinant of identity" prop_matrix_det_id
                  , testProperty "inverse"                 prop_matrix_inv
                  , testProperty "identityN"               prop_matrix_id_n
                  ]

data TestMatrix = TestMatrix (Matrix Double) deriving (Show, Eq)

instance Arbitrary TestMatrix where
    arbitrary = do
        r <- choose (2, 4)
        v <- vector $ r*r
        return $ TestMatrix $ groupByRowsOf r v


prop_matrix_transpose :: TestMatrix -> Bool
prop_matrix_transpose (TestMatrix m) = transpose m == toColumns m

prop_matrix_del :: TestMatrix -> Bool
prop_matrix_del (TestMatrix m) = let del = deleteColRow m 0 0
                                 in numColumns del == numColumns m -1 &&
                                        numRows del == numRows m -1

prop_matrix_minorAt :: TestMatrix -> Bool
prop_matrix_minorAt (TestMatrix m) = let minor = minorAt m 0 0
                                         in minor >= 0.0 || minor <= 0



prop_matrix_inv :: TestMatrix -> Bool
prop_matrix_inv (TestMatrix m) =
    let det     = determinant m
        mInv    = inverse m
        mult    = multiply m (fromJust mInv)
        detMult = determinant mult
        range   = 1.0 - detMult
    in if det == 0
       then isNothing mInv
       else abs range <= 0.001

prop_matrix_det_id :: TestMatrix -> Bool
prop_matrix_det_id (TestMatrix m) = 1.0 == determinant (identity m)

prop_matrix_id_n :: Bool
prop_matrix_id_n = let i = identityN 4 in
    (4 == numRows i) &&
        4 == numColumns i
