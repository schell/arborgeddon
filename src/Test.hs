module Main where

import Test.Framework ( Test, defaultMain, testGroup )
import Data.Maybe     ( fromJust, isNothing )

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Debug.Trace

import Arbor.Data.Vector
import Arbor.Data.Matrix
import Arbor.Data.Types


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Vector Group" vectorTestGroup
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

matrixTestGroup :: [Test]
matrixTestGroup = [ testProperty "2x2 transpose"   prop_mat2_transpose
                  , testProperty "2x2 minorAt"     prop_mat2_minorAt
--                  , testProperty "2x2 determinant" prop_mat2_det
--                  , testProperty "2x2 inverse"     prop_mat2_inv
                  ]

{- Vector -}
prop_magnitude :: Vector -> Bool
prop_magnitude vec = magnitude vec == sqrt (sum $ map (**2) vec)

prop_normalize :: Vector -> Bool
prop_normalize vec =
    if null vec
    then magnitude vec == 0.0 -- The zero vector.
    else (\n -> abs (n-1.0) < 0.0001) $ magnitude $ normalize vec

prop_unit :: Vector -> Bool
prop_unit = prop_normalize

prop_add :: Vector -> Vector -> Bool
prop_add vec1 vec2 = and $ zipWith3 (\a b apb -> a + b == apb) vec1 vec2 $ vec1 `add` vec2

prop_subtract :: Vector -> Vector -> Bool
prop_subtract v1 v2 = and $ zipWith3 (\a b amb -> a - b == amb) v1 v2 $ v1 `Arbor.Data.Vector.subtract` v2

prop_vecnat :: Int -> Int -> Vector -> Bool
prop_vecnat n at v = let vecn = vecNAt abn aba v
                         abn  = abs n
                         aba  = abs at
                     in
                         null vecn ||
                             length vecn == abn &&
                                 head vecn == v !! (aba*abn) &&
                                     last vecn == v !! (aba*abn+(abn-1))

prop_vec2at :: Int -> Vector -> Bool
prop_vec2at a v = let at = abs a in
    case vec2At at v of
        (0, 0) -> True
        (x, y) -> v !! (2*at) == x && v !! (2*at+1) == y

prop_vec3at :: Int -> Vector -> Bool
prop_vec3at at v = let vec3 = vec3At at v
                       vecn = vecNAt 3 at v in
    case vecn of
        []        -> vec3 == (0,0,0)
        (x:y:z:[])-> vec3 == (x,y,z)
        _         -> False

prop_vec4at :: Int -> Vector -> Bool
prop_vec4at at v = let vec4 = vec4At at v
                       vecn = vecNAt 4 at v in
    case vecn of
        []        -> vec4 == (0,0,0,0)
        [x,y,z,w] -> vec4 == (x,y,z,w)
        _         -> False

{- Matrix -}

instance Arbitrary Mat2x2 where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Mat2x2 a b c d
    shrink = shrink

prop_mat2_transpose :: Mat2x2 -> Bool
prop_mat2_transpose m@(Mat2x2 a b c d) = transpose m == Mat2x2 a c b d

prop_mat2_minorAt :: Mat2x2 -> Bool
prop_mat2_minorAt m@(Mat2x2 a b c d) = trace "quickchecking minorAt"
    minorAt m 0 0 == d

prop_mat2_det :: Mat2x2 -> Bool
prop_mat2_det m@(Mat2x2 a b c d) = trace "quickchecking determinant"
    a*d - b*c == determinant m

prop_mat2_inv :: Mat2x2 -> Bool
prop_mat2_inv m@(Mat2x2 a b c d) =
    let det  = trace "determinant" determinant m
        mInv = trace "inverse" inverse m
        mult = trace "multiply" multiply m (fromJust mInv)
        mVec = trace "toVector mult" toVector mult
        idVec= trace "toVector id" toVector (identity :: Mat2x2)

    in if det == 0
       then trace "checking is nothing" isNothing mInv
       else trace "checking is within range of identity" and $ zipWith (\e1 e2 -> e2 - e1 < 0.0001) mVec idVec

