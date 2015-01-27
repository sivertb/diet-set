{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List (nub, sort)
import Data.Set.Diet
import qualified Data.Set as S
import Prelude hiding (null)
import System.Exit
import Test.QuickCheck

instance (Integral a, Arbitrary a) => Arbitrary (Interval a) where
    arbitrary = do
        l <- arbitrary
        s <- arbitrary
        return $ Interval l (l + s)

    shrink (Interval l u) = [ Interval l' (l' + s)
                            | (l', s) <- shrink (l, u - l)
                            ]

-- | Takes a list of Ints, and returns a list of intervals covering those Ints.
listToIntervals :: (Enum a, Ord a) => [a] -> [Interval a]
listToIntervals ls =
    case nub $ sort ls of
         []     -> []
         (x:xs) -> helper (point x) xs
    where
        helper i                []     = [i]
        helper i@(Interval l u) (x:xs)
            | succ u == x = helper (Interval l x) xs
            | otherwise   = i : helper (point x) xs

-- | Takes a list of possibly overlapping intervals, and returns a sorted list
-- with no overlapping intervals.
mergeIntervals :: (Ord a, Enum a) => [Interval a] -> [Interval a]
mergeIntervals intervals =
    case sort $ filter (not . intervalInvalid) intervals of
         []     -> []
         (i:is) -> helper i is
    where
        helper i []     = [i]
        helper i (l:ls) =
            case merge i l of
                 Nothing -> i : helper l ls
                 Just j  -> helper j ls

prop_valid_insert :: [Int] -> Bool
prop_valid_insert a = valid $ foldr insert empty a

prop_valid_insert_delete :: [Int] -> [Int] -> Bool
prop_valid_insert_delete a d = valid $ foldr delete (foldr insert empty a) d

prop_valid_insertI :: [Interval Int] -> Bool
prop_valid_insertI a = valid $ foldr insertI empty a

prop_valid_insertI_deleteI :: [Interval Int] -> [Interval Int] -> Bool
prop_valid_insertI_deleteI a d = valid $ foldr deleteI (foldr insertI empty a) d

prop_size :: [Int] -> Property
prop_size a = size (foldr insert empty a) === length (nub a)

prop_sizeI_1 :: [Int] -> Property
prop_sizeI_1 ls = sizeI (foldr insert empty ls) === length (listToIntervals ls)

prop_sizeI_2 :: [Interval Int] -> Property
prop_sizeI_2 ls = sizeI (foldr insertI empty ls) === length (mergeIntervals ls)

prop_null1 :: Bool
prop_null1 = null empty

prop_null2 :: [Int] -> Bool
prop_null2 a = null $ foldr delete (foldr insert empty a) a

prop_minView :: [Int] -> Property
prop_minView ls =
    case minView (foldr insert empty ls) of
         Just (a, set') -> minimum ls === a .&&. notMember a set'
         _              -> ls === []

prop_maxView :: [Int] -> Property
prop_maxView ls =
    case maxView (foldr insert empty ls) of
         Just (a, set') -> maximum ls === a .&&. notMember a set'
         _              -> ls === []

prop_list :: [Int] -> Property
prop_list ls = S.toList (S.fromList ls) === toList (fromList ls)

prop_listI :: [Interval Int] -> Property
prop_listI ls = toListI (fromListI ls) === mergeIntervals ls

prop_toListI :: [Int] -> Property
prop_toListI ls = listToIntervals ls === toListI (foldr insert empty ls)

prop_singleton :: Int -> Property
prop_singleton i = toList (singleton i) === [i]

prop_singletonI :: Interval Int -> Property
prop_singletonI i = toListI (singletonI i) === [i]

prop_eq :: [Interval Int] -> Property
prop_eq ls = foldr insertI empty ls === foldr insertI empty (mergeIntervals ls)

prop_read_show :: [Interval Int] -> Property
prop_read_show ls =
    let d = foldr insertI empty ls
    in  d === read (show d)

prop_member :: [Int] -> [Int] -> Property
prop_member a f =
    let d = foldr insert empty a
        s = foldr S.insert S.empty a
    in  conjoin $ map (\i -> member i d === S.member i s) f

prop_notMember :: [Int] -> [Int] -> Property
prop_notMember a f =
    let d = foldr insert empty a
        s = foldr S.insert S.empty a
    in  conjoin $ map (\i -> notMember i d === S.notMember i s) f

return []
tests :: IO Bool
tests = $quickCheckAll

main :: IO ()
main = tests >>= flip unless exitFailure
