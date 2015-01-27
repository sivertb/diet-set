{-|
Module      : Data.Set.Diet
Description : Discrete Interval Encoding Trees
Copyright   : (C) Sivert Berg 2015
License     : GPL-3
Maintainer  : code@trev.is
Stability   : experimental
Portability : GHC only

Discrete Interval Encoding Trees, as described by Martin Erwig in "Diets for
Fat Sets". It uses 'Data.Set' as the underlying data structure to avoid having
to implement a balanced binary from scratch.

Discrete Interval Encoding Trees represent adjacent elements in the set using
an interval. E.g. the following

> fromList [0, 2, 3, 4, 6]

produces the set

> fromListI [Interval 0 0,Interval 2 4,Interval 6 6]

This allows some sets to be represented using far fewer nodes than if they had
to use a single node per value.
-}
module Data.Set.Diet
    (
    -- * Intervals
      Interval (..)
    , overlapping
    , adjacent
    , apart
    , intervalSize
    , intervalToList
    , intervalInvalid
    , merge
    , point
    , lower
    , upper

    -- * Set type
    , Diet

    -- * Query
    , null
    , size
    , sizeI
    , member
    , notMember

    -- * Construction
    , empty
    , insertI
    , deleteI
    , singletonI
    , insert
    , delete
    , singleton

    -- * Min/max
    , minView
    , maxView

    -- * To/from lists
    , toListI
    , toList
    , fromListI
    , fromList

    -- * Debugging
    , valid
    )
where

import Control.Arrow ((&&&))
import Control.Applicative ((<$), (<$>), (*>))
import Control.DeepSeq
import Control.Monad (guard)
import Data.Foldable (foldr, concatMap, and)
import Data.Ix (Ix, rangeSize)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Set as S
import Prelude hiding (null, foldr, concatMap, and)
import Text.Read

-- | A closed interval.
data Interval a = Interval !a !a deriving (Show, Read, Eq, Ord)

-- | A Discrete Interval Encoding Tree.
newtype Diet a = Diet { unDiet :: S.Set (Interval a) } deriving (Eq)

instance Show a => Show (Diet a) where
    show d = "fromListI " ++ show (toListI d)

instance (Read a, Enum a, Ord a) => Read (Diet a) where
    readPrec =
        parens
        . prec 10
        $ fromListI <$> (identP "fromListI" *> readPrec)
        where
            identP str = lexP >>= guard . (== Ident str)

instance NFData a => NFData (Interval a) where
    rnf (Interval a b) = rnf (a, b)

instance NFData a => NFData (Diet a) where
    rnf (Diet s) = rnf s

-- | Checks if a value is within a range.
inRange :: Ord a => a -> (a, a) -> Bool
inRange x (l, u) = x >= l && x <= u

-- | Checks if two intervals overlap.
overlapping :: Ord a => Interval a -> Interval a -> Bool
overlapping (Interval la ua) (Interval lb ub) =
    (la `inRange` (lb, ub)) || (lb `inRange` (la, ua))

-- | Checks if two intervals are adjacent.
adjacent :: (Ord a, Enum a) => Interval a -> Interval a -> Bool
adjacent (Interval la ua) (Interval lb ub) =
    succ ua == lb || succ ub == la

-- | Checks if two intervals are apart (i.e. not overlapping or adjacent).
apart :: (Ord a, Enum a) => Interval a -> Interval a -> Bool
apart a b = not (overlapping a b) && not (adjacent a b)

-- | Tries to merge two intervals.
--
-- If the two interval are overlapping or adjacent, the merged interval is
-- returned along with the second interval., otherwise 'Nothing is returned.
merge :: (Enum a, Ord a) => Interval a -> Interval a -> Maybe (Interval a)
merge a@(Interval la ua) b@(Interval lb ub) =
    Interval (min la lb) (max ua ub)
    <$ guard (overlapping a b || adjacent a b)

-- | Removes the values in the second interval from the first interval.
remove :: (Enum a, Ord a) => Interval a -> Interval a -> [Interval a]
remove (Interval la ua) (Interval lb ub) =
    catMaybes [ Interval la (pred lb) <$ guard (la < lb)
              , Interval (succ ub) ua <$ guard (ub < ua)
              ]

-- | Returns the number of elements in an interval.
intervalSize :: Ix a => Interval a -> Int
intervalSize (Interval l u) = rangeSize (l, u)

-- | Returns all the elements in an interval.
intervalToList :: Enum a => Interval a -> [a]
intervalToList (Interval a b) = enumFromTo a b

-- | Checks if the interval is invalid (upper bound lower than lower bound).
intervalInvalid :: Ord a => Interval a -> Bool
intervalInvalid i = upper i < lower i

-- | Returns the lower bound of the interval.
lower :: Interval a -> a
lower (Interval l _) = l

-- | Returns the upper bound of the interval
upper :: Interval a -> a
upper (Interval _ u) = u

-- | Creates a point interval (an interval containing only a single value).
point :: a -> Interval a
point a = Interval a a

-- | Creates an empty set.
empty :: Diet a
empty = Diet S.empty

-- | Checks if the set is empty.
null :: Diet a -> Bool
null = S.null . unDiet

-- | Returns the number of elements in the set.
size :: Ix a => Diet a -> Int
size = foldr ((+) . intervalSize) 0 . unDiet

-- | Returns the number of intervals in the set.
sizeI :: Diet a -> Int
sizeI = S.size . unDiet

-- | Checks if the value is in the set.
member :: Ord a => a -> Diet a -> Bool
member a = S.member (point a) . unDiet

-- | Checks if the value is not in the set.
notMember :: Ord a => a -> Diet a -> Bool
notMember a = not . member a

-- | Looks up an interval in a set, then tries to merge it with the current interval.
lookupAndMerge :: (Ord a, Enum a)
               => (Interval a -> S.Set (Interval a) -> Maybe (Interval a))
               -> (Interval a,  Diet a)
               -> Maybe (Interval a, Diet a)
lookupAndMerge lookupFunc (interval, diet) = do
    let set = unDiet diet
    j <- lookupFunc interval set
    m <- merge interval j
    return (m, Diet $ S.delete j set)

-- | Applies a function returning 'Maybe' until it returns 'Nothing'.
whileJust :: (a -> Maybe a) -> a -> a
whileJust f a =
    case f a of
         Just b  -> whileJust f b
         Nothing -> a

-- | Inserts all the values in an 'Interval' into the set.
insertI :: (Enum a, Ord a) => Interval a -> Diet a -> Diet a
insertI i d
    | intervalInvalid i = d
    | otherwise         =
        let (a, s) = whileJust (lookupAndMerge S.lookupGT)
                     . fromMaybe (i, d)
                     $ lookupAndMerge S.lookupLE (i, d)
        in  Diet . S.insert a $ unDiet s

-- | Inserts a value into the set.
insert :: (Enum a, Ord a) => a -> Diet a -> Diet a
insert = insertI . point

-- | Looks up an interval in a set, then removes the lookup value from the found value.
lookupAndRemove :: (Ord a, Enum a)
                => Interval a
                -> (Interval a -> S.Set (Interval a) -> Maybe (Interval a))
                -> Diet a
                -> Maybe (Diet a)
lookupAndRemove interval lookupFunc d = do
    let s = unDiet d
    j <- lookupFunc interval s
    guard (interval `overlapping` j)
    return . Diet . foldr S.insert (S.delete j s) $ remove j interval

-- | Deletes all the values in an 'Interval' from the set.
deleteI :: (Enum a, Ord a) => Interval a -> Diet a -> Diet a
deleteI i d
    | intervalInvalid i = d
    | otherwise         =
        whileJust (lookupAndRemove i S.lookupGT)
        . fromMaybe d
        $ lookupAndRemove i S.lookupLE d

-- | Deletes a value from the set.
delete :: (Enum a, Ord a) => a -> Diet a -> Diet a
delete = deleteI . point

-- | Creates a new set, containing a single interval.
singletonI :: Interval a -> Diet a
singletonI = Diet . S.singleton

-- | Creates a new set, containing a single value.
singleton :: a -> Diet a
singleton = singletonI . point

-- | Returns the minimal element, and the set without that element, or
-- 'Nothing' if the set is empty.
minView :: (Enum a, Ord a) => Diet a -> Maybe (a, Diet a)
minView d
    | null d    = Nothing
    | otherwise = Just . (id &&& flip delete d) . lower . S.findMin $ unDiet d

-- | Returns the maximal element, and the set without that element, or
-- 'Nothing' if the set is empty.
maxView :: (Enum a, Ord a) => Diet a -> Maybe (a, Diet a)
maxView d
    | null d    = Nothing
    | otherwise = Just . (id &&& flip delete d) . upper . S.findMax $ unDiet d

-- | Gets a list of all intervals in the set.
toListI :: Diet a -> [Interval a]
toListI = S.toList . unDiet

-- | Gets a list of all the elements in the set.
toList :: Enum a => Diet a -> [a]
toList = concatMap intervalToList . S.toList . unDiet

-- | Creates a new set from a list of intervals.
fromListI :: (Enum a, Ord a) => [Interval a] -> Diet a
fromListI = foldr insertI empty

-- | Creates a new set from a list of values.
fromList :: (Enum a, Ord a) => [a] -> Diet a
fromList = foldr insert empty

-- | Checks that the Diet set is valid.
valid :: (Ord a, Enum a) => Diet a -> Bool
valid d =
    and $ concat [ uncurry (zipWith apart) . (id &&& tail) . S.toAscList $ unDiet d
                 , map (not . intervalInvalid) . S.toList $ unDiet d
                 ]
