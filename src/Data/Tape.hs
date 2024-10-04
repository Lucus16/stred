module Data.Tape where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Prelude

data Tape a = Tape [a] a [a]

singleton :: a -> Tape a
singleton x = Tape [] x []

fromList :: [a] -> Maybe (Tape a)
fromList [] = Nothing
fromList (x : xs) = Just (Tape [] x xs)

previous :: Tape a -> Maybe (Tape a)
previous (Tape [] _ _) = Nothing
previous (Tape (x : before) x' after) = Just (Tape before x (x' : after))

next :: Tape a -> Maybe (Tape a)
next (Tape _ _ []) = Nothing
next (Tape before x (x' : after)) = Just (Tape (x : before) x' after)

first :: Tape a -> Tape a
first (Tape before cur after) = case reverse before of
  [] -> Tape [] cur after
  (cur' : after') -> Tape [] cur' (after' <> (cur : after))

last :: Tape a -> Tape a
last (Tape before cur after) = case reverse after of
  [] -> Tape before cur []
  (cur' : before') -> Tape (before' <> (cur : before)) cur' []

discard :: Tape a -> Maybe (Tape a)
discard (Tape [] _ []) = Nothing
discard (Tape (x : before) _ []) = Just (Tape before x [])
discard (Tape before _ (x : after)) = Just (Tape before x after)

peek :: Tape a -> a
peek (Tape _ x _) = x

mapCurrent :: (a -> a) -> Tape a -> Tape a
mapCurrent f (Tape before cur after) = Tape before (f cur) after

toNonEmpty :: Tape a -> NonEmpty a
toNonEmpty (Tape before cur after) = NonEmpty.prependList (reverse before) (cur :| after)

toNonEmptyWith :: (a -> b) -> (a -> b) -> Tape a -> NonEmpty b
toNonEmptyWith f g (Tape before cur after) =
  NonEmpty.prependList (f <$> reverse before) (g cur :| (f <$> after))

toList :: Tape a -> [a]
toList (Tape before cur after) = reverse before <> (cur : after)

toListWith :: (a -> b) -> (a -> b) -> Tape a -> [b]
toListWith f g (Tape before cur after) =
  (f <$> reverse before) <> (g cur : (f <$> after))

replace :: a -> Tape a -> Tape a
replace x (Tape before _ after) = Tape before x after

pushBefore :: a -> Tape a -> Tape a
pushBefore x (Tape before x' after) = Tape before x (x' : after)

pushAfter :: a -> Tape a -> Tape a
pushAfter x (Tape before x' after) = Tape (x' : before) x after

size :: Tape a -> Int
size (Tape before _ after) = length before + 1 + length after
