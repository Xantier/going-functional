module JoinList where

import Data.Monoid

import Buffer
import Editor
import Sized

data JoinList m a = Empty
  | Single m a 
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a `mappend` tag b) a b

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index (Single _ a)
  | index == 0 = Just a
  | otherwise  = Nothing
indexJ index (Append m l1 l2)
  | index < 0 || index > size0 = Nothing
  | index < size1              = indexJ index l1
  | otherwise                  = indexJ (index - size1) l2
    where size0 = getSize . size $ m
          size1 = getSize . size . tag $ l1
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty        = Empty
dropJ n l | n <= 0   = l
dropJ n (Single _ _) = Empty
dropJ n (Append _ l1 l2)
  | n < sz_l1         = dropJ n l1 +++ l2
  | otherwise         = dropJ (n - sz_l1) l2
  where sz_l1 = getSize . size . tag $ l1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty          = Empty
takeJ n l | n <= 0     = Empty
takeJ n s@(Single _ _) = s
takeJ n (Append _ l1 l2)
  | n <= sz_l1          = takeJ n l1
  | otherwise           = l1 +++ takeJ (n - sz_l1) l2
  where sz_l1 = getSize . size . tag $ l1
