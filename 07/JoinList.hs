module JoinList where

import Data.Monoid

import Buffer
import Editor

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
