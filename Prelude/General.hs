-- | Prelude replacement, use the NoImplicitPrelude extension before importing
--   this.

module Prelude.General(
    module Control.Applicative
  , module Control.Arrow
  , module Control.Category
  , module Control.Exception
  , module Control.Lens
  , module Control.Monad
  , module Data.Bool
  , module Data.Either
  , module Data.Eq
  , module Data.Foldable
  , module Data.Function
  , module Data.Function.Pointless
  , module Data.Int
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Ord
  , module Data.String
  , module Data.Traversable
  , module Data.Tuple
  , module Filesystem.Path
  , module System.IO
  , module Text.Show

  , (+), (-), (*), (/), mod
  , seq, ($!)
  , undefined, error
  , fromIntegral

  , if'
  , filter
  ) where

import           Control.Applicative
import           Control.Arrow ( first
                               , second )
import           Control.Category
import           Control.Exception
import           Control.Lens
import           Control.Monad hiding ( forM
                                      , forM_
                                      , mapM
                                      , mapM_
                                      , msum
                                      , sequence
                                      , sequence_ )
import           Data.Bool
import           Data.Either
import           Data.Eq
import           Data.Foldable
import           Data.Function hiding ( (.)
                                      , id )
import           Data.Function.Pointless
import           Data.Int
import           Data.List hiding ( all
                                  , and
                                  , any
                                  , concat
                                  , concatMap
                                  , elem
                                  , filter
                                  , find
                                  , foldl
                                  , foldl'
                                  , foldl1
                                  , foldr
                                  , foldr1
                                  , mapAccumL
                                  , mapAccumR
                                  , maximum
                                  , maximumBy
                                  , minimum
                                  , minimumBy
                                  , notElem
                                  , or
                                  , product
                                  , sum )
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String
import           Data.Traversable
import           Data.Tuple
import           Filesystem.Path hiding ( (<.>)
                                        , concat
                                        , empty
                                        , null
                                        , stripPrefix )
import           System.IO hiding ( FilePath )
import           Text.Show

import qualified Prelude
import           Prelude ( (+), (-), (*), (/), mod
                         , seq, ($!)
                         , undefined, error, fromIntegral )

-- | An either/maybe equivalent for Bool, often known as if'
if' :: a -- ^ Returned if the bool is True
        -> a -- ^ Returned if the bool is False
        -> Bool
        -> a
if' a _ True  = a
if' _ a False = a

filter :: (Monad m, Monoid (m a), Foldable t) => (a -> Bool) -> t a -> m a
filter p = foldMap (\a -> if' (return a) mempty (p a))

-- instance Foldable ((,) a) where
--     foldMap f (_, x) = f x

-- instance Traversable ((,) a) where
--     traverse f (a, x) = (,) a <$> f x
