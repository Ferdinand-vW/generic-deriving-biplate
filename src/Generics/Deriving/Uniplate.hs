{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}

{-|
Module      : Generics.Deriving.Uniplate
Copyright   : (c) Ferdinand van Walree 2017
License     : GPL-3
Maintainer  : ferdinandvwalree@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the Uniplate type class and its operations.

To use the Uniplate operations you will need to define a data type that derives Generic:

> data Tree = Node Int Int Tree Tree | Leaf deriving (Show, Generic)

The library will automatically derive an instance for your data types.

Each operation includes an example of what it does. In the examples I use the following value of type Tree:

> tree = Node 100 200 (Node 80 160 Leaf Leaf) (Node 60 120 Leaf Leaf)

-}

module Generics.Deriving.Uniplate
(
    Uniplate(..)
) where

import GHC.Generics
import GHC.Exts (build)
import Control.Monad (liftM)

import Generics.Deriving.Internal


class Uniplate on where
    -- | Retrieves all direct children of the root that have the same type
    --
    -- >>> children tree
    -- [Node 80 160 Leaf Leaf, Node 60 120 Leaf Leaf]
    children :: on -> [on]
    -- | Constructs a function that takes a list of values of type \@on and if applied replaces the direct children of the root of type @on with the values of the list in a left-to-right order.
    -- 
    -- >>> context tree [Leaf, Leaf]
    -- Node 100 200 Leaf leaf
    context :: on -> [on] -> on
    -- | Constructs a tuple of two components: \@t1 and \@t2, where \@t1 is children and \@t2 is context that have been applied to the given argument. We can reify the argument by applying \@t2 to \@t1.
    --
    -- >>> let (ch,ctx) = uniplate tree
    -- >>> ctx ch
    -- Node 100 200 (Node 80 160 Leaf Leaf) (Node 60 120 Leaf Leaf)
    uniplate :: on -> ([on], [on] -> on)
    -- | Retrieves all values of type \@to that are reachable from the root. A value is only reachable if it can be reached by only traversing values of type \@from
    --
    -- >>> universe tree
    -- [Node 100 200 (Node 80 160 Leaf Leaf) (Node 60 120 Leaf leaf), Node 80 160 Leaf Leaf, Leaf Leaf, Node 60 120 Leaf Leaf, Leaf Leaf]
    universe :: on -> [on]
    -- | Applies a function to values of type \@to that are reachable from the root. A value is only reachable if it can be reached by only traversing values of type \@from
    --
    -- >>> let f x = Leaf
    -- >>> transform f tree
    -- Leaf
    transform :: (on -> on) -> on -> on
    rewrite :: (on -> Maybe on) -> on -> on
    descend :: (on -> on) -> on -> on
    holes :: on -> [(on, on -> on)]
    contexts :: on -> [(on, on -> on)]
    transformM :: Monad m => (on -> m on) -> on -> m on
    rewriteM :: Monad m => (on -> m (Maybe on)) -> on -> m on
    descendM :: Monad m => (on -> m on) -> on -> m on
    para :: (on -> [r] -> r) -> on -> r

data Bottom

instance {-# OVERLAPPING #-} Uniplate Bottom where
    children    = error "Cannot match on Bottom"
    context     = error "Cannot match on Bottom"
    uniplate    = error "Cannot match on Bottom"
    universe    = error "Cannot match on Bottom"
    transform   = error "Cannot match on Bottom"
    rewrite     = error "Cannot match on Bottom"
    descend     = error "Cannot match on Bottom"
    contexts    = error "Cannot match on Bottom"
    holes       = error "Cannot match on Bottom"
    para        = error "Cannot match on Bottom"
    transformM  = error "Cannot match on Bottom"
    rewriteM    = error "Cannot match on Bottom"
    descendM    = error "Cannot match on Bottom"

instance {-# OVERLAPPABLE #-} (Generic on, Context' (Rep on) on, Children' (Rep on) on) => Uniplate on where
    children x = children' (from x)
    context x xs = to $ snd $ context' (from x) xs
    uniplate x = (children x, context x)
    universe x = build (go x)
        where
            go x cons nil = cons x $ foldr ($) nil $ map (\c -> go c cons) $ children x
    transform f x = f $ context x $ map (transform f) (children x)
    rewrite f = transform g
        where g x = maybe x (rewrite f) (f x)
    descend f x = context x $ map f $ children x
    contexts x = (x,id) : f (holes x)
      where
        f xs = [ (y, ctx . context)
               | (child, ctx) <- xs
               , (y, context) <- contexts child]
    holes x = uncurry f (uniplate x)
      where 
        f [] _ = []
        f (x:xs) gen = (x, gen . (:xs)) : f xs (gen . (x:))
    para op x = op x $ map (para op) $ children x
    transformM mf x = liftM (context x) $ mapM (transformM mf) (children x)
    rewriteM mf = transformM g
        where g x = mf x >>= \ma -> maybe (return x) g ma
    descendM mf x = mapM mf (children x) >>= return . context x