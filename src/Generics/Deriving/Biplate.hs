{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-}

{-|
Module      : Generics.Deriving.Biplate
Copyright   : (c) Ferdinand van Walree 2017
License     : GPL-3
Maintainer  : ferdinandvwalree@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the Biplate type class and its operations.

To use the Biplate operations you will need to define a data type that derives Generic:

> data Tree = Node Int Int Tree Tree | Leaf deriving (Show, Generic)

The library will automatically derive an instance for your data types.

Each operation includes an example of what it does. In the examples I use the following value of type Tree:

> tree = Node 100 200 (Node 80 160 Leaf Leaf) (Node 60 120 Leaf Leaf)

-}


module Generics.Deriving.Biplate 
(
    Biplate(..)
)
where

import GHC.Exts     (build)
import GHC.Generics

import Generics.Deriving.Uniplate
import Generics.Deriving.Internal

-- | Type class for interacting with elements of type \@to in a structure of type \@from.
-- If \@from == \@to behaviour will default to Uniplate.
class Uniplate from => Biplate from to where
    -- * Core
    -- | Retrieves the direct children of the root that have type \@to
    --
    -- >>> childrenBi tree :: [Int]
    -- [100]
    childrenBi      :: from -> [to]
    -- | Constructs a function that takes a list of values of type \@to and if applied replaces the direct children of the root of type @to with the values of the list in a left-to-right order.
    -- 
    -- >>> contextBi tree [5] :: [Int]
    -- Node 5 (Node 80 Leaf Leaf) (Node 60 Leaf Leaf)
    contextBi       :: from -> [to] -> from
    -- | Constructs a tuple of two components: \@t1 and \@t2, where \@t1 is childrenBi and \@t2 is contextBi that have been applied to the given argument. We can reify the argument by applying \@t2 to \@t1.
    --
    -- >>> let (ch,ctx) = biplate tree
    -- >>> ctx (ch :: [Int])
    -- Node 100 (Node 80 Leaf Leaf) (Node 60 Leaf Leaf)
    biplate         :: from -> ([to], [to] -> from)
    -- | Retrieves all values of type \@to that are reachable from the root. A value is only reachable if it can be reached by only traversing values of type \@from
    --
    -- >>> universeBi tree :: [Int]
    -- [100,80,60]
    universeBi      :: from -> [to]
    -- | Applies a function to values of type \@to that are reachable from the root. A value is only reachable if it can be reached by only traversing values of type \@from
    --
    -- >>> let f x = x + 1 :: [Int]
    -- >>> transformBi f tree
    -- Node 101 (Node 81 Leaf Leaf) (Node 61 Leaf Leaf)
    transformBi     :: (to -> to) -> from -> from
    -- | Repeatedly applies a function to values of type \@to until a fixpoint has been reached. A value is only reachable if it can be reached by only traversing values of type \@from
    --
    -- @
    -- divByTwo :: Int -> Maybe Int
    -- divByTwo x | y % 2 == 0 = Just y
    --            | otherwise = Nothing
    --      where y = x `div` 2
    -- @
    -- >>> rewriteBi divByTwo tree
    -- Node 25 (Node 5 Leaf Leaf) (Node 15 Leaf Leaf)
    rewriteBi       :: (to -> Maybe to) -> from -> from
    -- | Apply a function to the direct children of the root
    --
    -- >>> let f x = x + 1 :: [Int]
    -- >>> descendBi f tree
    -- Node 101 (Node 80 Leaf Leaf) (Node 60 Leaf Leaf)
    descendBi       :: (to -> to) -> from -> from
    -- | Constructs a tuple (t1, t2) for each value of type \@to that is a direct child of the root. Applying t2 to t1 will reconstruct the original \@from argument.
    -- We can replace t1 with a different value of type \@to to construct a value of type \@from that is different in only the child that corresponds to the t2 that we used.
    --
    -- >>> let (_:[hole]) = holesBi tree
    -- >>> snd hole (5 :: Int)
    -- Node 100 5 (Node 80 160 Leaf Leaf) (Node 60 120 Leaf Leaf)
    holesBi         :: from -> [(to, to -> from)]
    -- | Constructs a tuple (t1, t2) for each value of type \@to that is reachable from the root. Applying t2 to t1 will reconstruct the orignal \@from argument.
    -- We can replace t1 with a different value of type \@to to construct a value of type \@from that is different in only the value that corresponds to the t2 that we used.
    --
    -- >>> let (_:_:_:hole:xs) = contextsBi tree
    -- >>> snd hole (5 :: Int)
    -- Node 100 200 (Node 80 5 Leaf Leaf) (Node 60 120 Leaf Leaf)
    contextsBi      :: from -> [(to, to -> from)]
    transformBiM    :: Monad m => (to -> m to) -> from -> m from
    rewriteBiM      :: Monad m => (to -> m (Maybe to)) -> from -> m from
    descendBiM      :: Monad m => (to -> m to) -> from -> m from


-- | Instance that defaults to Uniplate behaviour
instance {-# OVERLAPPING #-} Uniplate from => Biplate from from where
    childrenBi = children
    contextBi = context
    biplate = uniplate
    universeBi = universe
    transformBi = transform
    rewriteBi = rewrite
    descendBi = descend
    holesBi = holes
    contextsBi = contexts
    transformBiM = transformM
    rewriteBiM = rewriteM
    descendBiM = descendM

-- | Instance that defines the Biplate behaviour
instance {-# OVERLAPPABLE #-} (Uniplate from, Generic from, Children' (Rep from) to, Context' (Rep from) to) => Biplate from to where
    -- childrenBi and contextBi are the core functions and can be defined in their generic versions
    childrenBi x = children' (from x)
    contextBi x xs = to $ snd $ context' (from x) xs
    biplate x = (childrenBi x, contextBi x)

    -- universeBi is defined using 'build' for fusion. Unlike Uniplate, I need to make use of both childrenBi and children.
    -- childrenBi extracts the direct children of the Parent, children allows us to go one level deeper and repeat the previous step.
    universeBi x = build (go x)
        where
            go :: (Uniplate from, Biplate from to) => from -> (to -> t -> t) -> t -> t
            go x cons nil = foldr cons go' (childrenBi x)
                where
                    go' = foldr ($) nil $ map (\c -> go c cons) $ children x
    -- Similar to universeBi, but instead of extracting values we apply a function to these values
    transformBi f x = contextBi desc $ map f $ childrenBi desc
        where desc = context x $ map (transformBi f) (children x)
    rewriteBi fm x = transformBi g x
        where g x = maybe x g (fm x)
    descendBi f x = contextBi x $ map f $ childrenBi x
    holesBi x = uncurry f (biplate x)
      where 
        f [] _ = []
        f (x:xs) gen = (x, gen . (:xs)) : f xs (gen . (x:))
    contextsBi x = holesBi x ++ f (holes x)
        where
           f xs = [ (y, ctx . context)
                  | (child, ctx) <- xs
                  , (y, context) <- contextsBi child]
    -- monadic versions of some of the above functions
    transformBiM mf x = mapM mf (childrenBi x) >>= \xs -> mdesc >>= \desc -> return $ contextBi desc xs
        where mdesc = mapM (transformBiM mf) (children x) >>= return . context x
    rewriteBiM mf x = transformBiM g x
        where g x = mf x >>= \ma -> maybe (return x) g ma
    descendBiM mf x = mapM mf (childrenBi x) >>= return . contextBi x