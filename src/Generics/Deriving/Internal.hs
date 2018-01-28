{-# LANGUAGE TypeOperators 			#-}
{-# LANGUAGE MultiParamTypeClasses 	#-}
{-# LANGUAGE FlexibleInstances 		#-}

module Generics.Deriving.Internal where

import Generics.Deriving hiding (children',context')

class Children' f on where
    children' :: f a -> [on]

instance Children' U1 a where
    children' _ = []

instance {-# OVERLAPPING #-} Children' (K1 i a) a where
    children' (K1 x) = [x]

instance {-# OVERLAPPABLE #-} Children' (K1 i a) b where
    children' _ = [] 

instance (Children' a c, Children' b c) => Children' (a :*: b) c where
    children' (a :*: b) = children' a ++ children' b

instance (Children' a c, Children' b c) => Children' (a :+: b) c where
    children' (L1 a) = children' a
    children' (R1 b) = children' b

instance Children' a b => Children' (M1 i c a) b where
    children' (M1 x) = children' x


class Context' f on where
    context' :: f a -> [on] -> ([on], f a)

instance Context' U1 a where
    context' x xs = (xs, x)

instance {-# OVERLAPPING #-} Context' (K1 i a) a where
    context' (K1 _) (x:xs) = (xs, K1 x)
    context' _ [] = error "context did not receive enough arguments"

instance {-# OVERLAPPABLE #-} Context' (K1 i a) b where
    context' (K1 x) xs = (xs, K1 x)

instance {-# OVERLAPPABLE #-} (Context' a c, Context' b c) => Context' (a :*: b) c where
    context' (a :*: b) xs = (xs'', a' :*: b')
        where (xs', a') = context' a xs
              (xs'',b') = context' b xs'

instance (Context' a c, Context' b c) => Context' (a :+: b) c where
    context' (L1 a) xs = let (xs', a') = context' a xs in (xs', L1 a')
    context' (R1 b) xs = let (xs', b') = context' b xs in (xs', R1 b')

instance Context' a b => Context' (M1 i c a) b where
    context' (M1 x) xs = let (xs', x') = context' x xs in (xs', M1 x')