{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) f (Compose a) = Compose $ f <$$> a

-- Implement the (<*>) function for an Apply instance for Compose
instance (Apply f, Apply g) => Apply (Compose f g) where
  (<*>) (Compose f) (Compose a) = Compose $ lift2 (<*>) f a

-- Implement the pure function for an Applicative instance for Compose
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure

-- Implement the (=<<) function for a Bind instance for Compose
instance (Bind f, Bind g) => Bind (Compose f g) where
  (=<<) = error "impossible"
