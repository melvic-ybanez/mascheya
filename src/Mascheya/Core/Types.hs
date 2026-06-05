module Mascheya.Core.Types where

type Endo a = a -> a

-- Named after the `pure` function under the Applicative functor typeclass
type Pure f a = a -> f a