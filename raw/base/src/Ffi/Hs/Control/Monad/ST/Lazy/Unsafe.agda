{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.ST.Lazy.Unsafe where

open import Agda.Builtin.IO using (IO)
open import Agda.Primitive
open import Ffi.Hs.Control.Monad.ST.Lazy using (ST)

private
    variable
        aℓ : Level
        A : Set aℓ
        S : Set

postulate
    unsafeInterleaveST : ST S A → ST S A
    unsafeIOToST       : IO A → ST S A

{-# FOREIGN GHC import qualified Control.Monad.ST.Lazy.Unsafe #-}
{-# COMPILE GHC unsafeInterleaveST = \ s aℓ a -> Control.Monad.ST.unsafeInterleaveST #-}
{-# COMPILE GHC unsafeIOToST       = \ aℓ a s -> Control.Monad.ST.unsafeIOToST       #-}
