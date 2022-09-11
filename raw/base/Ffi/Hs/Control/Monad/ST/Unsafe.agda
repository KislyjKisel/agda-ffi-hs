{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.ST.Unsafe where

open import Agda.Builtin.IO         using (IO)
open import Agda.Primitive
open import Ffi.Hs.Control.Monad.ST using (ST)

private
    variable
        aℓ : Level
        A : Set aℓ
        S : Set

postulate
    unsafeInterleaveST        : ST S A → ST S A
    unsafeDupableInterleaveST : ST S A → ST S A
    unsafeIOToST              : IO A → ST S A
    unsafeSTToIO              : ST S A → IO A

{-# FOREIGN GHC import qualified Control.Monad.ST.Unsafe #-}
{-# COMPILE GHC unsafeInterleaveST        = \ s aℓ a -> Control.Monad.ST.unsafeInterleaveST        #-}
{-# COMPILE GHC unsafeDupableInterleaveST = \ s aℓ a -> Control.Monad.ST.unsafeDupableInterleaveST #-}
{-# COMPILE GHC unsafeIOToST              = \ aℓ a s -> Control.Monad.ST.unsafeIOToST              #-}
{-# COMPILE GHC unsafeSTToIO              = \ s aℓ a -> Control.Monad.ST.unsafeSTToIO              #-}
