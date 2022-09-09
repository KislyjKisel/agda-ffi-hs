{-# OPTIONS --without-K #-}

module Ffi.Hs.System.IO.Unsafe where

open import Agda.Builtin.IO using (IO)
open import Agda.Primitive

{-# FOREIGN GHC
import qualified System.IO.Unsafe
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    unsafePerformIO        : IO A → A
    unsafeDupablePerformIO : IO A → A
    unsafeInterleaveIO     : IO A → IO A
    unsafeFixIO            : (A → IO A) → IO A

{-# COMPILE GHC unsafePerformIO        = \ aℓ a -> System.IO.Unsafe.unsafePerformIO        #-}
{-# COMPILE GHC unsafeDupablePerformIO = \ aℓ a -> System.IO.Unsafe.unsafeDupablePerformIO #-}
{-# COMPILE GHC unsafeInterleaveIO     = \ aℓ a -> System.IO.Unsafe.unsafeInterleaveIO     #-}
{-# COMPILE GHC unsafeFixIO            = \ aℓ a -> System.IO.Unsafe.unsafeFixIO            #-}
