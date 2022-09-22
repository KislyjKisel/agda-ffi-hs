{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Concurrent where

open import Agda.Builtin.IO           using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit         using (⊤; ⊤′)
open import Ffi.Hs.Foreign.ForeignPtr using (ForeignPtr)
open import Ffi.Hs.Foreign.Ptr        using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.Concurrent
#-}

private
    variable
        aℓ ℓ : Level
        A : Set aℓ

postulate
    newForeignPtr          : Ptr A → IO (⊤′ {ℓ}) → IO (ForeignPtr A)
    addForeignPtrFinalizer : ForeignPtr A → IO (⊤′ {ℓ}) → IO (⊤′ {ℓ})

{-# COMPILE GHC newForeignPtr          = \ aℓ a ℓ -> Foreign.Concurrent.newForeignPtr          #-}
{-# COMPILE GHC addForeignPtrFinalizer = \ ℓ      -> Foreign.Concurrent.addForeignPtrFinalizer #-}
