{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Concurrent where

open import Agda.Builtin.IO           using (IO)
open import Ffi.Hs.-base.Unit         using (⊤)
open import Ffi.Hs.Foreign.ForeignPtr using (ForeignPtr)
open import Ffi.Hs.Foreign.Ptr        using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.Concurrent
#-}

postulate
    newForeignPtr          : ∀{aℓ} {A : Set aℓ} {ℓ} → Ptr A → IO (⊤ {ℓ}) → IO (ForeignPtr A)
    addForeignPtrFinalizer : ∀{ℓ} → ForeignPtr A → IO (⊤ {ℓ}) → IO (⊤ {ℓ})

{-# COMPILE GHC newForeignPtr          = \ aℓ a ℓ -> Foreign.Concurrent.newForeignPtr          #-}
{-# COMPILE GHC addForeignPtrFinalizer = \ ℓ      -> Foreign.Concurrent.addForeignPtrFinalizer #-}
