{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.ForeignPtr.Unsafe where

open import Ffi.Hs.Foreign.ForeignPtr using (ForeignPtr)
open import Ffi.Hs.Foreign.Ptr        using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.ForeignPtr.Unsafe
#-}

postulate
    unsafeForeignPtrToPtr : ∀{aℓ} {A : Set aℓ} → ForeignPtr A → Ptr A

{-# COMPILE GHC unsafeForeignPtrToPtr = \ aℓ a -> Foreign.ForeignPtr.Unsafe.unsafeForeignPtrToPtr #-}
