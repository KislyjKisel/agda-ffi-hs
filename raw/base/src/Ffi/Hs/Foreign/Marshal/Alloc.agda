{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Marshal.Alloc where

open import Agda.Builtin.IO           using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class        using (Storable)
open import Ffi.Hs.-base.Unit         using (⊤)
open import Ffi.Hs.Data.Int           using (Int)
open import Ffi.Hs.Foreign.ForeignPtr using (FinalizerPtr)
open import Ffi.Hs.Foreign.Ptr        using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.Marshal.Alloc
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaStorable)
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ

postulate
    alloca             : ⦃ Storable A ⦄ → (Ptr A → IO B) → IO B
    allocaBytes        : Int → (Ptr A → IO B) → IO B
    allocaBytesAligned : Int → Int → (Ptr A → IO B) → IO B
    malloc             : ⦃ Storable A ⦄ → IO (Ptr A)
    mallocBytes        : Int → IO (Ptr A)
    calloc             : ⦃ Storable A ⦄ → IO (Ptr A)
    callocBytes        : Int → IO (Ptr A)
    realloc            : ⦃ Storable B ⦄ → Ptr A → IO (Ptr B)
    reallocBytes       : Ptr A → Int → IO (Ptr A)
    free               : Ptr A → IO (⊤ {lzero})
    finalizerFree      : FinalizerPtr {ℓ = aℓ} A

{-# COMPILE GHC alloca             = \ aℓ a bℓ b AgdaStorable -> Foreign.Marshal.Alloc.alloca             #-}
{-# COMPILE GHC allocaBytes        = \ aℓ a bℓ b              -> Foreign.Marshal.Alloc.allocaBytes        #-}
{-# COMPILE GHC allocaBytesAligned = \ aℓ a bℓ b              -> Foreign.Marshal.Alloc.allocaBytesAligned #-}
{-# COMPILE GHC malloc             = \ aℓ a      AgdaStorable -> Foreign.Marshal.Alloc.malloc             #-}
{-# COMPILE GHC mallocBytes        = \ aℓ a                   -> Foreign.Marshal.Alloc.mallocBytes        #-}
{-# COMPILE GHC calloc             = \ aℓ a      AgdaStorable -> Foreign.Marshal.Alloc.calloc             #-}
{-# COMPILE GHC callocBytes        = \ aℓ a                   -> Foreign.Marshal.Alloc.callocBytes        #-}
{-# COMPILE GHC realloc            = \ aℓ a bℓ b AgdaStorable -> Foreign.Marshal.Alloc.realloc            #-}
{-# COMPILE GHC reallocBytes       = \ aℓ a                   -> Foreign.Marshal.Alloc.reallocBytes       #-}
{-# COMPILE GHC free               = \ aℓ a                   -> Foreign.Marshal.Alloc.free               #-}
{-# COMPILE GHC finalizerFree      = \ ℓ aℓ a                 -> Foreign.Marshal.Alloc.finalizerFree      #-}
