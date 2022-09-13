{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.ForeignPtr where

open import Agda.Builtin.IO    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Foreign.Ptr using (Ptr; FunPtr)

{-# FOREIGN GHC
import qualified Foreign.ForeignPtr
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ eℓ : Level
        A B : Set aℓ

postulate
    ForeignPtr : Set aℓ → Set aℓ

    Data[ForeignPtr[A]] : ⦃ Data A ⦄ → Data (ForeignPtr A)
    Show[ForeignPtr[A]] : Show (ForeignPtr A)
    Eq[ForeignPtr[A]]   : Eq (ForeignPtr A)
    Ord[ForeignPtr[A]]  : Ord (ForeignPtr A)

{-# FOREIGN GHC type AgdaForeignPtr aℓ = Foreign.ForeignPtr.ForeignPtr #-}
{-# COMPILE GHC ForeignPtr = type(1) AgdaForeignPtr #-}

{-# COMPILE GHC Data[ForeignPtr[A]] = \ aℓ a AgdaData -> AgdaData #-}
{-# COMPILE GHC Show[ForeignPtr[A]] = \ aℓ a          -> AgdaShow #-}
{-# COMPILE GHC Eq[ForeignPtr[A]]   = \ aℓ a          -> AgdaEq   #-}
{-# COMPILE GHC Ord[ForeignPtr[A]]  = \ aℓ a          -> AgdaOrd  #-}

FinalizerPtr : ∀{ℓ} → Set aℓ → Set (aℓ ⊔ ℓ)
FinalizerPtr {ℓ = ℓ} A = FunPtr (Ptr A → IO (⊤ {ℓ}))

FinalizerEnvPtr : ∀{ℓ} → Set eℓ → Set aℓ → Set (eℓ ⊔ aℓ ⊔ ℓ)
FinalizerEnvPtr {ℓ = ℓ} Env A = FunPtr (Ptr Env → Ptr A → IO (⊤ {ℓ}))

postulate
    newForeignPtr             : ∀{ℓ} → FinalizerPtr {ℓ = ℓ} A → Ptr A → IO (ForeignPtr A)
    newForeignPtr_            : Ptr A → IO (ForeignPtr A)
    addForeignPtrFinalizer    : ∀{ℓ} → FinalizerPtr {ℓ = ℓ} A → ForeignPtr A → IO (⊤ {lzero})
    newForeignPtrEnv          : ∀{ℓ} {Env : Set eℓ} → FinalizerEnvPtr {ℓ = ℓ} Env A → Ptr Env → Ptr A → IO (ForeignPtr A)
    addForeignPtrFinalizerEnv : ∀{ℓ} {Env : Set eℓ} → FinalizerEnvPtr {ℓ = ℓ} Env A → Ptr Env → ForeignPtr A → IO (⊤ {lzero}) 
    withForeignPtr            : ForeignPtr A → (Ptr A → IO B) → IO B
    finalizeForeignPtr        : ForeignPtr A → IO (⊤ {lzero})

    touchForeignPtr : ForeignPtr A → IO (⊤ {lzero})
    castForeignPtr  : ForeignPtr A → ForeignPtr B
    plusForeignPtr  : ForeignPtr A → Int → ForeignPtr B

    mallocForeignPtr       : ⦃ Storable A ⦄ → IO (ForeignPtr A)
    mallocForeignPtrBytes  : Int → IO (ForeignPtr A)
    mallocForeignPtrArray  : ⦃ Storable A ⦄ → Int → IO (ForeignPtr A)
    mallocForeignPtrArray0 : ⦃ Storable A ⦄ → Int → IO (ForeignPtr A)

{-# COMPILE GHC newForeignPtr             = \ aℓ a ℓ        -> Foreign.ForeignPtr.newForeignPtr             #-}
{-# COMPILE GHC newForeignPtr_            = \ aℓ a          -> Foreign.ForeignPtr.newForeignPtr_            #-}
{-# COMPILE GHC addForeignPtrFinalizer    = \ aℓ a ℓ        -> Foreign.ForeignPtr.addForeignPtrFinalizer    #-}
{-# COMPILE GHC newForeignPtrEnv          = \ eℓ aℓ a ℓ env -> Foreign.ForeignPtr.newForeignPtrEnv          #-}
{-# COMPILE GHC addForeignPtrFinalizerEnv = \ eℓ aℓ a ℓ env -> Foreign.ForeignPtr.addForeignPtrFinalizerEnv #-}
{-# COMPILE GHC withForeignPtr            = \ aℓ a bℓ b     -> Foreign.ForeignPtr.withForeignPtr            #-}
{-# COMPILE GHC finalizeForeignPtr        = \ aℓ a          -> Foreign.ForeignPtr.finalizeForeignPtr        #-}

{-# COMPILE GHC touchForeignPtr = \ aℓ a      -> Foreign.ForeignPtr.touchForeignPtr #-}
{-# COMPILE GHC castForeignPtr  = \ aℓ a bℓ b -> Foreign.ForeignPtr.castForeignPtr  #-}
{-# COMPILE GHC plusForeignPtr  = \ aℓ a bℓ b -> Foreign.ForeignPtr.plusForeignPtr  #-}

{-# COMPILE GHC mallocForeignPtr       = \ aℓ a AgdaStorable -> Foreign.ForeignPtr.mallocForeignPtr       #-}
{-# COMPILE GHC mallocForeignPtrBytes  = \ aℓ a              -> Foreign.ForeignPtr.mallocForeignPtrBytes  #-}
{-# COMPILE GHC mallocForeignPtrArray  = \ aℓ a AgdaStorable -> Foreign.ForeignPtr.mallocForeignPtrArray  #-}
{-# COMPILE GHC mallocForeignPtrArray0 = \ aℓ a AgdaStorable -> Foreign.ForeignPtr.mallocForeignPtrArray0 #-}
