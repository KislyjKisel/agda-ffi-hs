{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.StablePtr where

open import Agda.Builtin.IO    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Eq; Storable)
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Foreign.Ptr using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.StablePtr
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    StablePtr : Set aℓ → Set aℓ

    Storable[StablePtr[A]] : Storable (StablePtr A)
    Eq[StablePtr[A]]       : Eq (StablePtr A)

    newStablePtr       : A → IO (StablePtr A)
    deRefStablePtr     : StablePtr A → IO A
    freeStablePtr      : StablePtr A → IO (⊤ {lzero})
    castStablePtrToPtr : StablePtr A → Ptr A
    castPtrToStablePtr : Ptr A → StablePtr A

{-# FOREIGN GHC type AgdaStablePtr aℓ = Foreign.StablePtr.StablePtr #-}
{-# COMPILE GHC StablePtr = type(1) AgdaStablePtr #-}

{-# COMPILE GHC Storable[StablePtr[A]] = \ aℓ a -> AgdaStorable #-}
{-# COMPILE GHC Eq[StablePtr[A]]       = \ aℓ a -> AgdaEq       #-}

{-# COMPILE GHC newStablePtr       = \ aℓ a -> Foreign.StablePtr.newStablePtr       #-}
{-# COMPILE GHC deRefStablePtr     = \ aℓ a -> Foreign.StablePtr.deRefStablePtr     #-}
{-# COMPILE GHC freeStablePtr      = \ aℓ a -> Foreign.StablePtr.freeStablePtr      #-}
{-# COMPILE GHC castStablePtrToPtr = \ aℓ a -> Foreign.StablePtr.castStablePtrToPtr #-}
{-# COMPILE GHC castPtrToStablePtr = \ aℓ a -> Foreign.StablePtr.castPtrToStablePtr #-}
