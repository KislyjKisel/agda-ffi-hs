{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.STM.TArray where

open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq; Ix)
open import Ffi.Hs.Control.Monad.STM using (STM)
open import Ffi.Hs.Data.Array.MArray using (MArray)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Concurrent.STM.TArray
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Array.MArray (AgdaMArray)
#-}

private
    variable
        iℓ eℓ : Level
        E : Set eℓ
        I : Set iℓ

postulate
    TArray : Set iℓ → Set eℓ → Set (iℓ ⊔ eℓ)
    MArray[TArray,E,STM] : MArray TArray E STM
    Eq[TArray[I,E]] : ⦃ Ix I ⦄ → Eq (TArray I E)

{-# FOREIGN GHC type AgdaTArray iℓ eℓ = Control.Concurrent.STM.TArray.TArray #-}
{-# COMPILE GHC TArray = type(2) AgdaTArray #-}

{-# COMPILE GHC MArray[TArray,E,STM] = \ aℓ e -> AgdaMArray         #-}
{-# COMPILE GHC Eq[TArray[I,E]]      = \ iℓ i eℓ e AgdaIx -> AgdaEq #-}
