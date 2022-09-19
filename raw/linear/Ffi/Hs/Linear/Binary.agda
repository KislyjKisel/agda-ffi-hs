{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.Binary where

open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Foldable; Applicative; Traversable)
open import Ffi.Hs.Data.Binary     using (Binary)
open import Ffi.Hs.Data.Binary.Get using (Get)
open import Ffi.Hs.Data.Binary.Put using (Put)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.Binary
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Binary (AgdaBinary(AgdaBinary))
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        F : Set aℓ → Set bℓ

postulate
    putLinear : ⦃ Binary A ⦄ → ⦃ Foldable F ⦄ → F A → Put
    getLinear : ⦃ Binary A ⦄ → ⦃ Applicative F ⦄ → ⦃ Traversable F ⦄ → Get (F A)

{-# COMPILE GHC putLinear = \ aℓ a bℓ f AgdaBinary AgdaFoldable                 -> Linear.Binary.putLinear #-}
{-# COMPILE GHC getLinear = \ aℓ a f AgdaBinary AgdaApplicative AgdaTraversable -> Linear.Binary.getLinear #-}
