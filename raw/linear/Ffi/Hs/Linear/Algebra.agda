{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.Algebra where

open import Agda.Primitive

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified 
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        rℓ mℓ : Level
        R : Set rℓ
        M : Set mℓ


postulate
    Algebra : Set rℓ → Set mℓ → Set (rℓ ⊔ mℓ)
    Algebra[R,M]⇒Num[R] : ⦃ Algebra R M ⦄ → Num R

    mult : ⦃ Algebra R M ⦄ → (M → M → R) → M → R
    unital : ⦃ Algebra R M ⦄ → R → M → R

    Algebra[R,⊤] : ⦃ Num R ⦄ → Algebra R ⊤
    Algebra[R,Void] : ⦃ Num R ⦄ → Algebra R Void

-- todo...