{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.Trace where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Functor; Num)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.Trace
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        M : Set aℓ → Set aℓ

postulate
    Trace : (Set aℓ → Set aℓ) → Set aℓ
    Trace[M]⇒Functor[M] : ⦃ Trace M ⦄ → Functor M

    trace    : ⦃ Trace M ⦄ → ⦃ Num A ⦄ → M (M A) → A
    diagonal : ⦃ Trace M ⦄ → ⦃ Num A ⦄ → M (M A) → M A
    -- todo: frobenius : ⦃ Num A ⦄ → ⦃ Foldable F ⦄ → ⦃ Additive F ⦄ → ⦃ Additive G ⦄ → ⦃ Distributive G ⦄ → ⦃ Trace G ⦄ → F (G A) → A

-- todo: instances

{-# FOREIGN GHC data AgdaTrace mℓ m = Linear.Trace.Trace m => AgdaTrace #-}
{-# COMPILE GHC Trace = type(0) AgdaTrace #-}

{-# COMPILE GHC Trace[M]⇒Functor[M] = \ mℓ m AgdaTrace -> AgdaFunctor #-}

{-# COMPILE GHC trace    = \ mℓ m a AgdaTrace AgdaNum -> Linear.Trace.trace    #-}
{-# COMPILE GHC diagonal = \ mℓ m a AgdaTrace AgdaNum -> Linear.Trace.diagonal #-}
