{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Level where

open import Agda.Primitive

record Liftℓ {aℓ} ℓ (A : Set aℓ) : Set (aℓ ⊔ ℓ) where
    constructor liftℓ
    field
        unliftℓ : A

open Liftℓ public
    using (unliftℓ)

{-# FOREIGN GHC
type AgdaLiftℓ aℓ ℓ a = a

pattern AgdaLiftℓ :: a -> AgdaLiftℓ aℓ ℓ a
pattern AgdaLiftℓ x = x

{-# COMPLETE AgdaLiftℓ #-}
#-}

{-# COMPILE GHC Liftℓ = data AgdaLiftℓ (AgdaLiftℓ) #-}
