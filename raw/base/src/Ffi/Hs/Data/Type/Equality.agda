{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Type.Equality where

open import Agda.Primitive
open import Ffi.Hs.-base.Kind using (IsKind)

private
    variable
        aℓ bℓ : Level
        A B C : Set aℓ
        F G : Set aℓ → Set bℓ

    open import Ffi.Hs.-base.Kind-Instanced

infix 4 _:~:_ _:~~:_

data _:~:_ {K : Set (lsuc aℓ)} ⦃ _ : IsKind K ⦄ (A : K) : K → Set (lsuc aℓ) where
    Refl : A :~: A

{-# FOREIGN GHC
type AgdaTypeEq aℓ k isk (a :: k) = (a Type.Reflection.:~:)
#-}
{-# COMPILE GHC _:~:_ = data(4) AgdaTypeEq (Type.Reflection.Refl) #-}

-- same level kinds, otherwise _:~~:_ is in Setω (sort can't depend on indices)
data _:~~:_
        {K₁ : Set (lsuc aℓ)} ⦃ _ : IsKind K₁ ⦄ (A : K₁) :
        {K₂ : Set (lsuc aℓ)} ⦃ _ : IsKind K₂ ⦄ (B : K₂) → Set (lsuc (lsuc aℓ))
        where
    HRefl : _:~~:_ A {K₂ = K₁} A

{-# FOREIGN GHC
type AgdaTypeHEq aℓ k1 isk1 (a :: k1) k2 isk2 (b :: k2) = a Type.Reflection.:~~: b
#-}
{-# COMPILE GHC _:~~:_ = data(7) AgdaTypeHEq (Type.Reflection.HRefl) #-}

postulate
    sym : _:~:_ A B -- → _:~:_ B A 
    -- trans : A :~: B → B :~: C → A :~: C
    -- castWith : A :~: B → A → B
    -- -- gcastWith : A :~: B → 
    -- apply : F :~: G → A :~: B → F A :~: G B
    -- inner : F A :~: G B → A :~: B
    -- outer : F A :~: G B → F :~: G
    -- _==_ : 
