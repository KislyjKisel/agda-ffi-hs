{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Kind where

open import Agda.Primitive

private
    variable
        aℓ : Level
        A B : Set aℓ

postulate
    IsKind : (A : Set aℓ) → Set

{-# FOREIGN GHC {-# LANGUAGE KindSignatures #-} #-}
{-# FOREIGN GHC import qualified Data.Kind #-}
{-# FOREIGN GHC data AgdaIsKind aℓ (a :: Data.Kind.Type) = AgdaIsKind #-}
{-# COMPILE GHC IsKind = type(0) AgdaIsKind  #-}

infixr 0.10 _⟶_
infixr 0.05 OfKind-syntax OfKind-syntax-0ℓ

OfKind-syntax : ∀ aℓ {kℓ rℓ} → (Set aℓ → Set kℓ) → (Set aℓ → Set rℓ) → Set (lsuc aℓ ⊔ kℓ ⊔ rℓ)
OfKind-syntax aℓ K R = {A : Set aℓ} → ⦃ K A ⦄ → R A

OfKind-syntax-0ℓ = OfKind-syntax lzero

syntax OfKind-syntax aℓ K (λ A → r) = A :: K ^ aℓ ∙ r
syntax OfKind-syntax-0ℓ K (λ A → r) = A :: K ∙ r

_⟶_ : ∀{aℓ bℓ} → Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)
A ⟶ B = A → B

-- const : ∀{aℓ bℓ} → A :: Type ^ aℓ ∙ B :: Type ^ bℓ ∙ A ⟶ B ⟶ A

postulate
    IsKind[Set] : ∀{aℓ} → IsKind (Set (lsuc aℓ))
    IsKind[A⟶B] : ⦃ IsKind A ⦄ → ⦃ IsKind B ⦄ → IsKind (A → B)

{-# COMPILE GHC IsKind[Set] = \ aℓ                              -> AgdaIsKind #-}
{-# COMPILE GHC IsKind[A⟶B] = \ aℓ a bℓ b AgdaIsKind AgdaIsKind -> AgdaIsKind #-}
