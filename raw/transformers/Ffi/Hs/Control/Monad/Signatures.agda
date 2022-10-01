{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Signatures where

open import Agda.Primitive
open import Ffi.Hs.-base.Level using (Liftℓ)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

CallCC : ∀{mℓ} (M : Set mℓ → Set mℓ) (A B : Set mℓ) → Set mℓ
CallCC M A B = ((A → M B) → M A) → M A

Catch : ∀{eℓ aℓ} (E : Set eℓ) (M : Set (eℓ ⊔ aℓ) → Set (eℓ ⊔ aℓ)) (A : Set aℓ) → Set (eℓ ⊔ aℓ)
Catch {eℓ} {aℓ} E M A = M (Liftℓ (eℓ ⊔ aℓ) A) → (E → M (Liftℓ (eℓ ⊔ aℓ) A)) → M (Liftℓ (eℓ ⊔ aℓ) A)

Listen : ∀{wℓ aℓ} (W : Set wℓ) (M : Set (wℓ ⊔ aℓ) → Set (wℓ ⊔ aℓ)) (A : Set aℓ) → Set (wℓ ⊔ aℓ)
Listen {wℓ} {aℓ} W M A = M (Liftℓ (wℓ ⊔ aℓ) A) → M (Tuple2 A W)

Pass : ∀{wℓ aℓ} (W : Set wℓ) (M : Set (wℓ ⊔ aℓ) → Set (wℓ ⊔ aℓ)) (A : Set aℓ) → Set (wℓ ⊔ aℓ)
Pass {wℓ} {aℓ} W M A = M (Tuple2 A (W → W)) → M (Liftℓ (wℓ ⊔ aℓ) A)
