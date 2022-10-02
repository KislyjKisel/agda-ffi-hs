{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Signatures where

open import Agda.Primitive
open import Ffi.Hs.Data.Tuple  using (Tuple2)

CallCC : ∀{mℓ} (M : Set mℓ → Set mℓ) (A B : Set mℓ) → Set mℓ
CallCC M A B = ((A → M B) → M A) → M A

Catch : ∀{eℓ} (E : Set eℓ) (M : Set eℓ → Set eℓ) (A : Set eℓ) → Set eℓ
Catch {eℓ} E M A = M A → (E → M A) → M A

Listen : ∀{wℓ} (W : Set wℓ) (M : Set wℓ → Set wℓ) (A : Set wℓ) → Set wℓ
Listen {wℓ} W M A = M A → M (Tuple2 A W)

Pass : ∀{wℓ} (W : Set wℓ) (M : Set wℓ → Set wℓ) (A : Set wℓ) → Set wℓ
Pass {wℓ} W M A = M (Tuple2 A (W → W)) → M A
