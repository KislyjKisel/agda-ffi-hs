{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Foldable where

open import Agda.Builtin.Bool          using (Bool)
open import Agda.Builtin.List          using (List)
open import Agda.Builtin.Maybe         using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Num  using (Num)
open import Ffi.Hs.-base.Unit using (⊤)
open import Ffi.Hs.Control.Applicative using (Applicative; Alternative)
open import Ffi.Hs.Control.Monad       using (Monad; MonadPlus)
open import Ffi.Hs.Data.Eq             using (Eq)
open import Ffi.Hs.Data.Int            using (Int)
open import Ffi.Hs.Data.Monoid         using (Monoid)
open import Ffi.Hs.Data.Ord            using (Ord; Ordering)

private
    variable
        aℓ bℓ fℓ : Level
        A : Set aℓ
        B : Set bℓ
        F M : Set fℓ → Set fℓ

postulate
    Foldable   : ∀{ℓ} → (Set ℓ → Set ℓ) → Set ℓ
    fold       : ⦃ Foldable F ⦄ → ⦃ Monoid A ⦄ → F A → A
    foldMap    : ⦃ Foldable F ⦄ → ⦃ Monoid B ⦄ → (A → B) → F A → B
    foldMap'   : ⦃ Foldable F ⦄ → ⦃ Monoid B ⦄ → (A → B) → F A → B
    foldl      : ⦃ Foldable F ⦄ → (B → A → B) → B → F A → B
    foldl'     : ⦃ Foldable F ⦄ → (B → A → B) → B → F A → B
    foldl1     : ⦃ Foldable F ⦄ → (A → A → A) → F A → A
    foldl1'    : ⦃ Foldable F ⦄ → (A → A → A) → F A → A
    foldr      : ⦃ Foldable F ⦄ → (A → B → B) → B → F A → B
    foldr'     : ⦃ Foldable F ⦄ → (A → B → B) → B → F A → B
    foldr1     : ⦃ Foldable F ⦄ → (A → A → A) → F A → A
    toList     : ⦃ Foldable F ⦄ → F A → List A
    null       : ⦃ Foldable F ⦄ → F A → Bool
    length     : ⦃ Foldable F ⦄ → F A → Int
    elem       : ⦃ Foldable F ⦄ → ⦃ Eq A ⦄ → A → F A → Bool
    maximum    : ⦃ Foldable F ⦄ → ⦃ Ord A ⦄ → F A → A
    minimum    : ⦃ Foldable F ⦄ → ⦃ Ord A ⦄ → F A → A
    sum        : ⦃ Foldable F ⦄ → ⦃ Num A ⦄ → F A → A
    product    : ⦃ Foldable F ⦄ → ⦃ Num A ⦄ → F A → A
    foldrM     : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → (A → B → M B) → B → F A → M B
    foldlM     : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → (B → A → M B) → B → F A → M B
    traverse_  : ⦃ Foldable F ⦄ → ⦃ Applicative M ⦄ → (A → M B) → F A → M ⊤
    for_       : ⦃ Foldable F ⦄ → ⦃ Applicative M ⦄ → F A → (A → M B) → M ⊤
    sequenceA_ : ⦃ Foldable F ⦄ → ⦃ Applicative M ⦄ → F (M A) → M ⊤
    asum       : ⦃ Foldable F ⦄ → ⦃ Alternative M ⦄ → F (M A) → M A
    mapM_      : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → (A → M B) → F A → M ⊤
    forM_      : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → F A → (A → M B) → M ⊤
    sequence_  : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → F (M A) → M ⊤
    msum       : ⦃ Foldable F ⦄ → ⦃ MonadPlus M ⦄ → F (M A) → M A
    concat     : ⦃ Foldable F ⦄ → F (List A) → List A
    concatMap  : ⦃ Foldable F ⦄ → (A → List B) → F A → List B
    and        : ⦃ Foldable F ⦄ → F Bool → Bool 
    or         : ⦃ Foldable F ⦄ → F Bool → Bool
    any        : ⦃ Foldable F ⦄ → (A → Bool) → F A → Bool
    all        : ⦃ Foldable F ⦄ → (A → Bool) → F A → Bool
    maximumBy  : ⦃ Foldable F ⦄ → (A → A → Ordering) → F A → A
    minimumBy  : ⦃ Foldable F ⦄ → (A → A → Ordering) → F A → A
    notElem    : ⦃ Foldable F ⦄ → ⦃ Eq A ⦄ → A → F A → Bool
    find       : ⦃ Foldable F ⦄ → (A → Bool) → F A → Maybe A

{-# COMPILE GHC asum     = \ ℓ t f a     AgdaFoldable AgdaAlternative -> Control.Applicative.asum     #-}