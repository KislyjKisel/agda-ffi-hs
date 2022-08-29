{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Monoid where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive

private
    variable
        aℓ fℓ : Level
        A : Set aℓ
        F : Set fℓ → Set fℓ

postulate
    Monoid  : Set aℓ → Set aℓ
    mempty  : ⦃ Monoid A ⦄ → A
    mconcat : ⦃ Monoid A ⦄ → List A → A
    
    Dual    : Set aℓ → Set aℓ
    mkDual  : A → Dual A
    getDual : Dual A → A

    Endo    : Set aℓ → Set aℓ
    mkEndo  : (A → A) → Endo A
    appEndo : Endo A → A → A

    All    : Set
    mkAll  : Bool → All
    getAll : All → Bool

    Any    : Set
    mkAny  : Bool → Any
    getAny : Any → Bool

    Sum    : Set aℓ → Set aℓ
    mkSum  : A → Sum A
    getSum : Sum A → A

    Product    : Set aℓ → Set aℓ
    mkProduct  : A → Product A
    getProduct : Product A → A

    First    : Set aℓ → Set aℓ
    mkFirst  : Maybe A → First A
    getFirst : First A → Maybe A

    Last    : Set aℓ → Set aℓ
    mkLast  : Maybe A → Last A
    getLast : Last A → Maybe A

    Alt    : (Set aℓ → Set aℓ) → Set aℓ → Set aℓ
    mkAlt  : F A → Alt F A
    getAlt : Alt F A → F A

    Ap    : (Set aℓ → Set aℓ) → Set aℓ → Set aℓ
    mkAp  : F A → Ap F A
    getAp : Ap F A → F A
