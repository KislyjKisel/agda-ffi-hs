{-# OPTIONS --without-K #-}

module Ffi.Hs.Type.Reflection where

open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤)
open import Ffi.Hs.-base.Kind using (IsKind)
open import Ffi.Hs.Data.Tuple using (Tuple2)

private
    variable
        aℓ bℓ kℓ : Level
        A : Set aℓ
        B : Set bℓ
        K : Set kℓ

infix 4 _:~:_

data _:~:_ (A : Set aℓ) : Set aℓ → Set (lsuc aℓ) where
    Refl : A :~: A

postulate
    TypeRep : ∀{K : Set (lsuc kℓ)} → ⦃ IsKind K ⦄ → K → Set

data SomeTypeRep {kℓ} : Set (lsuc (lsuc kℓ)) where
    mkSomeTypeRep : {K : Set (lsuc kℓ)} ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → SomeTypeRep

module _ {K₁ : Set (lsuc aℓ)} ⦃ _ : IsKind K₁ ⦄ where
    
    infix 4 _:~~:_

    data _:~~:_ (A : K₁) : {K₂ : Set (lsuc bℓ)} ⦃ _ : IsKind K₂ ⦄ → K₂ → Set ? where
        HRefl : _:~~:_ A {K₂ = K₁} A
    
    postulate
        eqTypeRep : {A : K₁}  {K₂ : Set (lsuc bℓ)} ⦃ _ : IsKind K₂ ⦄ {B : K₂} → TypeRep A → TypeRep B → Maybe (A :~~: B)

postulate
    TyCon : Set
    tyConPackage : TyCon → List Char
    tyConModule : TyCon → List Char
    tyConName : TyCon → List Char
    rnfTyCon : TyCon → ⊤ {lzero}

postulate
    Typeable     : ∀{K : Set (lsuc kℓ)} → ⦃ IsKind K ⦄ → K → Set aℓ
    typeRep      : ⦃ _ : IsKind K ⦄ {A : K} → ⦃ Typeable A ⦄ → TypeRep A
    withTypeable : ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → (⦃ Typeable A ⦄ → B) → B

    typeRepTyCon : ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → TyCon
    rnfTypeRep   : ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → ⊤ {lzero}
    typeRepKind  : ∀{A : K} → ⦃ _ : IsKind K ⦄ → TypeRep A → TypeRep {K = ?} ? 
    splitApps    : TypeRep A → Tuple2 TyCon (List {lsuc (lsuc kℓ)} SomeTypeRep)

    someTypeRep      : ∀{proxy : Set aℓ → Set bℓ} → ⦃ Typeable A ⦄ → proxy A → SomeTypeRep {kℓ}
    someTypeRepTyCon : SomeTypeRep {kℓ} → TyCon
    rnfSomeTypeRep   : SomeTypeRep {kℓ} → ⊤ {lzero}

typeOf : ⦃ Typeable A ⦄ → A → TypeRep A
typeOf _ = typeRep

postulate
    Module : Set
    moduleName    : Module → List Char
    modulePackage : Module → List Char
    rnfModule     : Module → ⊤ {lzero}

-- todo: patterns

module Instances where
    postulate
        X : Set
