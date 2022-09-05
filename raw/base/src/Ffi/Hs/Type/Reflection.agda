{-# OPTIONS --without-K #-}

module Ffi.Hs.Type.Reflection where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using ()
open import Ffi.Hs.-base.Kind  using (IsKind; IsKind[Set])
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Tuple  using (Tuple2)
open import Ffi.Hs.Data.Type.Equality using (_:~~:_)

{-# FOREIGN GHC
import qualified Type.Reflection

#-}

private
    variable
        aℓ bℓ kℓ : Level
        A : Set aℓ
        B : Set bℓ
        K : Set kℓ

    instance
        _ = IsKind[Set]


postulate
    TypeRep : ∀{K : Set (lsuc kℓ)} → ⦃ IsKind K ⦄ → K → Set

{-# FOREIGN GHC
type AgdaTypeRep kℓ k isk = Type.Reflection.TypeRep
#-}
{-# COMPILE GHC TypeRep = type(3) AgdaTypeRep #-}

data SomeTypeRep {kℓ} : Set (lsuc (lsuc kℓ)) where
    mkSomeTypeRep : {K : Set (lsuc kℓ)} ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → SomeTypeRep

{-# FOREIGN GHC
type AgdaSomeTypeRep kℓ = Type.Reflection.SomeTypeRep
#-}
{-# COMPILE GHC SomeTypeRep = data(1) AgdaSomeTypeRep (Type.Reflection.SomeTypeRep) #-}

postulate
    eqTypeRep : {K₁ K₂ : Set (lsuc aℓ)} ⦃ _ : IsKind K₁ ⦄ ⦃ _ : IsKind K₂ ⦄ {A : K₁} {B : K₂} → TypeRep A → TypeRep B → Maybe (A :~~: B)

{-# COMPILE GHC eqTypeRep = \ aℓ k1 k2 AgdaIsKind AgdaIsKind a b -> Type.Reflection.eqTypeRep #-}

postulate
    TyCon : Set
    tyConPackage : TyCon → List Char
    tyConModule  : TyCon → List Char
    tyConName    : TyCon → List Char
    rnfTyCon     : TyCon → ⊤ {lzero}

{-# COMPILE GHC TyCon = Type.Reflection.TyCon #-}
{-# COMPILE GHC tyConPackage = Type.Reflection.tyConPackage #-}
{-# COMPILE GHC tyConModule  = Type.Reflection.tyConModule  #-}
{-# COMPILE GHC tyConName    = Type.Reflection.tyConName    #-}
{-# COMPILE GHC rnfTyCon     = Type.Reflection.rnfTyCon     #-}

postulate
    Typeable     : ∀{K : Set (lsuc kℓ)} → ⦃ IsKind K ⦄ → K → Set kℓ
    typeRep      : ⦃ _ : IsKind K ⦄ {A : K} → ⦃ Typeable A ⦄ → TypeRep A
    withTypeable : ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → (⦃ Typeable A ⦄ → B) → B

    typeRepTyCon : ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → TyCon
    rnfTypeRep   : ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → ⊤ {lzero}
    typeRepKind  : ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → TypeRep K 
    splitApps    : ⦃ _ : IsKind K ⦄ {A : K} → TypeRep A → Tuple2 TyCon (List {lsuc (lsuc bℓ)} SomeTypeRep)

    someTypeRep      : ⦃ _ : IsKind K ⦄ {A : K} {proxy : K → Set bℓ} → ⦃ Typeable A ⦄ → proxy A → SomeTypeRep {kℓ}
    someTypeRepTyCon : SomeTypeRep {kℓ} → TyCon
    rnfSomeTypeRep   : SomeTypeRep {kℓ} → ⊤ {lzero}

{-# FOREIGN GHC
data AgdaTypeable kℓ k isk (a :: k) = Type.Reflection.Typeable a => AgdaTypeable
#-}
{-# COMPILE GHC Typeable = type(3) AgdaTypeable #-}

{-# COMPILE GHC typeRep      = \ kℓ k AgdaIsKind a AgdaTypeable -> Type.Reflection.typeRep #-}
{-# COMPILE GHC withTypeable = \ kℓ k AgdaIsKind a x f -> Type.Reflection.withTypeable x (f AgdaTypeable) #-}

{-# COMPILE GHC typeRepTyCon = \ kℓ k AgdaIsKind a    -> Type.Reflection.typeRepTyCon #-}
{-# COMPILE GHC rnfTypeRep   = \ kℓ k AgdaIsKind a    -> Type.Reflection.rnfTypeRep   #-}
{-# COMPILE GHC typeRepKind  = \ kℓ k AgdaIsKind a    -> Type.Reflection.typeRepKind  #-}
{-# COMPILE GHC splitApps    = \ kℓ k AgdaIsKind a bℓ -> Type.Reflection.splitApps    #-}

{-# COMPILE GHC someTypeRep      = \ kℓ k AgdaIsKind a proxy AgdaTypeable -> Type.Reflection.someTypeRep      #-}
{-# COMPILE GHC someTypeRepTyCon = \ kℓ                                   -> Type.Reflection.someTypeRepTyCon #-}
{-# COMPILE GHC rnfSomeTypeRep   = \ kℓ                                   -> Type.Reflection.rnfSomeTypeRep   #-}

typeOf : ⦃ Typeable A ⦄ → A → TypeRep ⦃ IsKind[Set] ⦄ A
typeOf _ = typeRep

postulate
    Module : Set
    moduleName    : Module → List Char
    modulePackage : Module → List Char
    rnfModule     : Module → ⊤ {lzero}

{-# COMPILE GHC Module = type Type.Reflection.Module #-}
{-# COMPILE GHC moduleName    = Type.Reflection.moduleName    #-}
{-# COMPILE GHC modulePackage = Type.Reflection.modulePackage #-}
{-# COMPILE GHC rnfModule     = Type.Reflection.rnfModule     #-}

-- todo: classes ~ and ~~
-- todo: patterns?

-- module Instances where
--     postulate

-- todo: instances for :~: and :~~:

-- open Instances public
