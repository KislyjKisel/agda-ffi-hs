{-# OPTIONS --without-K #-}

module Ffi.Hs.Type.Reflection where

open import Agda.Builtin.Char         using (Char)
open import Agda.Builtin.List         using (List)
open import Agda.Builtin.Maybe        using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class        using (Show; Eq; Ord; TestEquality)
open import Ffi.Hs.-base.Unit         using (⊤; ⊤′)
open import Ffi.Hs.Data.Tuple         using (Tuple2)
open import Ffi.Hs.Data.Type.Equality using (_:~~:_)

open Ffi.Hs.-base.Class public
    using (Typeable)

{-# FOREIGN GHC
import qualified Type.Reflection
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ kℓ : Level
        A : Set aℓ
        B : Set bℓ
        K : Set kℓ

postulate
    TypeRep : {K : Set (lsuc kℓ)} → K → Set

    TestEquality[TypeRep] : TestEquality {aℓ} TypeRep
    Show[TypeRep[A]]      : {K : Set (lsuc kℓ)} {A : K} → Show (TypeRep A)
    Eq[TypeRep[A]]        : {K : Set (lsuc kℓ)} {A : K} → Eq (TypeRep A)
    Ord[TypeRep[A]]       : {K : Set (lsuc kℓ)} {A : K} → Ord (TypeRep A)

{-# FOREIGN GHC
type AgdaTypeRep kℓ k = Type.Reflection.TypeRep
#-}
{-# COMPILE GHC TypeRep = type(2) AgdaTypeRep #-}

{-# COMPILE GHC TestEquality[TypeRep] = \ aℓ     -> AgdaTestEquality #-}
{-# COMPILE GHC Show[TypeRep[A]]      = \ kℓ k a -> AgdaShow         #-}
{-# COMPILE GHC Eq[TypeRep[A]]        = \ kℓ k a -> AgdaEq           #-}
{-# COMPILE GHC Ord[TypeRep[A]]       = \ kℓ k a -> AgdaOrd          #-}

-- todo: separate versions for types and kinds? (can't mix?: Sets are translated as units)
-- mb types work better as sets, kinds - with ⦃IsKind⦄ (iskindortype?)

-- only for types - otherwise ∀K→(A : K)→... - A is not erased (K mb ≠ Set)
data SomeTypeRep {kℓ} : Set (lsuc kℓ) where
    mkSomeTypeRep : {A : Set kℓ} → TypeRep A → SomeTypeRep

{-# FOREIGN GHC
type AgdaSomeTypeRep kℓ = Type.Reflection.SomeTypeRep
#-}
{-# COMPILE GHC SomeTypeRep = data(1) AgdaSomeTypeRep (Type.Reflection.SomeTypeRep) #-}

postulate
    Show[SomeTypeRep] : Show (SomeTypeRep {aℓ})
    Eq[SomeTypeRep]   : Eq (SomeTypeRep {aℓ})
    Ord[SomeTypeRep]  : Ord (SomeTypeRep {aℓ})

{-# COMPILE GHC Show[SomeTypeRep] = \ aℓ -> AgdaShow #-}
{-# COMPILE GHC Eq[SomeTypeRep]   = \ aℓ -> AgdaEq   #-}
{-# COMPILE GHC Ord[SomeTypeRep]  = \ aℓ -> AgdaOrd  #-}

postulate
    TyCon : Set
    tyConPackage : TyCon → List Char
    tyConModule  : TyCon → List Char
    tyConName    : TyCon → List Char
    rnfTyCon     : TyCon → ⊤′ {lzero}

    Show[TyCon] : Show TyCon
    Eq[TyCon]   : Eq TyCon
    Ord[TyCon]  : Ord TyCon

{-# COMPILE GHC TyCon = type Type.Reflection.TyCon #-}
{-# COMPILE GHC tyConPackage = Type.Reflection.tyConPackage #-}
{-# COMPILE GHC tyConModule  = Type.Reflection.tyConModule  #-}
{-# COMPILE GHC tyConName    = Type.Reflection.tyConName    #-}
{-# COMPILE GHC rnfTyCon     = Type.Reflection.rnfTyCon     #-}

{-# COMPILE GHC Show[TyCon] = AgdaShow #-}
{-# COMPILE GHC Eq[TyCon]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TyCon]  = AgdaOrd  #-}

postulate
    typeRep      : {A : K} → ⦃ Typeable A ⦄ → TypeRep A
    withTypeable : {A : K} → TypeRep A → (⦃ Typeable A ⦄ → B) → B

    typeRepTyCon : {A : K} → TypeRep A → TyCon
    rnfTypeRep   : {A : K} → TypeRep A → ⊤′ {lzero}
    -- todo: (req kinds) eqTypeRep    : {K₁ K₂ : Set (lsuc aℓ)} {A : K₁} {B : K₂} → TypeRep A → TypeRep B → Maybe (A :~~: B)
    -- todo: (A : K -> K is translated as xA :: k -> xK) typeRepKind : {A : K} → TypeRep A → TypeRep K
    splitApps    : {A : K} → TypeRep A → Tuple2 TyCon (List {lsuc (lsuc bℓ)} SomeTypeRep)

    someTypeRep      : {K : Set (lsuc kℓ)} {A : K} {proxy : K → Set bℓ} → ⦃ Typeable A ⦄ → proxy A → SomeTypeRep {kℓ}
    someTypeRepTyCon : SomeTypeRep {kℓ} → TyCon
    rnfSomeTypeRep   : SomeTypeRep {kℓ} → ⊤′ {lzero}

{-# COMPILE GHC typeRep      = \ kℓ k a AgdaTypeable -> Type.Reflection.typeRep #-}
{-# COMPILE GHC withTypeable = \ kℓ k bℓ b a x f -> Type.Reflection.withTypeable x (f AgdaTypeable) #-}

{-# COMPILE GHC typeRepTyCon = \ kℓ k a       -> Type.Reflection.typeRepTyCon #-}
{-# COMPILE GHC rnfTypeRep   = \ kℓ k a       -> Type.Reflection.rnfTypeRep   #-}
-- {-# COMPILE GHC eqTypeRep    = \ aℓ k1 k2 a b -> Type.Reflection.eqTypeRep    #-}
-- {-# COMPILE GHC typeRepKind  = \ kℓ k a       -> Type.Reflection.typeRepKind  #-}
{-# COMPILE GHC splitApps    = \ kℓ k a bℓ    -> Type.Reflection.splitApps    #-}

{-# COMPILE GHC someTypeRep      = \ kℓ k a bℓ proxy AgdaTypeable -> Type.Reflection.someTypeRep      #-}
{-# COMPILE GHC someTypeRepTyCon = \ kℓ                           -> Type.Reflection.someTypeRepTyCon #-}
{-# COMPILE GHC rnfSomeTypeRep   = \ kℓ                           -> Type.Reflection.rnfSomeTypeRep   #-}

typeOf : ⦃ Typeable A ⦄ → A → TypeRep A
typeOf _ = typeRep

postulate
    Module : Set
    moduleName    : Module → List Char
    modulePackage : Module → List Char
    rnfModule     : Module → ⊤′ {lzero}

    Show[Module] : Show Module
    Eq[Module]   : Eq Module

{-# COMPILE GHC Module        = type Type.Reflection.Module        #-}
{-# COMPILE GHC moduleName    =      Type.Reflection.moduleName    #-}
{-# COMPILE GHC modulePackage =      Type.Reflection.modulePackage #-}
{-# COMPILE GHC rnfModule     =      Type.Reflection.rnfModule     #-}

{-# COMPILE GHC Show[Module] = AgdaShow #-}
{-# COMPILE GHC Eq[Module]   = AgdaEq   #-}
