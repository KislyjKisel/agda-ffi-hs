{-# OPTIONS --without-K #-}

module Ffi.Hs.Type.Reflection where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Tuple  using (Tuple2)
open import Ffi.Hs.Data.Type.Equality using (_:~~:_)

open import Ffi.Hs.-base.Class public
    using (Typeable)

{-# FOREIGN GHC
import qualified Type.Reflection
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaTypeable)
#-}

private
    variable
        aℓ bℓ kℓ : Level
        A : Set aℓ
        B : Set bℓ
        K : Set kℓ

postulate
    TypeRep : ∀{K : Set (lsuc kℓ)} → K → Set

{-# FOREIGN GHC
type AgdaTypeRep kℓ k = Type.Reflection.TypeRep
#-}
{-# COMPILE GHC TypeRep = type(2) AgdaTypeRep #-}

data SomeTypeRep {kℓ} : Set (lsuc (lsuc kℓ)) where
    mkSomeTypeRep : {K : Set (lsuc kℓ)} {A : K} → TypeRep A → SomeTypeRep

{-# FOREIGN GHC
type AgdaSomeTypeRep kℓ = Type.Reflection.SomeTypeRep
#-}
{-# COMPILE GHC SomeTypeRep = data(1) AgdaSomeTypeRep (Type.Reflection.SomeTypeRep) #-}

postulate
    eqTypeRep : {K₁ K₂ : Set (lsuc aℓ)} {A : K₁} {B : K₂} → TypeRep A → TypeRep B → Maybe (A :~~: B)

{-# COMPILE GHC eqTypeRep = \ aℓ k1 k2 a b -> Type.Reflection.eqTypeRep #-}

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
    typeRep      : {A : K} → ⦃ Typeable A ⦄ → TypeRep A
    withTypeable : {A : K} → TypeRep A → (⦃ Typeable A ⦄ → B) → B

    typeRepTyCon : {A : K} → TypeRep A → TyCon
    rnfTypeRep   : {A : K} → TypeRep A → ⊤ {lzero}
    typeRepKind  : {A : K} → TypeRep A → TypeRep K 
    splitApps    : {A : K} → TypeRep A → Tuple2 TyCon (List {lsuc (lsuc bℓ)} SomeTypeRep)

    someTypeRep      : {A : K} {proxy : K → Set bℓ} → ⦃ Typeable A ⦄ → proxy A → SomeTypeRep {kℓ}
    someTypeRepTyCon : SomeTypeRep {kℓ} → TyCon
    rnfSomeTypeRep   : SomeTypeRep {kℓ} → ⊤ {lzero}

{-# COMPILE GHC typeRep      = \ kℓ k a AgdaTypeable -> Type.Reflection.typeRep #-}
{-# COMPILE GHC withTypeable = \ kℓ k a x f -> Type.Reflection.withTypeable x (f AgdaTypeable) #-}

{-# COMPILE GHC typeRepTyCon = \ kℓ k a    -> Type.Reflection.typeRepTyCon #-}
{-# COMPILE GHC rnfTypeRep   = \ kℓ k a    -> Type.Reflection.rnfTypeRep   #-}
{-# COMPILE GHC typeRepKind  = \ kℓ k a    -> Type.Reflection.typeRepKind  #-}
{-# COMPILE GHC splitApps    = \ kℓ k a bℓ -> Type.Reflection.splitApps    #-}

{-# COMPILE GHC someTypeRep      = \ kℓ k a proxy AgdaTypeable -> Type.Reflection.someTypeRep      #-}
{-# COMPILE GHC someTypeRepTyCon = \ kℓ                        -> Type.Reflection.someTypeRepTyCon #-}
{-# COMPILE GHC rnfSomeTypeRep   = \ kℓ                        -> Type.Reflection.rnfSomeTypeRep   #-}

typeOf : ⦃ Typeable A ⦄ → A → TypeRep A
typeOf _ = typeRep

postulate
    Module : Set
    moduleName    : Module → List Char
    modulePackage : Module → List Char
    rnfModule     : Module → ⊤ {lzero}

{-# COMPILE GHC Module        = type Type.Reflection.Module        #-}
{-# COMPILE GHC moduleName    =      Type.Reflection.moduleName    #-}
{-# COMPILE GHC modulePackage =      Type.Reflection.modulePackage #-}
{-# COMPILE GHC rnfModule     =      Type.Reflection.rnfModule     #-}

-- todo: patterns?
-- todo: instances
