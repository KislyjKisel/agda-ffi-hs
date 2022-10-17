{-# OPTIONS --without-K #-}

module Ffi.Hs.Type.Reflection.Unsafe where

open import Agda.Builtin.Char           using (Char)
open import Agda.Builtin.List           using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class          using (Show)
open import Ffi.Hs.Data.Int             using (Int)
open import Ffi.Hs.GHC.Exts             using (RuntimeRep; Addr#)
open import Ffi.Hs.GHC.Fingerprint.Type using (Fingerprint)
open import Ffi.Hs.Type.Reflection      using (SomeTypeRep; TyCon)

open Ffi.Hs.Type.Reflection public
    using (TypeRep)

{-# FOREIGN GHC
import qualified Type.Reflection.Unsafe
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ

data TypeLitSort : Set where
    TypeLitSymbol : TypeLitSort
    TypeLitNat    : TypeLitSort
    TypeLitChar   : TypeLitSort

{-# COMPILE GHC TypeLitSort = data Type.Reflection.Unsafe.TypeLitSort
    ( Type.Reflection.Unsafe.TypeLitSymbol
    | Type.Reflection.Unsafe.TypeLitNat
    | Type.Reflection.Unsafe.TypeLitChar
    ) #-}

KindBndr : Set
KindBndr = Int

data KindRep : Set where
    KindRepTyConApp : TyCon → List KindRep → KindRep
    KindRepVar      : KindBndr → KindRep
    KindRepApp      : KindRep → KindRep → KindRep
    KindRepFun      : KindRep → KindRep → KindRep
    KindRepTYPE     : RuntimeRep → KindRep
    KindRepTypeLitS : TypeLitSort → Addr# → KindRep
    KindRepTypeLitD : TypeLitSort → List Char → KindRep

{-# COMPILE GHC KindRep = data Type.Reflection.Unsafe.KindRep
    ( Type.Reflection.Unsafe.KindRepTyConApp
    | Type.Reflection.Unsafe.KindRepVar
    | Type.Reflection.Unsafe.KindRepApp
    | Type.Reflection.Unsafe.KindRepFun
    | Type.Reflection.Unsafe.KindRepTYPE
    | Type.Reflection.Unsafe.KindRepTypeLitS
    | Type.Reflection.Unsafe.KindRepTypeLitD
    ) #-}

postulate
    Show[TypeLitSort] : Show TypeLitSort
    Show[KindRep] : Show KindRep

{-# COMPILE GHC Show[TypeLitSort] = AgdaShow #-}
{-# COMPILE GHC Show[KindRep]     = AgdaShow #-}

postulate
    mkTrApp                : {K₁ : Set (lsuc aℓ)} {K₂ : Set (lsuc bℓ)} {A : K₁ → K₂} {B : K₁} → TypeRep A → TypeRep B → TypeRep (A B)
    mkTyCon                : List Char → List Char → List Char → Int → KindRep → TyCon
    typeRepFingerprint     : TypeRep A → Fingerprint
    someTypeRepFingerprint : SomeTypeRep {aℓ} → Fingerprint

    mkTrCon          : {K : Set (lsuc aℓ)} {A : K} → TyCon → List (SomeTypeRep {bℓ}) → TypeRep A
    tyConKindRep     : TyCon → KindRep
    tyConKindArgs    : TyCon → Int
    tyConFingerprint : TyCon → Fingerprint

{-# COMPILE GHC mkTrApp                = \ aℓ k1 bℓ k2 a b -> Type.Reflection.Unsafe.mkTrApp                #-}
{-# COMPILE GHC mkTyCon                =                      Type.Reflection.Unsafe.mkTyCon                #-}
{-# COMPILE GHC typeRepFingerprint     = \ aℓ a            -> Type.Reflection.Unsafe.typeRepFingerprint     #-}
{-# COMPILE GHC someTypeRepFingerprint = \ aℓ              -> Type.Reflection.Unsafe.someTypeRepFingerprint #-}

{-# COMPILE GHC mkTrCon          = \ aℓ k a bℓ -> Type.Reflection.Unsafe.mkTrCon          #-}
{-# COMPILE GHC tyConKindRep     =                Type.Reflection.Unsafe.tyConKindRep     #-}
{-# COMPILE GHC tyConKindArgs    =                Type.Reflection.Unsafe.tyConKindArgs    #-}
{-# COMPILE GHC tyConFingerprint =                Type.Reflection.Unsafe.tyConFingerprint #-}
