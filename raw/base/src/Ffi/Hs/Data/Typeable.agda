{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Typeable where

open import Agda.Builtin.List using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Kind using (IsKind[Set])
-- open import Ffi.Hs.-base.Class using ()
open import Ffi.Hs.Text.Show using (ShowS)
open import Ffi.Hs.Type.Reflection as Reflect using ()
open import Ffi.Hs.Data.Proxy using (Proxy; mkProxy)
open import Ffi.Hs.Data.Type.Equality using (_:~:_)
open import Ffi.Hs.Data.Tuple using (Tuple2)
open import Ffi.Hs.-base.Fingerprint using (Fingerprint)

open Reflect public
    using (Typeable; TyCon; tyConPackage; tyConModule; tyConName; rnfTyCon)
    renaming (SomeTypeRep to TypeRep; rnfSomeTypeRep to rnfTypeRep; someTypeRep to typeRep)

{-# FOREIGN GHC
import qualified Data.Typeable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class ()
#-}

private
    variable
        aℓ bℓ cℓ : Level
        A B C : Set aℓ
        F : Set aℓ → Set bℓ

postulate
    cast  : ⦃ Typeable A ⦄ → ⦃ Typeable B ⦄ → A → Maybe B
    eqT   : ⦃ Typeable A ⦄ → ⦃ Typeable B ⦄ → Maybe (A :~: B)
    gcast : ⦃ Typeable A ⦄ → ⦃ Typeable B ⦄ → F A → Maybe (F B)

    gcast1 : {T₁ T₂ : Set aℓ → Set bℓ} → ⦃ Typeable T₁ ⦄ → ⦃ Typeable T₂ ⦄ → F (T₁ A) → Maybe (F (T₂ A))
    gcast2 : {T₁ T₂ : Set aℓ → Set bℓ → Set cℓ} → ⦃ Typeable T₁ ⦄ → ⦃ Typeable T₂ ⦄ → F (T₁ A B) → Maybe (F (T₂ A B))

    showsTypeRep       : TypeRep {aℓ} → ShowS
    mkFunTy            : TypeRep {aℓ} → TypeRep {bℓ} → TypeRep {aℓ ⊔ bℓ}
    funResultTy        : TypeRep {aℓ ⊔ bℓ} → TypeRep {aℓ} → Maybe (TypeRep {bℓ})
    splitTyConApp      : TypeRep {aℓ} → (Tuple2 TyCon (List (TypeRep {aℓ})))
    typeRepArgs        : TypeRep {aℓ} → List (TypeRep {aℓ})
    typeRepTyCon       : TypeRep {aℓ} → TyCon
    typeRepFingerprint : TypeRep {aℓ} → Fingerprint
    tyConFingerprint   : TyCon → Fingerprint

{-# COMPILE GHC  =  #-}

typeOf :  ⦃ Typeable {lsuc aℓ} A ⦄ → A → TypeRep {lsuc aℓ}
typeOf {A = A} _ = Reflect.someTypeRep {proxy = Proxy} (mkProxy {A = A})


