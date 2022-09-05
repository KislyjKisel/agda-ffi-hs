{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Typeable where

open import Agda.Primitive
open import Ffi.Hs.-base.Kind using (IsKind[Set])
-- open import Ffi.Hs.-base.Class using ()
open import Ffi.Hs.Text.Show using (ShowS)
open import Ffi.Hs.Type.Reflection as Reflect using ()
open import Ffi.Hs.Data.Proxy using (Proxy; mkProxy)

open Reflect public
    using (Typeable)
    renaming (SomeTypeRep to TypeRep; rnfSomeTypeRep to rnfTypeRep; someTypeRep to typeRep)

{-# FOREIGN GHC
import qualified Data.Typeable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class ()
#-}

private
    variable
        aℓ bℓ kℓ : Level
        A : Set aℓ
        B : Set bℓ
        K : Set kℓ

    instance _ = IsKind[Set]

postulate
    cast : ⦃ Typeable A ⦄ → ⦃ Typeable B ⦄ → A → Maybe B
    

    showsTypeRep : TypeRep {kℓ} → ShowS
    mkFunTy : TypeRep {kℓ} → TypeRep {kℓ} → TypeRep {kℓ}

typeOf :  ⦃ Typeable {lsuc aℓ} A ⦄ → A → TypeRep {lsuc aℓ}
typeOf {A = A} _ = Reflect.someTypeRep {proxy = Proxy} (mkProxy {A = A})
