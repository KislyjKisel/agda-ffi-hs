{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Typeable where

open import Agda.Builtin.List                 using (List)
open import Agda.Builtin.Maybe                using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.Data.Proxy                 using (Proxy; mkProxy)
open import Ffi.Hs.Data.Tuple                 using (Tuple2)
open import Ffi.Hs.GHC.Fingerprint.Type       using (Fingerprint)
open import Ffi.Hs.Text.Show                  using (ShowS)
open import Ffi.Hs.Type.Reflection            using ()

open Ffi.Hs.Type.Reflection public
    using
    ( Typeable ; TyCon ; tyConPackage
    ; tyConModule ; tyConName ; rnfTyCon
    )
    renaming
    ( SomeTypeRep to TypeRep ; rnfSomeTypeRep to rnfTypeRep
    ; someTypeRep to typeRep
    )

open import Ffi.Hs.Type.Reflection.Unsafe public
    using (tyConFingerprint)
    renaming
    ( someTypeRepFingerprint to typeRepFingerprint
    )

open import Ffi.Hs.Data.Type.Equality public
    using (_:~:_; Refl; _:~~:_; HRefl)

{-# FOREIGN GHC
import qualified Data.Typeable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
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

{-# COMPILE GHC cast  = \ aℓ a bℓ b   AgdaTypeable AgdaTypeable -> Data.Typeable.cast  #-}
{-# COMPILE GHC eqT   = \ aℓ a b      AgdaTypeable AgdaTypeable -> Data.Typeable.eqT   #-}
{-# COMPILE GHC gcast = \ aℓ a b fℓ f AgdaTypeable AgdaTypeable -> Data.Typeable.gcast #-}

{-# COMPILE GHC gcast1 = \ aℓ bℓ t1 t2 fℓ f    AgdaTypeable AgdaTypeable -> Data.Typeable.gcast1 #-}
{-# COMPILE GHC gcast2 = \ aℓ bℓ cℓ t1 t2 fℓ f AgdaTypeable AgdaTypeable -> Data.Typeable.gcast2 #-}

{-# COMPILE GHC showsTypeRep       = \ aℓ    -> Data.Typeable.showsTypeRep       #-}
{-# COMPILE GHC mkFunTy            = \ aℓ bℓ -> Data.Typeable.mkFunTy            #-}
{-# COMPILE GHC funResultTy        = \ aℓ bℓ -> Data.Typeable.funResultTy        #-}
{-# COMPILE GHC splitTyConApp      = \ aℓ    -> Data.Typeable.splitTyConApp      #-}
{-# COMPILE GHC typeRepArgs        = \ aℓ    -> Data.Typeable.typeRepArgs        #-}
{-# COMPILE GHC typeRepTyCon       = \ aℓ    -> Data.Typeable.typeRepTyCon       #-}

typeOf :  ⦃ Typeable {lsuc aℓ} A ⦄ → A → TypeRep {lsuc aℓ}
typeOf {A = A} _ = typeRep {proxy = Proxy} (mkProxy {A = A})
