{-# OPTIONS --without-K #-}

module Ffi.Hs.Unsafe.Coerce where

open import Ffi.Hs.-base.Kind using (OfKind-syntax; _⟶_)
open import Ffi.Hs.GHC.Exts   using (LiftedType; UnliftedType)

{-# FOREIGN GHC
import qualified Unsafe.Coerce
import MAlonzo.Code.Ffi.Hs.GHC.Exts (AgdaTYPE)
#-}

postulate
    unsafeCoerce   : ∀{aℓ bℓ} {A : Set aℓ} {B : Set bℓ} → A → B
    unsafeCoerce-u : ∀{aℓ bℓ} → A :: UnliftedType ^ aℓ ∙ B :: UnliftedType ^ bℓ ∙ A ⟶ B

{-# COMPILE GHC unsafeCoerce   = \ aℓ bℓ a b                   -> Unsafe.Coerce.unsafeCoerce #-}
{-# COMPILE GHC unsafeCoerce-u = \ aℓ bℓ a AgdaTYPE b AgdaTYPE -> Unsafe.Coerce.unsafeCoerce #-}
