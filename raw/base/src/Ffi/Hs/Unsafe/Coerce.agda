{-# OPTIONS --without-K #-}

module Ffi.Hs.Unsafe.Coerce where

open import Ffi.Hs.-base.Kind using (OfKind-syntax; _⟶_)
open import Ffi.Hs.GHC.Exts   using (LiftedType; UnliftedType)

{-# FOREIGN GHC
import qualified Unsafe.Coerce
import MAlonzo.Code.Ffi.Hs.GHC.Exts (AgdaTYPE)
#-}

postulate
    unsafeCoerce-lifted   : ∀{aℓ bℓ} → A :: LiftedType   ^ aℓ ∙ B :: LiftedType   ^ bℓ ∙ A ⟶ B
    unsafeCoerce-unlifted : ∀{aℓ bℓ} → A :: UnliftedType ^ aℓ ∙ B :: UnliftedType ^ bℓ ∙ A ⟶ B

{-# COMPILE GHC unsafeCoerce-lifted   = \ aℓ bℓ a AgdaTYPE b AgdaTYPE -> Unsafe.Coerce.unsafeCoerce #-}
{-# COMPILE GHC unsafeCoerce-unlifted = \ aℓ bℓ a AgdaTYPE b AgdaTYPE -> Unsafe.Coerce.unsafeCoerce #-}
