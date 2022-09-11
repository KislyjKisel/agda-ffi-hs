{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Coerce where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Coercible)
open import Ffi.Hs.GHC.Exts    using (TYPE; RuntimeRep; UnliftedType)

{-# FOREIGN GHC
import qualified Data.Coerce
import MAlonzo.Code.Ffi.Hs.GHC.Exts (AgdaTYPE)
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaCoercible)
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    coerce   : ⦃ Coercible A B ⦄ → A → B
    coerce-u : ⦃ UnliftedType A ⦄ → ⦃ UnliftedType B ⦄ → ⦃ Coercible A B ⦄ → A → B

    Coercible[A,A] : Coercible A A

{-# COMPILE GHC coerce   = \ aℓ bℓ a b AgdaCoercible                   -> Data.Coerce.coerce #-}
{-# COMPILE GHC coerce-u = \ aℓ bℓ a b AgdaTYPE AgdaTYPE AgdaCoercible -> Data.Coerce.coerce #-}

{-# COMPILE GHC Coercible[A,A] = \ aℓ a -> AgdaCoercible #-}
