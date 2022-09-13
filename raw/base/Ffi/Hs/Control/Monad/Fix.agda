{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Fix where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Monad)

open Ffi.Hs.-base.Class public
    using (MonadFix)

{-# FOREIGN GHC
import qualified Control.Monad.Fix
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ mℓ : Level
        A : Set aℓ
        M : Set mℓ → Set mℓ

postulate
    mfix : ⦃ MonadFix M ⦄ → (A → M A) → M A

{-# COMPILE GHC mfix = \ mℓ m a AgdaMonadFix -> Control.Monad.Fix.mfix #-}

postulate
    MonadFix[M]⇒Monad[M] : ⦃ MonadFix M ⦄ → Monad M

{-# COMPILE GHC MonadFix[M]⇒Monad[M] = \ mℓ m AgdaMonadFix -> AgdaMonad #-}
