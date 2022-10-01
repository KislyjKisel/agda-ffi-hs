{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Trans.Class where

open import Ffi.Hs.-base.Class using (Monad)

{-# FOREIGN GHC
import qualified Control.Monad.Trans.Class
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonad(AgdaMonad))
#-}

postulate
    MonadTrans : ∀{tℓ} → ((Set tℓ → Set tℓ) → Set tℓ → Set tℓ) → Set tℓ

    lift : ∀{tℓ} {T : (Set tℓ → Set tℓ) → Set tℓ → Set tℓ} {M : Set tℓ → Set tℓ} {A : Set tℓ} →
        ⦃ MonadTrans T ⦄ → ⦃ Monad M ⦄ → M A → T M A

{-# FOREIGN GHC data AgdaMonadTrans tℓ t = Control.Monad.Trans.Class.MonadTrans t => AgdaMonadTrans #-}
{-# COMPILE GHC MonadTrans = type(0) AgdaMonadTrans #-}

{-# COMPILE GHC lift = \ tℓ t m a AgdaMonadTrans AgdaMonad -> Control.Monad.Trans.Class.lift #-}
