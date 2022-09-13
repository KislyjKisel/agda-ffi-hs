{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.IO.Class where

open import Agda.Builtin.IO    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Monad)

open Ffi.Hs.-base.Class public
    using (MonadIO)

{-# FOREIGN GHC
import qualified Control.Monad.IO.Class
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ mℓ : Level
        A : Set aℓ
        M : Set mℓ → Set mℓ

postulate
    liftIO : ⦃ MonadIO M ⦄ → IO A → M A

{-# COMPILE GHC liftIO = \ mℓ m a AgdaMonadIO -> Control.Monad.IO.Class.liftIO #-}

postulate
    MonadIO[M]⇒Monad[M] : ⦃ MonadIO M ⦄ → Monad M

{-# COMPILE GHC MonadIO[M]⇒Monad[M] = \ mℓ m AgdaMonadIO -> AgdaMonad #-}
