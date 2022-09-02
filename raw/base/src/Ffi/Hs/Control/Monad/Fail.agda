{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Fail where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Monad)

open Ffi.Hs.-base.Class public
    using (MonadFail)

{-# FOREIGN GHC
import qualified Control.Monad.Fail
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class(AgdaMonad, AgdaMonadFail)
#-}

private
    variable
        aℓ mℓ : Level
        A : Set aℓ
        M : Set mℓ → Set mℓ

postulate
    fail : ⦃ MonadFail M ⦄ → List Char → M A

{-# COMPILE GHC fail = \ mℓ a m AgdaMonadFail -> Control.Monad.Fail.fail #-}

module Instances where
    postulate
        MonadFail[M]⇒Monad[M] : ⦃ MonadFail M ⦄ → Monad M

{-# COMPILE GHC Instances.MonadFail[M]⇒Monad[M] = \ mℓ m AgdaMonadFail -> AgdaMonad #-}
