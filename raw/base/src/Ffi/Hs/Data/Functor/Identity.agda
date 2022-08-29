{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Identity where

open import Agda.Primitive

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Identity    : Set aℓ → Set aℓ
    mkIdentity  : A → Identity A
    runIdentity : Identity A → A

{-# FOREIGN GHC import qualified Data.Functor.Identity #-}
{-# COMPILE GHC Identity    = type(0) Data.Functor.Identity.Identity #-}
{-# COMPILE GHC mkIdentity  = Data.Functor.Identity.Identity         #-}
{-# COMPILE GHC runIdentity = Data.Functor.Identity.runIdentity      #-}
