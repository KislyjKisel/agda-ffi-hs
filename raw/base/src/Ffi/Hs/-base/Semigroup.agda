{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Semigroup where

postulate
    Semigroup : ∀{ℓ} → Set ℓ → Set ℓ

{-# FOREIGN GHC import qualified Data.Semigroup #-}
{-# FOREIGN GHC type AgdaSemigroup ℓ = Data.Semigroup.Semigroup #-}
{-# COMPILE GHC Semigroup            = type(1) AgdaSemigroup    #-}
