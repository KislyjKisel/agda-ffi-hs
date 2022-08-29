{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Unit where

record ⊤ {ℓ} : Set ℓ where
    instance constructor tt

{-# FOREIGN GHC type AgdaUnit ℓ = ()      #-}
{-# COMPILE GHC ⊤ = data(1) AgdaUnit (()) #-}
