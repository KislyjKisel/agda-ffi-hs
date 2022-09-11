{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.STM where

postulate
    STM : ∀{aℓ} → Set aℓ → Set aℓ

{-# FOREIGN GHC import qualified GHC.Conc #-}
{-# FOREIGN GHC type AgdaSTM aℓ = GHC.Conc.STM #-}
{-# COMPILE GHC STM = type(1) AgdaSTM #-}
