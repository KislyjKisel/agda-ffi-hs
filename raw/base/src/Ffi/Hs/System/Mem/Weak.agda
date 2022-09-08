{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Mem.Weak where

open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

{-# FOREIGN GHC
import qualified System.Mem.Weak
#-}

private
    variable
        aℓ ℓ : Level
        K V : Set aℓ

postulate
    Weak         : Set aℓ → Set aℓ
    mkWeak       : K → V → (Maybe (IO (⊤ {ℓ}))) → IO (Weak V)
    deRefWeak    : Weak V → IO (Maybe V)
    finalize     : Weak V → IO (⊤ {lzero})
    mkWeakPtr    : K → Maybe (IO (⊤ {ℓ})) → IO (Weak K)
    addFinalizer : K → IO (⊤ {ℓ}) → IO (⊤ {ℓ})
    mkWeakPair   : K → V → Maybe (IO (⊤ {ℓ})) → IO (Weak (Tuple2 K V))

{-# FOREIGN GHC type AgdaWeak aℓ = System.Mem.Weak.Weak #-}
{-# COMPILE GHC Weak = type(1) AgdaWeak #-}

{-# COMPILE GHC mkWeak       = \ kℓ k vℓ v ℓ -> System.Mem.Weak.mkWeak       #-}
{-# COMPILE GHC deRefWeak    = \ vℓ v        -> System.Mem.Weak.deRefWeak    #-}
{-# COMPILE GHC finalize     = \ vℓ v        -> System.Mem.Weak.finalize     #-}
{-# COMPILE GHC mkWeakPtr    = \ kℓ k ℓ      -> System.Mem.Weak.mkWeakPtr    #-}
{-# COMPILE GHC addFinalizer = \ kℓ k ℓ      -> System.Mem.Weak.addFinalizer #-}
{-# COMPILE GHC mkWeakPair   = \ kℓ k vℓ v ℓ -> System.Mem.Weak.mkWeakPair   #-}
