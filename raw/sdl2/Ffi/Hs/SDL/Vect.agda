{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Vect where

open import Agda.Primitive

open import Ffi.Hs.Linear.V2 public
    using
    ( V2
    ; mkV2
    ; Functor[V2]
    )

open import Ffi.Hs.Linear.V4 public
    using
    ( V4
    ; mkV4
    )

open import Ffi.Hs.Linear.Affine public

-- todo: preprocessor (cabal flags)

{-# FOREIGN GHC
import qualified SDL.Vect
#-}

postulate
    V3 : ∀{aℓ} → Set aℓ → Set aℓ

{-# FOREIGN GHC type AgdaV3 aℓ = SDL.Vect.V3 #-}
{-# COMPILE GHC V3 = type(1) AgdaV3 #-}
