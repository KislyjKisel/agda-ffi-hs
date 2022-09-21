{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Vect where

open import Agda.Primitive

{-# WARNING_ON_IMPORT "unimplemented: requires preprocessor" #-}

{-# FOREIGN GHC
import qualified SDL.Vect
#-}

postulate
    V2 : ∀{aℓ} → Set aℓ → Set aℓ
    V3 : ∀{aℓ} → Set aℓ → Set aℓ
    V4 : ∀{aℓ} → Set aℓ → Set aℓ
    Point : ∀{aℓ bℓ} → (Set aℓ → Set bℓ) → Set aℓ → Set (aℓ ⊔ bℓ)

{-# FOREIGN GHC type AgdaV2 aℓ = SDL.Vect.V2 #-}
{-# COMPILE GHC V2 = type(1) AgdaV2 #-}

{-# FOREIGN GHC type AgdaV3 aℓ = SDL.Vect.V3 #-}
{-# COMPILE GHC V3 = type(1) AgdaV3 #-}

{-# FOREIGN GHC type AgdaV4 aℓ = SDL.Vect.V4 #-}
{-# COMPILE GHC V4 = type(1) AgdaV4 #-}

{-# FOREIGN GHC type AgdaPoint aℓ bℓ = SDL.Vect.Point #-}
{-# COMPILE GHC Point = type(2) AgdaPoint #-}
