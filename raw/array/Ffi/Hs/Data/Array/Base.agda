{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.Base where

open import Agda.Primitive

{-# FOREIGN GHC
import qualified Data.Array
#-}

postulate
    Array : ∀{iℓ eℓ} → Set iℓ → Set eℓ → Set (iℓ ⊔ eℓ)

{-# FOREIGN GHC type AgdaArray iℓ eℓ = Data.Array.Array #-}
{-# COMPILE GHC Array = type(2) AgdaArray #-}
