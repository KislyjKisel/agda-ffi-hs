{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.IO.BufferedIO where

open import Agda.Primitive

private
    variable
        aℓ : Level

postulate
    BufferedIO : Set aℓ → Set aℓ
    -- newBuffer : A → BufferState → IO (Buffer Word8)
