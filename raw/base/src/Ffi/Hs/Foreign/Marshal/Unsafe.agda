{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Marshal.Unsafe where

open import Agda.Builtin.IO using (IO)

{-# FOREIGN GHC
import qualified Foreign.Marshal.Unsafe
#-}

postulate
    unsafeLocalState : ∀{aℓ} {A : Set aℓ} → IO A → A

{-# COMPILE GHC unsafeLocalState = \ aℓ a -> Foreign.Marshal.Unsafe.unsafeLocalState #-}
