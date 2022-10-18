{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.CaseInsensitive.Unsafe where

open import Agda.Primitive              using (Level)
open import Ffi.Hs.Data.CaseInsensitive using (CI; FoldCase)

{-# FOREIGN GHC
import qualified Data.CaseInsensitive.Unsafe
import MAlonzo.Code.Ffi.Hs.Data.CaseInsensitive (AgdaFoldCase(AgdaFoldCase))
#-}

private
    variable
        sℓ : Level
        S : Set sℓ


postulate
    unsafeMk : ⦃ FoldCase S ⦄ → S → CI S

{-# COMPILE GHC unsafeMk = \ sℓ s AgdaFoldCase -> Data.CaseInsensitive.Unsafe.unsafeMk #-}
