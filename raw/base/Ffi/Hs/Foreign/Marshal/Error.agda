{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Marshal.Error where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Ord; Num)
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Foreign.Ptr using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.Marshal.Error
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    throwIf     : (A → Bool) → (A → List Char) → IO A → IO A
    throwIf-    : (A → Bool) → (A → List Char) → IO A → IO (⊤ {lzero})
    throwIfNeg  : ⦃ Ord A ⦄ → ⦃ Num A ⦄ → (A → List Char) → IO A → IO A
    throwIfNeg- : ⦃ Ord A ⦄ → ⦃ Num A ⦄ → (A → List Char) → IO A → IO (⊤ {lzero})
    throwIfNull : List Char → IO (Ptr A) → IO (Ptr A)

{-# COMPILE GHC throwIf     = \ aℓ a                 -> Foreign.Marshal.Error.throwIf     #-}
{-# COMPILE GHC throwIf-    = \ aℓ a                 -> Foreign.Marshal.Error.throwIf_    #-}
{-# COMPILE GHC throwIfNeg  = \ aℓ a AgdaOrd AgdaNum -> Foreign.Marshal.Error.throwIfNeg  #-}
{-# COMPILE GHC throwIfNeg- = \ aℓ a AgdaOrd AgdaNum -> Foreign.Marshal.Error.throwIfNeg_ #-}
{-# COMPILE GHC throwIfNull = \ aℓ a                 -> Foreign.Marshal.Error.throwIfNull #-}
