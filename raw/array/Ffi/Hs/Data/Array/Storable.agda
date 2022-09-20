{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.Storable where

open import Agda.Builtin.IO    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Storable)
open import Ffi.Hs.-base.Unit  using (⊤; ⊤′)
open import Ffi.Hs.Foreign.Ptr using (Ptr)

open import Ffi.Hs.Data.Array.MArray public

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Array.StorableArray
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Array.MArray (AgdaMArray)
#-}

private
    variable
        iℓ eℓ aℓ : Level
        I : Set iℓ
        E : Set eℓ
        A : Set aℓ

postulate
    StorableArray : Set aℓ → Set aℓ → Set aℓ

    MArray[StorableArray,E,IO] : ⦃ Storable E ⦄ → MArray StorableArray E IO

    withStorableArray  : StorableArray I E → (Ptr E → IO A) → IO A
    touchStorableArray : StorableArray I E → IO ⊤

{-# FOREIGN GHC type AgdaStorableArray aℓ = Data.Array.StorableArray.StorableArray #-}
{-# COMPILE GHC StorableArray = type(1) AgdaStorableArray #-}

{-# COMPILE GHC MArray[StorableArray,E,IO] = \ eℓ e AgdaStorable -> AgdaMArray #-}

{-# COMPILE GHC withStorableArray  = \ iℓ i e aℓ a -> Data.Array.StorableArray.withStorableArray  #-}
{-# COMPILE GHC touchStorableArray = \ iℓ i e      -> Data.Array.StorableArray.touchStorableArray #-}
