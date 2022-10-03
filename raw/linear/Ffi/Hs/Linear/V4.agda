{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.V4 where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.V4
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level


record V4 (A : Set aℓ) : Set aℓ where
    constructor mkV4
    field
        x y z w : A

{-# FOREIGN GHC type AgdaV4 aℓ = Linear.V4.V4 #-}
{-# COMPILE GHC V4 = data(1) AgdaV4 (Linear.V4.V4) #-}
