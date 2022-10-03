{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.V2 where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.V2
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level


record V2 (A : Set aℓ) : Set aℓ where
    constructor mkV2
    field
        x y : A

{-# FOREIGN GHC type AgdaV2 aℓ = Linear.V2.V2 #-}
{-# COMPILE GHC V2 = data(1) AgdaV2 (Linear.V2.V2) #-}

postulate
    Functor[V2] : Functor {aℓ} V2

{-# COMPILE GHC Functor[V2] = \ aℓ -> AgdaFunctor #-}
