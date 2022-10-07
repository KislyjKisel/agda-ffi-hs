{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.V4 where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Linear.Vector using (Additive)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.V4
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Linear.Vector (AgdaAdditive(AgdaAdditive))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

record V4 (A : Set aℓ) : Set aℓ where
    constructor mkV4
    field
        x y z w : A

{-# FOREIGN GHC type AgdaV4 aℓ = Linear.V4.V4 #-}
{-# COMPILE GHC V4 = data(1) AgdaV4 (Linear.V4.V4) #-}

postulate
    Num[V4[A]] : ⦃ Num A ⦄ → Num (V4 A)
    Additive[V4] : Additive {aℓ} V4
    Functor[V4] : Functor {aℓ} V4

{-# COMPILE GHC Num[V4[A]]   = \ aℓ a AgdaNum -> AgdaNum      #-}
{-# COMPILE GHC Additive[V4] = \ aℓ           -> AgdaAdditive #-}
{-# COMPILE GHC Functor[V4]  = \ aℓ           -> AgdaFunctor  #-}
