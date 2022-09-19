{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.Epsilon where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Num; RealFloat)
open import Ffi.Hs.-base.Float     using (Double; Float)
open import Ffi.Hs.Data.Complex    using (Complex)
open import Ffi.Hs.Foreign.C.Types using (CFloat; CDouble)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.Epsilon
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Epsilon : Set aℓ → Set aℓ
    Epsilon[A]⇒Num[A] : ⦃ Epsilon A ⦄ → Num A
    nearZero : ⦃ Epsilon A ⦄ → A → Bool

    Epsilon[Double]     : Epsilon Double
    Epsilon[Float]      : Epsilon Float
    Epsilon[CFloat]     : Epsilon CFloat
    Epsilon[CDouble]    : Epsilon CDouble
    Epsilon[Complex[A]] : ⦃ Epsilon A ⦄ → ⦃ RealFloat A ⦄ → Epsilon (Complex A)

{-# FOREIGN GHC data AgdaEpsilon aℓ a = Linear.Epsilon.Epsilon a => AgdaEpsilon #-}
{-# COMPILE GHC Epsilon = type(0) AgdaEpsilon #-}

{-# COMPILE GHC Epsilon[A]⇒Num[A] = \ aℓ a AgdaEpsilon -> AgdaNum #-}

{-# COMPILE GHC nearZero = \ aℓ a AgdaEpsilon -> Linear.Epsilon.nearZero #-}

{-# COMPILE GHC Epsilon[Double]     =                                     AgdaEpsilon #-}
{-# COMPILE GHC Epsilon[Float]      =                                     AgdaEpsilon #-}
{-# COMPILE GHC Epsilon[CFloat]     =                                     AgdaEpsilon #-}
{-# COMPILE GHC Epsilon[CDouble]    =                                     AgdaEpsilon #-}
{-# COMPILE GHC Epsilon[Complex[A]] = \ aℓ a AgdaEpsilon AgdaRealFloat -> AgdaEpsilon #-}
