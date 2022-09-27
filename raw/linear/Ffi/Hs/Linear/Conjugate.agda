{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.Conjugate where

open import Agda.Builtin.Int       using () renaming (Int to Integer)
open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Num; RealFloat)
open import Ffi.Hs.Data.Complex    using (Complex)
open import Ffi.Hs.Data.Int        using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Word       using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.Foreign.C.Types using (CFloat; CDouble)
open import Ffi.Hs.GHC.Float       using (Float; Double)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.Conjugate
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


postulate
    Conjugate : Set aℓ → Set aℓ
    Conjugate[A]⇒Num[A] : ⦃ Conjugate A ⦄ → Num A

    conjugate : ⦃ Conjugate A ⦄ → A → A

    Conjugate[Double]     : Conjugate Double
    Conjugate[Float]      : Conjugate Float
    Conjugate[Int]        : Conjugate Int
    Conjugate[Int8]       : Conjugate Int8
    Conjugate[Int16]      : Conjugate Int16
    Conjugate[Int32]      : Conjugate Int32
    Conjugate[Int64]      : Conjugate Int64
    Conjugate[Integer]    : Conjugate Integer
    Conjugate[Word]       : Conjugate Word
    Conjugate[Word8]      : Conjugate Word8
    Conjugate[Word16]     : Conjugate Word16
    Conjugate[Word32]     : Conjugate Word32
    Conjugate[Word64]     : Conjugate Word64
    Conjugate[CFloat]     : Conjugate CFloat
    Conjugate[CDouble]    : Conjugate CDouble
    Conjugate[Complex[A]] : ⦃ Conjugate A ⦄ → ⦃ RealFloat A ⦄ → Conjugate (Complex A)

{-# FOREIGN GHC data AgdaConjugate aℓ a = Linear.Conjugate.Conjugate a => AgdaConjugate #-}
{-# COMPILE GHC Conjugate = type(0) AgdaConjugate #-}

{-# COMPILE GHC Conjugate[A]⇒Num[A] = \ aℓ a AgdaConjugate -> AgdaNum #-}

{-# COMPILE GHC conjugate = \ aℓ a AgdaConjugate -> Linear.Conjugate.conjugate #-}

{-# COMPILE GHC Conjugate[Double]     =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Float]      =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Int]        =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Int8]       =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Int16]      =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Int32]      =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Int64]      =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Integer]    =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Word]       =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Word8]      =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Word16]     =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Word32]     =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Word64]     =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[CFloat]     =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[CDouble]    =                                       AgdaConjugate #-}
{-# COMPILE GHC Conjugate[Complex[A]] = \ aℓ a AgdaConjugate AgdaRealFloat -> AgdaConjugate #-}


postulate
    TrivialConjugate : Set aℓ → Set aℓ
    TrivialConjugate[A]⇒Conjugate[A] : ⦃ TrivialConjugate A ⦄ → Conjugate A

    TrivialConjugate[Double]  : TrivialConjugate Double
    TrivialConjugate[Float]   : TrivialConjugate Float
    TrivialConjugate[Int]     : TrivialConjugate Int
    TrivialConjugate[Int8]    : TrivialConjugate Int8
    TrivialConjugate[Int16]   : TrivialConjugate Int16
    TrivialConjugate[Int32]   : TrivialConjugate Int32
    TrivialConjugate[Int64]   : TrivialConjugate Int64
    TrivialConjugate[Integer] : TrivialConjugate Integer
    TrivialConjugate[Word]    : TrivialConjugate Word
    TrivialConjugate[Word8]   : TrivialConjugate Word8
    TrivialConjugate[Word16]  : TrivialConjugate Word16
    TrivialConjugate[Word32]  : TrivialConjugate Word32
    TrivialConjugate[Word64]  : TrivialConjugate Word64
    TrivialConjugate[CFloat]  : TrivialConjugate CFloat
    TrivialConjugate[CDouble] : TrivialConjugate CDouble

{-# FOREIGN GHC data AgdaTrivialConjugate aℓ a = Linear.Conjugate.TrivialConjugate a => AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate = type(0) AgdaTrivialConjugate #-}

{-# COMPILE GHC TrivialConjugate[A]⇒Conjugate[A] = \ aℓ a AgdaTrivialConjugate -> AgdaConjugate #-}

{-# COMPILE GHC TrivialConjugate[Double]  = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Float]   = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Int]     = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Int8]    = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Int16]   = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Int32]   = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Int64]   = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Integer] = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Word]    = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Word8]   = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Word16]  = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Word32]  = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[Word64]  = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[CFloat]  = AgdaTrivialConjugate #-}
{-# COMPILE GHC TrivialConjugate[CDouble] = AgdaTrivialConjugate #-}
