{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Int where

open import Agda.Builtin.Bool          using (true; false)
open import Agda.Builtin.Int      as ℤ using ()
open import Agda.Builtin.Nat      as ℕ using (zero; suc)
open import Agda.Builtin.Unit          using (⊤)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Literals
open import Ffi.Hs.Data.Void           using (Void)
open import Ffi.Hs.GHC.Num             using (fromInteger)

open import Ffi.Hs.GHC.Exts public
    using (Int; Int8; Int16; Int32; Int64)

{-# FOREIGN GHC
import qualified Data.Int
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

postulate
    Data[Int]       : Data Int
    Storable[Int]   : Storable Int
    Bits[Int]       : Bits Int
    FiniteBits[Int] : FiniteBits Int
    Bounded[Int]    : Bounded Int
    Enum[Int]       : Enum Int
    Ix[Int]         : Ix Int
    Num[Int]        : Num Int
    Read[Int]       : Read Int
    Integral[Int]   : Integral Int
    Real[Int]       : Real Int
    Show[Int]       : Show Int
    Eq[Int]         : Eq Int
    Ord[Int]        : Ord Int

    Data[Int8]       : Data Int8
    Storable[Int8]   : Storable Int8
    Bits[Int8]       : Bits Int8
    FiniteBits[Int8] : FiniteBits Int8
    Bounded[Int8]    : Bounded Int8
    Enum[Int8]       : Enum Int8
    Ix[Int8]         : Ix Int8
    Num[Int8]        : Num Int8
    Read[Int8]       : Read Int8
    Integral[Int8]   : Integral Int8
    Real[Int8]       : Real Int8
    Show[Int8]       : Show Int8
    Eq[Int8]         : Eq Int8
    Ord[Int8]        : Ord Int8

    Data[Int16]       : Data Int16
    Storable[Int16]   : Storable Int16
    Bits[Int16]       : Bits Int16
    FiniteBits[Int16] : FiniteBits Int16
    Bounded[Int16]    : Bounded Int16
    Enum[Int16]       : Enum Int16
    Ix[Int16]         : Ix Int16
    Num[Int16]        : Num Int16
    Read[Int16]       : Read Int16
    Integral[Int16]   : Integral Int16
    Real[Int16]       : Real Int16
    Show[Int16]       : Show Int16
    Eq[Int16]         : Eq Int16
    Ord[Int16]        : Ord Int16

    Data[Int32]       : Data Int32
    Storable[Int32]   : Storable Int32
    Bits[Int32]       : Bits Int32
    FiniteBits[Int32] : FiniteBits Int32
    Bounded[Int32]    : Bounded Int32
    Enum[Int32]       : Enum Int32
    Ix[Int32]         : Ix Int32
    Num[Int32]        : Num Int32
    Read[Int32]       : Read Int32
    Integral[Int32]   : Integral Int32
    Real[Int32]       : Real Int32
    Show[Int32]       : Show Int32
    Eq[Int32]         : Eq Int32
    Ord[Int32]        : Ord Int32

    Data[Int64]       : Data Int64
    Storable[Int64]   : Storable Int64
    Bits[Int64]       : Bits Int64
    FiniteBits[Int64] : FiniteBits Int64
    Bounded[Int64]    : Bounded Int64
    Enum[Int64]       : Enum Int64
    Ix[Int64]         : Ix Int64
    Num[Int64]        : Num Int64
    Read[Int64]       : Read Int64
    Integral[Int64]   : Integral Int64
    Real[Int64]       : Real Int64
    Show[Int64]       : Show Int64
    Eq[Int64]         : Eq Int64
    Ord[Int64]        : Ord Int64

{-# COMPILE GHC Data[Int]       = AgdaData       #-}
{-# COMPILE GHC Storable[Int]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Int]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Int] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Int]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Int]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Int]         = AgdaIx         #-}
{-# COMPILE GHC Num[Int]        = AgdaNum        #-}
{-# COMPILE GHC Read[Int]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Int]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Int]       = AgdaReal       #-}
{-# COMPILE GHC Show[Int]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Int]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Int]        = AgdaOrd        #-}

{-# COMPILE GHC Data[Int8]       = AgdaData       #-}
{-# COMPILE GHC Storable[Int8]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Int8]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Int8] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Int8]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Int8]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Int8]         = AgdaIx         #-}
{-# COMPILE GHC Num[Int8]        = AgdaNum        #-}
{-# COMPILE GHC Read[Int8]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Int8]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Int8]       = AgdaReal       #-}
{-# COMPILE GHC Show[Int8]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Int8]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Int8]        = AgdaOrd        #-}

{-# COMPILE GHC Data[Int16]       = AgdaData       #-}
{-# COMPILE GHC Storable[Int16]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Int16]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Int16] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Int16]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Int16]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Int16]         = AgdaIx         #-}
{-# COMPILE GHC Num[Int16]        = AgdaNum        #-}
{-# COMPILE GHC Read[Int16]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Int16]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Int16]       = AgdaReal       #-}
{-# COMPILE GHC Show[Int16]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Int16]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Int16]        = AgdaOrd        #-}

{-# COMPILE GHC Data[Int32]       = AgdaData       #-}
{-# COMPILE GHC Storable[Int32]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Int32]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Int32] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Int32]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Int32]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Int32]         = AgdaIx         #-}
{-# COMPILE GHC Num[Int32]        = AgdaNum        #-}
{-# COMPILE GHC Read[Int32]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Int32]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Int32]       = AgdaReal       #-}
{-# COMPILE GHC Show[Int32]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Int32]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Int32]        = AgdaOrd        #-}

{-# COMPILE GHC Data[Int64]       = AgdaData       #-}
{-# COMPILE GHC Storable[Int64]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Int64]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Int64] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Int64]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Int64]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Int64]         = AgdaIx         #-}
{-# COMPILE GHC Num[Int64]        = AgdaNum        #-}
{-# COMPILE GHC Read[Int64]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Int64]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Int64]       = AgdaReal       #-}
{-# COMPILE GHC Show[Int64]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Int64]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Int64]        = AgdaOrd        #-}


Lit-FromNat[Int] : Lit-FromNat Int
Lit-FromNat[Int] .Lit-ConstrainNat n with n ℕ.< 536870912
... | false = Void
... | true  = ⊤
Lit-FromNat[Int] .Lit-fromNat n = fromInteger ⦃ Num[Int] ⦄ (ℤ.pos n)

Lit-FromNat[Int8] : Lit-FromNat Int8
Lit-FromNat[Int8] .Lit-ConstrainNat n with n ℕ.< 128
... | false = Void
... | true  = ⊤
Lit-FromNat[Int8] .Lit-fromNat n = fromInteger ⦃ Num[Int8] ⦄ (ℤ.pos n)

Lit-FromNat[Int16] : Lit-FromNat Int16
Lit-FromNat[Int16] .Lit-ConstrainNat n with n ℕ.< 32768
... | false = Void
... | true  = ⊤
Lit-FromNat[Int16] .Lit-fromNat n = fromInteger ⦃ Num[Int16] ⦄ (ℤ.pos n)

Lit-FromNat[Int32] : Lit-FromNat Int32
Lit-FromNat[Int32] .Lit-ConstrainNat n with n ℕ.< 2147483648
... | false = Void
... | true  = ⊤
Lit-FromNat[Int32] .Lit-fromNat n = fromInteger ⦃ Num[Int32] ⦄ (ℤ.pos n)

Lit-FromNat[Int64] : Lit-FromNat Int64
Lit-FromNat[Int64] .Lit-ConstrainNat n with n ℕ.< 9223372036854775808
... | false = Void
... | true  = ⊤
Lit-FromNat[Int64] .Lit-fromNat n = fromInteger ⦃ Num[Int64] ⦄ (ℤ.pos n)

Lit-FromNeg[Int] : Lit-FromNeg Int
Lit-FromNeg[Int] .Lit-ConstrainNeg n with n ℕ.< 536870913
... | false = Void
... | true  = ⊤
Lit-FromNeg[Int] .Lit-fromNeg zero    = fromInteger ⦃ Num[Int] ⦄ (ℤ.pos 0)
Lit-FromNeg[Int] .Lit-fromNeg (suc n) = fromInteger ⦃ Num[Int] ⦄ (ℤ.negsuc n)

Lit-FromNeg[Int8] : Lit-FromNeg Int8
Lit-FromNeg[Int8] .Lit-ConstrainNeg n with n ℕ.< 129
... | false = Void
... | true  = ⊤
Lit-FromNeg[Int8] .Lit-fromNeg zero    = fromInteger ⦃ Num[Int8] ⦄ (ℤ.pos 0)
Lit-FromNeg[Int8] .Lit-fromNeg (suc n) = fromInteger ⦃ Num[Int8] ⦄ (ℤ.negsuc n)

Lit-FromNeg[Int16] : Lit-FromNeg Int16
Lit-FromNeg[Int16] .Lit-ConstrainNeg n with n ℕ.< 32769
... | false = Void
... | true  = ⊤
Lit-FromNeg[Int16] .Lit-fromNeg zero    = fromInteger ⦃ Num[Int16] ⦄ (ℤ.pos 0)
Lit-FromNeg[Int16] .Lit-fromNeg (suc n) = fromInteger ⦃ Num[Int16] ⦄ (ℤ.negsuc n)

Lit-FromNeg[Int32] : Lit-FromNeg Int32
Lit-FromNeg[Int32] .Lit-ConstrainNeg n with n ℕ.< 2147483649
... | false = Void
... | true  = ⊤
Lit-FromNeg[Int32] .Lit-fromNeg zero    = fromInteger ⦃ Num[Int32] ⦄ (ℤ.pos 0)
Lit-FromNeg[Int32] .Lit-fromNeg (suc n) = fromInteger ⦃ Num[Int32] ⦄ (ℤ.negsuc n)

Lit-FromNeg[Int64] : Lit-FromNeg Int64
Lit-FromNeg[Int64] .Lit-ConstrainNeg n with n ℕ.< 9223372036854775809
... | false = Void
... | true  = ⊤
Lit-FromNeg[Int64] .Lit-fromNeg zero    = fromInteger ⦃ Num[Int64] ⦄ (ℤ.pos 0)
Lit-FromNeg[Int64] .Lit-fromNeg (suc n) = fromInteger ⦃ Num[Int64] ⦄ (ℤ.negsuc n)
