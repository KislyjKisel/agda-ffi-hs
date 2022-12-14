{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Int-Instanced where

open import Ffi.Hs.Data.Int

instance
    inst:Data[Int]       = Data[Int]
    inst:Storable[Int]   = Storable[Int]
    inst:Bits[Int]       = Bits[Int]
    inst:FiniteBits[Int] = FiniteBits[Int]
    inst:Bounded[Int]    = Bounded[Int]
    inst:Enum[Int]       = Enum[Int]
    inst:Ix[Int]         = Ix[Int]
    inst:Num[Int]        = Num[Int]
    inst:Read[Int]       = Read[Int]
    inst:Integral[Int]   = Integral[Int]
    inst:Real[Int]       = Real[Int]
    inst:Show[Int]       = Show[Int]
    inst:Eq[Int]         = Eq[Int]
    inst:Ord[Int]        = Ord[Int]

    inst:Data[Int8]       = Data[Int8]
    inst:Storable[Int8]   = Storable[Int8]
    inst:Bits[Int8]       = Bits[Int8]
    inst:FiniteBits[Int8] = FiniteBits[Int8]
    inst:Bounded[Int8]    = Bounded[Int8]
    inst:Enum[Int8]       = Enum[Int8]
    inst:Ix[Int8]         = Ix[Int8]
    inst:Num[Int8]        = Num[Int8]
    inst:Read[Int8]       = Read[Int8]
    inst:Integral[Int8]   = Integral[Int8]
    inst:Real[Int8]       = Real[Int8]
    inst:Show[Int8]       = Show[Int8]
    inst:Eq[Int8]         = Eq[Int8]
    inst:Ord[Int8]        = Ord[Int8]

    inst:Data[Int16]       = Data[Int16]
    inst:Storable[Int16]   = Storable[Int16]
    inst:Bits[Int16]       = Bits[Int16]
    inst:FiniteBits[Int16] = FiniteBits[Int16]
    inst:Bounded[Int16]    = Bounded[Int16]
    inst:Enum[Int16]       = Enum[Int16]
    inst:Ix[Int16]         = Ix[Int16]
    inst:Num[Int16]        = Num[Int16]
    inst:Read[Int16]       = Read[Int16]
    inst:Integral[Int16]   = Integral[Int16]
    inst:Real[Int16]       = Real[Int16]
    inst:Show[Int16]       = Show[Int16]
    inst:Eq[Int16]         = Eq[Int16]
    inst:Ord[Int16]        = Ord[Int16]

    inst:Data[Int32]       = Data[Int32]
    inst:Storable[Int32]   = Storable[Int32]
    inst:Bits[Int32]       = Bits[Int32]
    inst:FiniteBits[Int32] = FiniteBits[Int32]
    inst:Bounded[Int32]    = Bounded[Int32]
    inst:Enum[Int32]       = Enum[Int32]
    inst:Ix[Int32]         = Ix[Int32]
    inst:Num[Int32]        = Num[Int32]
    inst:Read[Int32]       = Read[Int32]
    inst:Integral[Int32]   = Integral[Int32]
    inst:Real[Int32]       = Real[Int32]
    inst:Show[Int32]       = Show[Int32]
    inst:Eq[Int32]         = Eq[Int32]
    inst:Ord[Int32]        = Ord[Int32]

    inst:Data[Int64]       = Data[Int64]
    inst:Storable[Int64]   = Storable[Int64]
    inst:Bits[Int64]       = Bits[Int64]
    inst:FiniteBits[Int64] = FiniteBits[Int64]
    inst:Bounded[Int64]    = Bounded[Int64]
    inst:Enum[Int64]       = Enum[Int64]
    inst:Ix[Int64]         = Ix[Int64]
    inst:Num[Int64]        = Num[Int64]
    inst:Read[Int64]       = Read[Int64]
    inst:Integral[Int64]   = Integral[Int64]
    inst:Real[Int64]       = Real[Int64]
    inst:Show[Int64]       = Show[Int64]
    inst:Eq[Int64]         = Eq[Int64]
    inst:Ord[Int64]        = Ord[Int64]

    inst:Lit-FromNat[Int]   = Lit-FromNat[Int]
    inst:Lit-FromNat[Int8]  = Lit-FromNat[Int8]
    inst:Lit-FromNat[Int16] = Lit-FromNat[Int16]
    inst:Lit-FromNat[Int32] = Lit-FromNat[Int32]
    inst:Lit-FromNat[Int64] = Lit-FromNat[Int64]

    inst:Lit-FromNeg[Int]   = Lit-FromNeg[Int]
    inst:Lit-FromNeg[Int8]  = Lit-FromNeg[Int8]
    inst:Lit-FromNeg[Int16] = Lit-FromNeg[Int16]
    inst:Lit-FromNeg[Int32] = Lit-FromNeg[Int32]
    inst:Lit-FromNeg[Int64] = Lit-FromNeg[Int64]
