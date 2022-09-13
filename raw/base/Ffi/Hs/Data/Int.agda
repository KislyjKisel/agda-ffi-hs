{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Int where

open import Ffi.Hs.-base.Class

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
