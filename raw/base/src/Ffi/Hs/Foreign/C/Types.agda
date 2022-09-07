{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.C.Types where

open import Ffi.Hs.-base.Class

{-# FOREIGN GHC
import qualified Foreign.C.Types
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( Agda
    )
#-}

postulate
    CChar CSChar CUChar CWchar : Set
    
    Storable[CChar]   : Storable CChar
    Bits[CChar]       : Bits CChar
    FiniteBits[CChar] : FiniteBits CChar
    Bounded[CChar]    : Bounded CChar
    Enum[CChar]       : Enum CChar
    Ix[CChar]         : Ix CChar
    Num[CChar]        : Num CChar
    Read[CChar]       : Read CChar
    Integral[CChar]   : Integral CChar
    Real[CChar]       : Real CChar
    Show[CChar]       : Show CChar
    Eq[CChar]         : Eq CChar
    Ord[CChar]        : Ord CChar

    Storable[CSChar]   : Storable CSChar
    Bits[CSChar]       : Bits CSChar
    FiniteBits[CSChar] : FiniteBits CSChar
    Bounded[CSChar]    : Bounded CSChar
    Enum[CSChar]       : Enum CSChar
    Ix[CSChar]         : Ix CSChar
    Num[CSChar]        : Num CSChar
    Read[CSChar]       : Read CSChar
    Integral[CSChar]   : Integral CSChar
    Real[CSChar]       : Real CSChar
    Show[CSChar]       : Show CSChar
    Eq[CSChar]         : Eq CSChar
    Ord[CSChar]        : Ord CSChar

    Storable[CUChar]   : Storable CUChar
    Bits[CUChar]       : Bits CUChar
    FiniteBits[CUChar] : FiniteBits CUChar
    Bounded[CUChar]    : Bounded CUChar
    Enum[CUChar]       : Enum CUChar
    Ix[CUChar]         : Ix CUChar
    Num[CUChar]        : Num CUChar
    Read[CUChar]       : Read CUChar
    Integral[CUChar]   : Integral CUChar
    Real[CUChar]       : Real CUChar
    Show[CUChar]       : Show CUChar
    Eq[CUChar]         : Eq CUChar
    Ord[CUChar]        : Ord CUChar

{-# COMPILE GHC CChar  = type Foreign.C.Types.CChar  #-}
{-# COMPILE GHC CSChar = type Foreign.C.Types.CSChar #-}
{-# COMPILE GHC CUChar = type Foreign.C.Types.CUChar #-}
{-# COMPILE GHC CWchar = type Foreign.C.Types.CWchar #-}

{-# COMPILE GHC Storable[CChar]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CChar]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CChar] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CChar]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CChar]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CChar]         = AgdaIx         #-}
{-# COMPILE GHC Num[CChar]        = AgdaNum        #-}
{-# COMPILE GHC Read[CChar]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CChar]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CChar]       = AgdaReal       #-}
{-# COMPILE GHC Show[CChar]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CChar]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CChar]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CSChar]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CSChar]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CSChar] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CSChar]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CSChar]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CSChar]         = AgdaIx         #-}
{-# COMPILE GHC Num[CSChar]        = AgdaNum        #-}
{-# COMPILE GHC Read[CSChar]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CSChar]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CSChar]       = AgdaReal       #-}
{-# COMPILE GHC Show[CSChar]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CSChar]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CSChar]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CUChar]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CUChar]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CUChar] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CUChar]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CUChar]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CUChar]         = AgdaIx         #-}
{-# COMPILE GHC Num[CUChar]        = AgdaNum        #-}
{-# COMPILE GHC Read[CUChar]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CUChar]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CUChar]       = AgdaReal       #-}
{-# COMPILE GHC Show[CUChar]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CUChar]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CUChar]        = AgdaOrd        #-}

postulate
    CShort CUShort CInt CUInt : Set
    CLong CULong CLLong CULLong : Set
    CSize : Set
    CSigAtomic : Set
    CIntPtr CUIntPtr CPtrdiff : Set
    CBool : Set
    CIntMax CUIntMax : Set
    CClock CTime CUSeconds CSUSeconds : Set
    CFloat CDouble : Set
    CFile CFpos CJmpBuf : Set

{-# COMPILE GHC CShort  = type Foreign.C.Types.CShort  #-}
{-# COMPILE GHC CUShort = type Foreign.C.Types.CUShort #-}
{-# COMPILE GHC CInt    = type Foreign.C.Types.CInt    #-}
{-# COMPILE GHC CUInt   = type Foreign.C.Types.CUInt   #-}

{-# COMPILE GHC CLong   = type Foreign.C.Types.CLong   #-}
{-# COMPILE GHC CULong  = type Foreign.C.Types.CULong  #-}
{-# COMPILE GHC CLLong  = type Foreign.C.Types.CLLong  #-}
{-# COMPILE GHC CULLong = type Foreign.C.Types.CULLong #-}

{-# COMPILE GHC CSize = type Foreign.C.Types.CSize #-}

{-# COMPILE GHC CSigAtomic = type Foreign.C.Types.CSigAtomic #-}

{-# COMPILE GHC CIntPtr  = type Foreign.C.Types.CIntPtr  #-}
{-# COMPILE GHC CUIntPtr = type Foreign.C.Types.CUIntPtr #-}
{-# COMPILE GHC CPtrdiff = type Foreign.C.Types.CPtrdiff #-}

{-# COMPILE GHC CBool = type Foreign.C.Types.CBool #-}

{-# COMPILE GHC CIntMax  = type Foreign.C.Types.CIntMax  #-}
{-# COMPILE GHC CUIntMax = type Foreign.C.Types.CUIntMax #-}

{-# COMPILE GHC CClock     = type Foreign.C.Types.CClock     #-}
{-# COMPILE GHC CTime      = type Foreign.C.Types.CTime      #-}
{-# COMPILE GHC CUSeconds  = type Foreign.C.Types.CUSeconds  #-}
{-# COMPILE GHC CSUSeconds = type Foreign.C.Types.CSUSeconds #-}

{-# COMPILE GHC CFloat  = type Foreign.C.Types.CFloat  #-}
{-# COMPILE GHC CDouble = type Foreign.C.Types.CDouble #-}

{-# COMPILE GHC CFile   = type Foreign.C.Types.CFile   #-}
{-# COMPILE GHC CFpos   = type Foreign.C.Types.CFpos   #-}
{-# COMPILE GHC CJmpBuf = type Foreign.C.Types.CJmpBuf #-}
