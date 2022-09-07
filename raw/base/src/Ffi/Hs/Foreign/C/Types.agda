{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.C.Types where

open import Ffi.Hs.-base.Class

{-# FOREIGN GHC
import qualified Foreign.C.Types
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaStorable, AgdaBits, AgdaFiniteBits, AgdaBounded
    , AgdaEnum, AgdaIx, AgdaNum, AgdaRead, AgdaIntegral
    , AgdaReal, AgdaShow, AgdaEq, AgdaOrd, AgdaFloating
    , AgdaRealFloat, AgdaRealFrac
    )
#-}

postulate
    CChar    : Set
    CSChar   : Set
    CUChar   : Set
    CWchar   : Set
    CShort   : Set
    CUShort  : Set
    CInt     : Set
    CUInt    : Set
    CLong    : Set
    CULong   : Set
    CLLong   : Set
    CULLong  : Set
    CIntMax  : Set
    CUIntMax : Set

    CFloat  : Set
    CDouble : Set
    
    CSize    : Set
    CIntPtr  : Set
    CUIntPtr : Set
    CPtrdiff : Set

    CClock     : Set
    CTime      : Set
    CUSeconds  : Set
    CSUSeconds : Set
   
    CBool      : Set
    CSigAtomic : Set
    CFile      : Set
    CFpos      : Set
    CJmpBuf    : Set

{-# COMPILE GHC CChar    = type Foreign.C.Types.CChar    #-}
{-# COMPILE GHC CSChar   = type Foreign.C.Types.CSChar   #-}
{-# COMPILE GHC CUChar   = type Foreign.C.Types.CUChar   #-}
{-# COMPILE GHC CWchar   = type Foreign.C.Types.CWchar   #-}
{-# COMPILE GHC CShort   = type Foreign.C.Types.CShort   #-}
{-# COMPILE GHC CUShort  = type Foreign.C.Types.CUShort  #-}
{-# COMPILE GHC CInt     = type Foreign.C.Types.CInt     #-}
{-# COMPILE GHC CUInt    = type Foreign.C.Types.CUInt    #-}
{-# COMPILE GHC CLong    = type Foreign.C.Types.CLong    #-}
{-# COMPILE GHC CULong   = type Foreign.C.Types.CULong   #-}
{-# COMPILE GHC CLLong   = type Foreign.C.Types.CLLong   #-}
{-# COMPILE GHC CULLong  = type Foreign.C.Types.CULLong  #-}
{-# COMPILE GHC CIntMax  = type Foreign.C.Types.CIntMax  #-}
{-# COMPILE GHC CUIntMax = type Foreign.C.Types.CUIntMax #-}

{-# COMPILE GHC CFloat  = type Foreign.C.Types.CFloat  #-}
{-# COMPILE GHC CDouble = type Foreign.C.Types.CDouble #-}

{-# COMPILE GHC CSize = type Foreign.C.Types.CSize #-}
{-# COMPILE GHC CIntPtr  = type Foreign.C.Types.CIntPtr  #-}
{-# COMPILE GHC CUIntPtr = type Foreign.C.Types.CUIntPtr #-}
{-# COMPILE GHC CPtrdiff = type Foreign.C.Types.CPtrdiff #-}

{-# COMPILE GHC CClock     = type Foreign.C.Types.CClock     #-}
{-# COMPILE GHC CTime      = type Foreign.C.Types.CTime      #-}
{-# COMPILE GHC CUSeconds  = type Foreign.C.Types.CUSeconds  #-}
{-# COMPILE GHC CSUSeconds = type Foreign.C.Types.CSUSeconds #-}

{-# COMPILE GHC CBool      = type Foreign.C.Types.CBool      #-}
{-# COMPILE GHC CSigAtomic = type Foreign.C.Types.CSigAtomic #-}
{-# COMPILE GHC CFile      = type Foreign.C.Types.CFile      #-}
{-# COMPILE GHC CFpos      = type Foreign.C.Types.CFpos      #-}
{-# COMPILE GHC CJmpBuf    = type Foreign.C.Types.CJmpBuf    #-}

postulate
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

    Storable[CWchar]   : Storable CWchar
    Bits[CWchar]       : Bits CWchar
    FiniteBits[CWchar] : FiniteBits CWchar
    Bounded[CWchar]    : Bounded CWchar
    Enum[CWchar]       : Enum CWchar
    Ix[CWchar]         : Ix CWchar
    Num[CWchar]        : Num CWchar
    Read[CWchar]       : Read CWchar
    Integral[CWchar]   : Integral CWchar
    Real[CWchar]       : Real CWchar
    Show[CWchar]       : Show CWchar
    Eq[CWchar]         : Eq CWchar
    Ord[CWchar]        : Ord CWchar

    Storable[CShort]   : Storable CShort
    Bits[CShort]       : Bits CShort
    FiniteBits[CShort] : FiniteBits CShort
    Bounded[CShort]    : Bounded CShort
    Enum[CShort]       : Enum CShort
    Ix[CShort]         : Ix CShort
    Num[CShort]        : Num CShort
    Read[CShort]       : Read CShort
    Integral[CShort]   : Integral CShort
    Real[CShort]       : Real CShort
    Show[CShort]       : Show CShort
    Eq[CShort]         : Eq CShort
    Ord[CShort]        : Ord CShort

    Storable[CUShort]   : Storable CUShort
    Bits[CUShort]       : Bits CUShort
    FiniteBits[CUShort] : FiniteBits CUShort
    Bounded[CUShort]    : Bounded CUShort
    Enum[CUShort]       : Enum CUShort
    Ix[CUShort]         : Ix CUShort
    Num[CUShort]        : Num CUShort
    Read[CUShort]       : Read CUShort
    Integral[CUShort]   : Integral CUShort
    Real[CUShort]       : Real CUShort
    Show[CUShort]       : Show CUShort
    Eq[CUShort]         : Eq CUShort
    Ord[CUShort]        : Ord CUShort

    Storable[CInt]   : Storable CInt
    Bits[CInt]       : Bits CInt
    FiniteBits[CInt] : FiniteBits CInt
    Bounded[CInt]    : Bounded CInt
    Enum[CInt]       : Enum CInt
    Ix[CInt]         : Ix CInt
    Num[CInt]        : Num CInt
    Read[CInt]       : Read CInt
    Integral[CInt]   : Integral CInt
    Real[CInt]       : Real CInt
    Show[CInt]       : Show CInt
    Eq[CInt]         : Eq CInt
    Ord[CInt]        : Ord CInt

    Storable[CUInt]   : Storable CUInt
    Bits[CUInt]       : Bits CUInt
    FiniteBits[CUInt] : FiniteBits CUInt
    Bounded[CUInt]    : Bounded CUInt
    Enum[CUInt]       : Enum CUInt
    Ix[CUInt]         : Ix CUInt
    Num[CUInt]        : Num CUInt
    Read[CUInt]       : Read CUInt
    Integral[CUInt]   : Integral CUInt
    Real[CUInt]       : Real CUInt
    Show[CUInt]       : Show CUInt
    Eq[CUInt]         : Eq CUInt
    Ord[CUInt]        : Ord CUInt

    Storable[CLong]   : Storable CLong
    Bits[CLong]       : Bits CLong
    FiniteBits[CLong] : FiniteBits CLong
    Bounded[CLong]    : Bounded CLong
    Enum[CLong]       : Enum CLong
    Ix[CLong]         : Ix CLong
    Num[CLong]        : Num CLong
    Read[CLong]       : Read CLong
    Integral[CLong]   : Integral CLong
    Real[CLong]       : Real CLong
    Show[CLong]       : Show CLong
    Eq[CLong]         : Eq CLong
    Ord[CLong]        : Ord CLong

    Storable[CULong]   : Storable CULong
    Bits[CULong]       : Bits CULong
    FiniteBits[CULong] : FiniteBits CULong
    Bounded[CULong]    : Bounded CULong
    Enum[CULong]       : Enum CULong
    Ix[CULong]         : Ix CULong
    Num[CULong]        : Num CULong
    Read[CULong]       : Read CULong
    Integral[CULong]   : Integral CULong
    Real[CULong]       : Real CULong
    Show[CULong]       : Show CULong
    Eq[CULong]         : Eq CULong
    Ord[CULong]        : Ord CULong

    Storable[CLLong]   : Storable CLLong
    Bits[CLLong]       : Bits CLLong
    FiniteBits[CLLong] : FiniteBits CLLong
    Bounded[CLLong]    : Bounded CLLong
    Enum[CLLong]       : Enum CLLong
    Ix[CLLong]         : Ix CLLong
    Num[CLLong]        : Num CLLong
    Read[CLLong]       : Read CLLong
    Integral[CLLong]   : Integral CLLong
    Real[CLLong]       : Real CLLong
    Show[CLLong]       : Show CLLong
    Eq[CLLong]         : Eq CLLong
    Ord[CLLong]        : Ord CLLong

    Storable[CULLong]   : Storable CULLong
    Bits[CULLong]       : Bits CULLong
    FiniteBits[CULLong] : FiniteBits CULLong
    Bounded[CULLong]    : Bounded CULLong
    Enum[CULLong]       : Enum CULLong
    Ix[CULLong]         : Ix CULLong
    Num[CULLong]        : Num CULLong
    Read[CULLong]       : Read CULLong
    Integral[CULLong]   : Integral CULLong
    Real[CULLong]       : Real CULLong
    Show[CULLong]       : Show CULLong
    Eq[CULLong]         : Eq CULLong
    Ord[CULLong]        : Ord CULLong

    Storable[CIntMax]   : Storable CIntMax
    Bits[CIntMax]       : Bits CIntMax
    FiniteBits[CIntMax] : FiniteBits CIntMax
    Bounded[CIntMax]    : Bounded CIntMax
    Enum[CIntMax]       : Enum CIntMax
    Ix[CIntMax]         : Ix CIntMax
    Num[CIntMax]        : Num CIntMax
    Read[CIntMax]       : Read CIntMax
    Integral[CIntMax]   : Integral CIntMax
    Real[CIntMax]       : Real CIntMax
    Show[CIntMax]       : Show CIntMax
    Eq[CIntMax]         : Eq CIntMax
    Ord[CIntMax]        : Ord CIntMax

    Storable[CUIntMax]   : Storable CUIntMax
    Bits[CUIntMax]       : Bits CUIntMax
    FiniteBits[CUIntMax] : FiniteBits CUIntMax
    Bounded[CUIntMax]    : Bounded CUIntMax
    Enum[CUIntMax]       : Enum CUIntMax
    Ix[CUIntMax]         : Ix CUIntMax
    Num[CUIntMax]        : Num CUIntMax
    Read[CUIntMax]       : Read CUIntMax
    Integral[CUIntMax]   : Integral CUIntMax
    Real[CUIntMax]       : Real CUIntMax
    Show[CUIntMax]       : Show CUIntMax
    Eq[CUIntMax]         : Eq CUIntMax
    Ord[CUIntMax]        : Ord CUIntMax


    Storable[CFloat]   : Storable CFloat
    Enum[CFloat]       : Enum CFloat
    Floating[CFloat]   : Floating CFloat
    RealFloat[CFloat]  : RealFloat CFloat
    Num[CFloat]        : Num CFloat
    Read[CFloat]       : Read CFloat
    Fractional[CFloat] : Fractional CFloat
    Real[CFloat]       : Real CFloat
    RealFrac[CFloat]   : RealFrac CFloat
    Show[CFloat]       : Show CFloat
    Eq[CFloat]         : Eq CFloat
    Ord[CFloat]        : Ord CFloat

    Storable[CDouble]   : Storable CDouble
    Enum[CDouble]       : Enum CDouble
    Floating[CDouble]   : Floating CDouble
    RealFloat[CDouble]  : RealFloat CDouble
    Num[CDouble]        : Num CDouble
    Read[CDouble]       : Read CDouble
    Fractional[CDouble] : Fractional CDouble
    Real[CDouble]       : Real CDouble
    RealFrac[CDouble]   : RealFrac CDouble
    Show[CDouble]       : Show CDouble
    Eq[CDouble]         : Eq CDouble
    Ord[CDouble]        : Ord CDouble

    
    Storable[CSize]   : Storable CSize
    Bits[CSize]       : Bits CSize
    FiniteBits[CSize] : FiniteBits CSize
    Bounded[CSize]    : Bounded CSize
    Enum[CSize]       : Enum CSize
    Ix[CSize]         : Ix CSize
    Num[CSize]        : Num CSize
    Read[CSize]       : Read CSize
    Integral[CSize]   : Integral CSize
    Real[CSize]       : Real CSize
    Show[CSize]       : Show CSize
    Eq[CSize]         : Eq CSize
    Ord[CSize]        : Ord CSize

    Storable[CIntPtr]   : Storable CIntPtr
    Bits[CIntPtr]       : Bits CIntPtr
    FiniteBits[CIntPtr] : FiniteBits CIntPtr
    Bounded[CIntPtr]    : Bounded CIntPtr
    Enum[CIntPtr]       : Enum CIntPtr
    Ix[CIntPtr]         : Ix CIntPtr
    Num[CIntPtr]        : Num CIntPtr
    Read[CIntPtr]       : Read CIntPtr
    Integral[CIntPtr]   : Integral CIntPtr
    Real[CIntPtr]       : Real CIntPtr
    Show[CIntPtr]       : Show CIntPtr
    Eq[CIntPtr]         : Eq CIntPtr
    Ord[CIntPtr]        : Ord CIntPtr

    Storable[CUIntPtr]   : Storable CUIntPtr
    Bits[CUIntPtr]       : Bits CUIntPtr
    FiniteBits[CUIntPtr] : FiniteBits CUIntPtr
    Bounded[CUIntPtr]    : Bounded CUIntPtr
    Enum[CUIntPtr]       : Enum CUIntPtr
    Ix[CUIntPtr]         : Ix CUIntPtr
    Num[CUIntPtr]        : Num CUIntPtr
    Read[CUIntPtr]       : Read CUIntPtr
    Integral[CUIntPtr]   : Integral CUIntPtr
    Real[CUIntPtr]       : Real CUIntPtr
    Show[CUIntPtr]       : Show CUIntPtr
    Eq[CUIntPtr]         : Eq CUIntPtr
    Ord[CUIntPtr]        : Ord CUIntPtr

    Storable[CPtrdiff]   : Storable CPtrdiff
    Bits[CPtrdiff]       : Bits CPtrdiff
    FiniteBits[CPtrdiff] : FiniteBits CPtrdiff
    Bounded[CPtrdiff]    : Bounded CPtrdiff
    Enum[CPtrdiff]       : Enum CPtrdiff
    Ix[CPtrdiff]         : Ix CPtrdiff
    Num[CPtrdiff]        : Num CPtrdiff
    Read[CPtrdiff]       : Read CPtrdiff
    Integral[CPtrdiff]   : Integral CPtrdiff
    Real[CPtrdiff]       : Real CPtrdiff
    Show[CPtrdiff]       : Show CPtrdiff
    Eq[CPtrdiff]         : Eq CPtrdiff
    Ord[CPtrdiff]        : Ord CPtrdiff


    Storable[CClock] : Storable CClock
    Enum[CClock]     : Enum CClock
    Num[CClock]      : Num CClock
    Read[CClock]     : Read CClock
    Real[CClock]     : Real CClock
    Show[CClock]     : Show CClock
    Eq[CClock]       : Eq CClock
    Ord[CClock]      : Ord CClock

    Storable[CTime] : Storable CTime
    Enum[CTime]     : Enum CTime
    Num[CTime]      : Num CTime
    Read[CTime]     : Read CTime
    Real[CTime]     : Real CTime
    Show[CTime]     : Show CTime
    Eq[CTime]       : Eq CTime
    Ord[CTime]      : Ord CTime

    Storable[CUSeconds] : Storable CUSeconds
    Enum[CUSeconds]     : Enum CUSeconds
    Num[CUSeconds]      : Num CUSeconds
    Read[CUSeconds]     : Read CUSeconds
    Real[CUSeconds]     : Real CUSeconds
    Show[CUSeconds]     : Show CUSeconds
    Eq[CUSeconds]       : Eq CUSeconds
    Ord[CUSeconds]      : Ord CUSeconds

    Storable[CSUSeconds] : Storable CSUSeconds
    Enum[CSUSeconds]     : Enum CSUSeconds
    Num[CSUSeconds]      : Num CSUSeconds
    Read[CSUSeconds]     : Read CSUSeconds
    Real[CSUSeconds]     : Real CSUSeconds
    Show[CSUSeconds]     : Show CSUSeconds
    Eq[CSUSeconds]       : Eq CSUSeconds
    Ord[CSUSeconds]      : Ord CSUSeconds


    Storable[CBool]   : Storable CBool
    Bits[CBool]       : Bits CBool
    FiniteBits[CBool] : FiniteBits CBool
    Bounded[CBool]    : Bounded CBool
    Enum[CBool]       : Enum CBool
    Ix[CBool]         : Ix CBool
    Num[CBool]        : Num CBool
    Read[CBool]       : Read CBool
    Integral[CBool]   : Integral CBool
    Real[CBool]       : Real CBool
    Show[CBool]       : Show CBool
    Eq[CBool]         : Eq CBool
    Ord[CBool]        : Ord CBool

    Storable[CSigAtomic]   : Storable CSigAtomic
    Bits[CSigAtomic]       : Bits CSigAtomic
    FiniteBits[CSigAtomic] : FiniteBits CSigAtomic
    Bounded[CSigAtomic]    : Bounded CSigAtomic
    Enum[CSigAtomic]       : Enum CSigAtomic
    Ix[CSigAtomic]         : Ix CSigAtomic
    Num[CSigAtomic]        : Num CSigAtomic
    Read[CSigAtomic]       : Read CSigAtomic
    Integral[CSigAtomic]   : Integral CSigAtomic
    Real[CSigAtomic]       : Real CSigAtomic
    Show[CSigAtomic]       : Show CSigAtomic
    Eq[CSigAtomic]         : Eq CSigAtomic
    Ord[CSigAtomic]        : Ord CSigAtomic

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

{-# COMPILE GHC Storable[CWchar]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CWchar]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CWchar] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CWchar]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CWchar]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CWchar]         = AgdaIx         #-}
{-# COMPILE GHC Num[CWchar]        = AgdaNum        #-}
{-# COMPILE GHC Read[CWchar]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CWchar]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CWchar]       = AgdaReal       #-}
{-# COMPILE GHC Show[CWchar]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CWchar]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CWchar]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CShort]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CShort]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CShort] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CShort]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CShort]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CShort]         = AgdaIx         #-}
{-# COMPILE GHC Num[CShort]        = AgdaNum        #-}
{-# COMPILE GHC Read[CShort]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CShort]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CShort]       = AgdaReal       #-}
{-# COMPILE GHC Show[CShort]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CShort]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CShort]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CUShort]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CUShort]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CUShort] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CUShort]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CUShort]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CUShort]         = AgdaIx         #-}
{-# COMPILE GHC Num[CUShort]        = AgdaNum        #-}
{-# COMPILE GHC Read[CUShort]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CUShort]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CUShort]       = AgdaReal       #-}
{-# COMPILE GHC Show[CUShort]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CUShort]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CUShort]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CInt]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CInt]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CInt] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CInt]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CInt]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CInt]         = AgdaIx         #-}
{-# COMPILE GHC Num[CInt]        = AgdaNum        #-}
{-# COMPILE GHC Read[CInt]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CInt]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CInt]       = AgdaReal       #-}
{-# COMPILE GHC Show[CInt]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CInt]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CInt]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CUInt]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CUInt]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CUInt] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CUInt]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CUInt]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CUInt]         = AgdaIx         #-}
{-# COMPILE GHC Num[CUInt]        = AgdaNum        #-}
{-# COMPILE GHC Read[CUInt]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CUInt]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CUInt]       = AgdaReal       #-}
{-# COMPILE GHC Show[CUInt]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CUInt]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CUInt]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CLong]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CLong]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CLong] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CLong]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CLong]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CLong]         = AgdaIx         #-}
{-# COMPILE GHC Num[CLong]        = AgdaNum        #-}
{-# COMPILE GHC Read[CLong]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CLong]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CLong]       = AgdaReal       #-}
{-# COMPILE GHC Show[CLong]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CLong]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CLong]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CULong]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CULong]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CULong] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CULong]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CULong]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CULong]         = AgdaIx         #-}
{-# COMPILE GHC Num[CULong]        = AgdaNum        #-}
{-# COMPILE GHC Read[CULong]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CULong]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CULong]       = AgdaReal       #-}
{-# COMPILE GHC Show[CULong]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CULong]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CULong]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CLLong]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CLLong]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CLLong] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CLLong]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CLLong]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CLLong]         = AgdaIx         #-}
{-# COMPILE GHC Num[CLLong]        = AgdaNum        #-}
{-# COMPILE GHC Read[CLLong]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CLLong]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CLLong]       = AgdaReal       #-}
{-# COMPILE GHC Show[CLLong]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CLLong]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CLLong]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CULLong]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CULLong]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CULLong] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CULLong]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CULLong]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CULLong]         = AgdaIx         #-}
{-# COMPILE GHC Num[CULLong]        = AgdaNum        #-}
{-# COMPILE GHC Read[CULLong]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CULLong]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CULLong]       = AgdaReal       #-}
{-# COMPILE GHC Show[CULLong]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CULLong]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CULLong]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CIntMax]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CIntMax]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CIntMax] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CIntMax]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CIntMax]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CIntMax]         = AgdaIx         #-}
{-# COMPILE GHC Num[CIntMax]        = AgdaNum        #-}
{-# COMPILE GHC Read[CIntMax]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CIntMax]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CIntMax]       = AgdaReal       #-}
{-# COMPILE GHC Show[CIntMax]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CIntMax]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CIntMax]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CUIntMax]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CUIntMax]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CUIntMax] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CUIntMax]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CUIntMax]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CUIntMax]         = AgdaIx         #-}
{-# COMPILE GHC Num[CUIntMax]        = AgdaNum        #-}
{-# COMPILE GHC Read[CUIntMax]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CUIntMax]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CUIntMax]       = AgdaReal       #-}
{-# COMPILE GHC Show[CUIntMax]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CUIntMax]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CUIntMax]        = AgdaOrd        #-}


{-# COMPILE GHC Storable[CFloat]   = AgdaStorable   #-}
{-# COMPILE GHC Enum[CFloat]       = AgdaEnum       #-}
{-# COMPILE GHC Floating[CFloat]   = AgdaBits       #-}
{-# COMPILE GHC RealFloat[CFloat]  = AgdaFiniteBits #-}
{-# COMPILE GHC Num[CFloat]        = AgdaNum        #-}
{-# COMPILE GHC Read[CFloat]       = AgdaRead       #-}
{-# COMPILE GHC Fractional[CFloat] = AgdaBounded    #-}
{-# COMPILE GHC Real[CFloat]       = AgdaReal       #-}
{-# COMPILE GHC RealFrac[CFloat]   = AgdaIx         #-}
{-# COMPILE GHC Show[CFloat]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CFloat]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CFloat]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CDouble]   = AgdaStorable   #-}
{-# COMPILE GHC Enum[CDouble]       = AgdaEnum       #-}
{-# COMPILE GHC Floating[CDouble]   = AgdaBits       #-}
{-# COMPILE GHC RealFloat[CDouble]  = AgdaFiniteBits #-}
{-# COMPILE GHC Num[CDouble]        = AgdaNum        #-}
{-# COMPILE GHC Read[CDouble]       = AgdaRead       #-}
{-# COMPILE GHC Fractional[CDouble] = AgdaBounded    #-}
{-# COMPILE GHC Real[CDouble]       = AgdaReal       #-}
{-# COMPILE GHC RealFrac[CDouble]   = AgdaIx         #-}
{-# COMPILE GHC Show[CDouble]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CDouble]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CDouble]        = AgdaOrd        #-}


{-# COMPILE GHC Storable[CSize]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CSize]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CSize] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CSize]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CSize]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CSize]         = AgdaIx         #-}
{-# COMPILE GHC Num[CSize]        = AgdaNum        #-}
{-# COMPILE GHC Read[CSize]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CSize]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CSize]       = AgdaReal       #-}
{-# COMPILE GHC Show[CSize]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CSize]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CSize]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CIntPtr]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CIntPtr]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CIntPtr] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CIntPtr]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CIntPtr]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CIntPtr]         = AgdaIx         #-}
{-# COMPILE GHC Num[CIntPtr]        = AgdaNum        #-}
{-# COMPILE GHC Read[CIntPtr]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CIntPtr]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CIntPtr]       = AgdaReal       #-}
{-# COMPILE GHC Show[CIntPtr]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CIntPtr]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CIntPtr]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CUIntPtr]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CUIntPtr]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CUIntPtr] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CUIntPtr]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CUIntPtr]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CUIntPtr]         = AgdaIx         #-}
{-# COMPILE GHC Num[CUIntPtr]        = AgdaNum        #-}
{-# COMPILE GHC Read[CUIntPtr]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CUIntPtr]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CUIntPtr]       = AgdaReal       #-}
{-# COMPILE GHC Show[CUIntPtr]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CUIntPtr]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CUIntPtr]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CPtrdiff]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CPtrdiff]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CPtrdiff] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CPtrdiff]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CPtrdiff]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CPtrdiff]         = AgdaIx         #-}
{-# COMPILE GHC Num[CPtrdiff]        = AgdaNum        #-}
{-# COMPILE GHC Read[CPtrdiff]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CPtrdiff]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CPtrdiff]       = AgdaReal       #-}
{-# COMPILE GHC Show[CPtrdiff]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CPtrdiff]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CPtrdiff]        = AgdaOrd        #-}


{-# COMPILE GHC Storable[CClock]   = AgdaStorable   #-}
{-# COMPILE GHC Enum[CClock]       = AgdaEnum       #-}
{-# COMPILE GHC Num[CClock]        = AgdaNum        #-}
{-# COMPILE GHC Read[CClock]       = AgdaRead       #-}
{-# COMPILE GHC Real[CClock]       = AgdaReal       #-}
{-# COMPILE GHC Show[CClock]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CClock]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CClock]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CTime]   = AgdaStorable   #-}
{-# COMPILE GHC Enum[CTime]       = AgdaEnum       #-}
{-# COMPILE GHC Num[CTime]        = AgdaNum        #-}
{-# COMPILE GHC Read[CTime]       = AgdaRead       #-}
{-# COMPILE GHC Real[CTime]       = AgdaReal       #-}
{-# COMPILE GHC Show[CTime]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CTime]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CTime]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CUSeconds]   = AgdaStorable   #-}
{-# COMPILE GHC Enum[CUSeconds]       = AgdaEnum       #-}
{-# COMPILE GHC Num[CUSeconds]        = AgdaNum        #-}
{-# COMPILE GHC Read[CUSeconds]       = AgdaRead       #-}
{-# COMPILE GHC Real[CUSeconds]       = AgdaReal       #-}
{-# COMPILE GHC Show[CUSeconds]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CUSeconds]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CUSeconds]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CSUSeconds]   = AgdaStorable   #-}
{-# COMPILE GHC Enum[CSUSeconds]       = AgdaEnum       #-}
{-# COMPILE GHC Num[CSUSeconds]        = AgdaNum        #-}
{-# COMPILE GHC Read[CSUSeconds]       = AgdaRead       #-}
{-# COMPILE GHC Real[CSUSeconds]       = AgdaReal       #-}
{-# COMPILE GHC Show[CSUSeconds]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CSUSeconds]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CSUSeconds]        = AgdaOrd        #-}


{-# COMPILE GHC Storable[CBool]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CBool]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CBool] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CBool]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CBool]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CBool]         = AgdaIx         #-}
{-# COMPILE GHC Num[CBool]        = AgdaNum        #-}
{-# COMPILE GHC Read[CBool]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CBool]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CBool]       = AgdaReal       #-}
{-# COMPILE GHC Show[CBool]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CBool]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CBool]        = AgdaOrd        #-}

{-# COMPILE GHC Storable[CSigAtomic]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[CSigAtomic]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[CSigAtomic] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[CSigAtomic]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[CSigAtomic]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[CSigAtomic]         = AgdaIx         #-}
{-# COMPILE GHC Num[CSigAtomic]        = AgdaNum        #-}
{-# COMPILE GHC Read[CSigAtomic]       = AgdaRead       #-}
{-# COMPILE GHC Integral[CSigAtomic]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[CSigAtomic]       = AgdaReal       #-}
{-# COMPILE GHC Show[CSigAtomic]       = AgdaShow       #-}
{-# COMPILE GHC Eq[CSigAtomic]         = AgdaEq         #-}
{-# COMPILE GHC Ord[CSigAtomic]        = AgdaOrd        #-}
