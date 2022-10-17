{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Word where

open import Agda.Builtin.Bool          using (true; false)
open import Agda.Builtin.Int      as ℤ using ()
open import Agda.Builtin.Nat      as ℕ using ()
open import Agda.Builtin.Unit          using (⊤)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Empty         using (⊥)
open import Ffi.Hs.-base.Literals
open import Ffi.Hs.GHC.Num             using (fromInteger)

open import Ffi.Hs.GHC.Exts public
    using (Word; Word8; Word16; Word32; Word64)

{-# FOREIGN GHC
import qualified Data.Word
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

postulate
    byteSwap16 : Word16 → Word16
    byteSwap32 : Word32 → Word32
    byteSwap64 : Word64 → Word64

    bitReverse8  : Word8 → Word8
    bitReverse16 : Word16 → Word16
    bitReverse32 : Word32 → Word32
    bitReverse64 : Word64 → Word64

{-# COMPILE GHC byteSwap16 = Data.Word.byteSwap16 #-}
{-# COMPILE GHC byteSwap32 = Data.Word.byteSwap32 #-}
{-# COMPILE GHC byteSwap64 = Data.Word.byteSwap64 #-}

{-# COMPILE GHC bitReverse8  = Data.Word.bitReverse8  #-}
{-# COMPILE GHC bitReverse16 = Data.Word.bitReverse16 #-}
{-# COMPILE GHC bitReverse32 = Data.Word.bitReverse32 #-}
{-# COMPILE GHC bitReverse64 = Data.Word.bitReverse64 #-}

postulate
    Data[Word]       : Data Word
    Storable[Word]   : Storable Word
    Bits[Word]       : Bits Word
    FiniteBits[Word] : FiniteBits Word
    Bounded[Word]    : Bounded Word
    Enum[Word]       : Enum Word
    Ix[Word]         : Ix Word
    Num[Word]        : Num Word
    Read[Word]       : Read Word
    Integral[Word]   : Integral Word
    Real[Word]       : Real Word
    Show[Word]       : Show Word
    Eq[Word]         : Eq Word
    Ord[Word]        : Ord Word

    Data[Word8]       : Data Word8
    Storable[Word8]   : Storable Word8
    Bits[Word8]       : Bits Word8
    FiniteBits[Word8] : FiniteBits Word8
    Bounded[Word8]    : Bounded Word8
    Enum[Word8]       : Enum Word8
    Ix[Word8]         : Ix Word8
    Num[Word8]        : Num Word8
    Read[Word8]       : Read Word8
    Integral[Word8]   : Integral Word8
    Real[Word8]       : Real Word8
    Show[Word8]       : Show Word8
    Eq[Word8]         : Eq Word8
    Ord[Word8]        : Ord Word8

    Data[Word16]       : Data Word16
    Storable[Word16]   : Storable Word16
    Bits[Word16]       : Bits Word16
    FiniteBits[Word16] : FiniteBits Word16
    Bounded[Word16]    : Bounded Word16
    Enum[Word16]       : Enum Word16
    Ix[Word16]         : Ix Word16
    Num[Word16]        : Num Word16
    Read[Word16]       : Read Word16
    Integral[Word16]   : Integral Word16
    Real[Word16]       : Real Word16
    Show[Word16]       : Show Word16
    Eq[Word16]         : Eq Word16
    Ord[Word16]        : Ord Word16

    Data[Word32]       : Data Word32
    Storable[Word32]   : Storable Word32
    Bits[Word32]       : Bits Word32
    FiniteBits[Word32] : FiniteBits Word32
    Bounded[Word32]    : Bounded Word32
    Enum[Word32]       : Enum Word32
    Ix[Word32]         : Ix Word32
    Num[Word32]        : Num Word32
    Read[Word32]       : Read Word32
    Integral[Word32]   : Integral Word32
    Real[Word32]       : Real Word32
    Show[Word32]       : Show Word32
    Eq[Word32]         : Eq Word32
    Ord[Word32]        : Ord Word32

    Data[Word64]       : Data Word64
    Storable[Word64]   : Storable Word64
    Bits[Word64]       : Bits Word64
    FiniteBits[Word64] : FiniteBits Word64
    Bounded[Word64]    : Bounded Word64
    Enum[Word64]       : Enum Word64
    Ix[Word64]         : Ix Word64
    Num[Word64]        : Num Word64
    Read[Word64]       : Read Word64
    Integral[Word64]   : Integral Word64
    Real[Word64]       : Real Word64
    Show[Word64]       : Show Word64
    Eq[Word64]         : Eq Word64
    Ord[Word64]        : Ord Word64

{-# COMPILE GHC Data[Word]       = AgdaData       #-}
{-# COMPILE GHC Storable[Word]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Word]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Word] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Word]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Word]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Word]         = AgdaIx         #-}
{-# COMPILE GHC Num[Word]        = AgdaNum        #-}
{-# COMPILE GHC Read[Word]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Word]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Word]       = AgdaReal       #-}
{-# COMPILE GHC Show[Word]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Word]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Word]        = AgdaOrd        #-}

{-# COMPILE GHC Data[Word8]       = AgdaData       #-}
{-# COMPILE GHC Storable[Word8]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Word8]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Word8] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Word8]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Word8]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Word8]         = AgdaIx         #-}
{-# COMPILE GHC Num[Word8]        = AgdaNum        #-}
{-# COMPILE GHC Read[Word8]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Word8]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Word8]       = AgdaReal       #-}
{-# COMPILE GHC Show[Word8]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Word8]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Word8]        = AgdaOrd        #-}

{-# COMPILE GHC Data[Word16]       = AgdaData       #-}
{-# COMPILE GHC Storable[Word16]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Word16]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Word16] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Word16]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Word16]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Word16]         = AgdaIx         #-}
{-# COMPILE GHC Num[Word16]        = AgdaNum        #-}
{-# COMPILE GHC Read[Word16]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Word16]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Word16]       = AgdaReal       #-}
{-# COMPILE GHC Show[Word16]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Word16]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Word16]        = AgdaOrd        #-}

{-# COMPILE GHC Data[Word32]       = AgdaData       #-}
{-# COMPILE GHC Storable[Word32]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Word32]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Word32] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Word32]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Word32]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Word32]         = AgdaIx         #-}
{-# COMPILE GHC Num[Word32]        = AgdaNum        #-}
{-# COMPILE GHC Read[Word32]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Word32]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Word32]       = AgdaReal       #-}
{-# COMPILE GHC Show[Word32]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Word32]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Word32]        = AgdaOrd        #-}

{-# COMPILE GHC Data[Word64]       = AgdaData       #-}
{-# COMPILE GHC Storable[Word64]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[Word64]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Word64] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Word64]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Word64]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[Word64]         = AgdaIx         #-}
{-# COMPILE GHC Num[Word64]        = AgdaNum        #-}
{-# COMPILE GHC Read[Word64]       = AgdaRead       #-}
{-# COMPILE GHC Integral[Word64]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[Word64]       = AgdaReal       #-}
{-# COMPILE GHC Show[Word64]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Word64]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Word64]        = AgdaOrd        #-}


Lit-FromNat[Word] : Lit-FromNat Word
Lit-FromNat[Word] .Lit-ConstrainNat s with s ℕ.< 1073741824
... | false = ⊥
... | true  = ⊤
Lit-FromNat[Word] .Lit-fromNat s = fromInteger ⦃ Num[Word] ⦄ (ℤ.pos s)

Lit-FromNat[Word8] : Lit-FromNat Word8
Lit-FromNat[Word8] .Lit-ConstrainNat s with s ℕ.< 256
... | false = ⊥
... | true  = ⊤
Lit-FromNat[Word8] .Lit-fromNat s = fromInteger ⦃ Num[Word8] ⦄ (ℤ.pos s)

Lit-FromNat[Word16] : Lit-FromNat Word16
Lit-FromNat[Word16] .Lit-ConstrainNat s with s ℕ.< 65536
... | false = ⊥
... | true  = ⊤
Lit-FromNat[Word16] .Lit-fromNat s = fromInteger ⦃ Num[Word16] ⦄ (ℤ.pos s)

Lit-FromNat[Word32] : Lit-FromNat Word32
Lit-FromNat[Word32] .Lit-ConstrainNat s with s ℕ.< 4294967296
... | false = ⊥
... | true  = ⊤
Lit-FromNat[Word32] .Lit-fromNat s = fromInteger ⦃ Num[Word32] ⦄ (ℤ.pos s)

Lit-FromNat[Word64] : Lit-FromNat Word64
Lit-FromNat[Word64] .Lit-ConstrainNat s with s ℕ.< 18446744073709551616
... | false = ⊥
... | true  = ⊤
Lit-FromNat[Word64] .Lit-fromNat s = fromInteger ⦃ Num[Word64] ⦄ (ℤ.pos s)
