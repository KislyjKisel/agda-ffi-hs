{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.IO where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.Char        using (Char)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq)
open import Ffi.Hs.-base.Float       using (Float; Double)
open import Ffi.Hs.-base.Unit        using (⊤)
open import Ffi.Hs.Data.Int          using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Word         using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.Foreign.Ptr       using (Ptr; FunPtr)
open import Ffi.Hs.Foreign.StablePtr using (StablePtr)
open import Ffi.Hs.System.IO         using (Handle; IO)

open import Ffi.Hs.Data.Array.MArray public

{-# FOREIGN GHC
import qualified Data.Array.IO
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Array.MArray (AgdaMArray)
#-}

private
    variable
        aℓ : Level
        I E A : Set aℓ

postulate
    IOArray : Set aℓ → Set aℓ → Set aℓ

    MArray[IOArray,E,IO] : MArray IOArray E IO
    Eq[IOArray[I,E]]     : Eq (IOArray I E)

{-# FOREIGN GHC AgdaIOArray aℓ = Data.Array.IO.IOArray #-}
{-# COMPILE GHC IOArray = type(1) AgdaIOArray #-}

{-# COMPILE GHC MArray[IOArray,E,IO] = \ aℓ e   -> AgdaMArray #-}
{-# COMPILE GHC Eq[IOArray[I,E]]     = \ aℓ i e -> AgdaEq     #-}

postulate
    IOUArray : Set aℓ → Set aℓ → Set aℓ

    MArray[IOUArray,Int,IO]          : MArray IOUArray Int IO
    MArray[IOUArray,Int8,IO]         : MArray IOUArray Int8 IO
    MArray[IOUArray,Int16,IO]        : MArray IOUArray Int16 IO
    MArray[IOUArray,Int32,IO]        : MArray IOUArray Int32 IO
    MArray[IOUArray,Int64,IO]        : MArray IOUArray Int64 IO
    MArray[IOUArray,Word,IO]         : MArray IOUArray Word IO
    MArray[IOUArray,Word8,IO]        : MArray IOUArray Word8 IO
    MArray[IOUArray,Word16,IO]       : MArray IOUArray Word16 IO
    MArray[IOUArray,Word32,IO]       : MArray IOUArray Word32 IO
    MArray[IOUArray,Word64,IO]       : MArray IOUArray Word64 IO
    MArray[IOUArray,Bool,IO]         : MArray IOUArray Bool IO
    MArray[IOUArray,Char,IO]         : MArray IOUArray Char IO
    MArray[IOUArray,Float,IO]        : MArray IOUArray Float IO
    MArray[IOUArray,Double,IO]       : MArray IOUArray Double IO
    MArray[IOUArray,Ptr[A],IO]       : MArray IOUArray (Ptr A) IO
    MArray[IOUArray,FunPtr[A],IO]    : MArray IOUArray (FunPtr A) IO
    MArray[IOUArray,StablePtr[A],IO] : MArray IOUArray (StablePtr A) IO
    Eq[IOUArray[I,E]]                : Eq (IOUArray I E)

{-# FOREIGN GHC AgdaIOUArray aℓ = Data.Array.IO.IOUArray #-}
{-# COMPILE GHC IOUArray = type(1) AgdaIOUArray #-}

{-# COMPILE GHC MArray[IOUArray,Int,IO]          =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Int8,IO]         =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Int16,IO]        =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Int32,IO]        =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Int64,IO]        =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Word,IO]         =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Word8,IO]        =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Word16,IO]       =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Word32,IO]       =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Word64,IO]       =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Bool,IO]         =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Char,IO]         =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Float,IO]        =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Double,IO]       =             AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,Ptr[A],IO]       = \ aℓ a   -> AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,FunPtr[A],IO]    = \ aℓ a   -> AgdaMArray #-}
{-# COMPILE GHC MArray[IOUArray,StablePtr[A],IO] = \ aℓ a   -> AgdaMArray #-}
{-# COMPILE GHC Eq[IOUArray[I,E]]                = \ aℓ i e -> AgdaEq     #-}

postulate
    hGetArray : Handle → IOUArray Int Word8 → Int → IO Int
    hPutArray : Handle → IOUArray Int Word8 → Int → IO (⊤ {lzero})

{-# COMPILE GHC hGetArray = Data.Array.IO.hGetArray #-}
{-# COMPILE GHC hPutArray = Data.Array.IO.hPutArray #-}
