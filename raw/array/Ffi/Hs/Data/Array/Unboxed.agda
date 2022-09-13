{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.Unboxed where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.Char        using (Char)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq; Ord; Read; Show)
open import Ffi.Hs.-base.Float       using (Float; Double)
open import Ffi.Hs.Data.Int          using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Word         using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.Foreign.Ptr       using (Ptr; FunPtr)
open import Ffi.Hs.Foreign.StablePtr using (StablePtr)

open import Ffi.Hs.Data.Array.IArray public

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Array.Unboxed
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Array.IArray (AgdaIArray)
#-}

private
    variable
        iℓ eℓ aℓ : Level
        I : Set iℓ
        E : Set eℓ
        A : Set aℓ

postulate
    UArray : Set iℓ → Set eℓ → Set (iℓ ⊔ eℓ)

    IArray[UArray,Bool]         : IArray {iℓ} UArray Bool
    IArray[UArray,Char]         : IArray {iℓ} UArray Char
    IArray[UArray,Float]        : IArray {iℓ} UArray Float
    IArray[UArray,Double]       : IArray {iℓ} UArray Double
    IArray[UArray,Int]          : IArray {iℓ} UArray Int
    IArray[UArray,Int8]         : IArray {iℓ} UArray Int8
    IArray[UArray,Int16]        : IArray {iℓ} UArray Int16
    IArray[UArray,Int32]        : IArray {iℓ} UArray Int32
    IArray[UArray,Int64]        : IArray {iℓ} UArray Int64
    IArray[UArray,Word]         : IArray {iℓ} UArray Word
    IArray[UArray,Word8]        : IArray {iℓ} UArray Word8
    IArray[UArray,Word16]       : IArray {iℓ} UArray Word16
    IArray[UArray,Word32]       : IArray {iℓ} UArray Word32
    IArray[UArray,Word64]       : IArray {iℓ} UArray Word64
    IArray[UArray,StablePtr[A]] : IArray {iℓ} UArray (StablePtr A)
    IArray[UArray,Ptr[A]]       : IArray {iℓ} UArray (Ptr A)
    IArray[UArray,FunPtr[A]]    : IArray {iℓ} UArray (FunPtr A)

    Eq[UArray[I,E]]   : {I : Set iℓ} → ⦃ Ix I ⦄ → ⦃ Eq E ⦄ → ⦃ IArray {iℓ} UArray E ⦄ → Eq (UArray I E)
    Ord[UArray[I,E]]  : {I : Set iℓ} → ⦃ Ix I ⦄ → ⦃ Ord E ⦄ → ⦃ IArray {iℓ} UArray E ⦄ → Ord (UArray I E)
    Read[UArray[I,E]] : {I : Set iℓ} → ⦃ Ix I ⦄ → ⦃ Read I ⦄ → ⦃ Read E ⦄ → ⦃ IArray {iℓ} UArray E ⦄ → Read (UArray I E)
    Show[UArray[I,E]] : {I : Set iℓ} → ⦃ Ix I ⦄ → ⦃ Show I ⦄ → ⦃ Show E ⦄ → ⦃ IArray {iℓ} UArray E ⦄ → Show (UArray I E)

{-# FOREIGN GHC type AgdaUArray iℓ eℓ = Data.Array.Unboxed.UArray #-}
{-# COMPILE GHC UArray = type(2) AgdaUArray #-}

{-# COMPILE GHC IArray[UArray,Bool]         = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Char]         = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Float]        = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Double]       = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Int]          = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Int8]         = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Int16]        = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Int32]        = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Int64]        = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Word]         = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Word8]        = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Word16]       = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Word32]       = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Word64]       = \ iℓ      -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,StablePtr[A]] = \ iℓ aℓ a -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,Ptr[A]]       = \ iℓ aℓ a -> AgdaIArray #-}
{-# COMPILE GHC IArray[UArray,FunPtr[A]]    = \ iℓ aℓ a -> AgdaIArray #-}

{-# COMPILE GHC Eq[UArray[I,E]]   = \ iℓ i eℓ e AgdaIx AgdaEq AgdaIArray            -> AgdaEq   #-}
{-# COMPILE GHC Ord[UArray[I,E]]  = \ iℓ i eℓ e AgdaIx AgdaOrd AgdaIArray           -> AgdaOrd  #-}
{-# COMPILE GHC Read[UArray[I,E]] = \ iℓ i eℓ e AgdaIx AgdaRead AgdaRead AgdaIArray -> AgdaRead #-}
{-# COMPILE GHC Show[UArray[I,E]] = \ iℓ i eℓ e AgdaIx AgdaShow AgdaShow AgdaIArray -> AgdaShow #-}
