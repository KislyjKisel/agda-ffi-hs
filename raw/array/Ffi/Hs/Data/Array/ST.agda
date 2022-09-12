{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.ST where

open import Agda.Builtin.Bool            using (Bool)
open import Agda.Builtin.Char            using (Char)
open import Agda.Primitive
open import Ffi.Hs.-base.Class           using (Eq)
open import Ffi.Hs.-base.Float           using (Float; Double)
open import Ffi.Hs.Control.Monad.ST      using () renaming (ST to StrictST)
open import Ffi.Hs.Control.Monad.ST.Lazy using () renaming (ST to LazyST)
open import Ffi.Hs.Data.Array.Base       using (Array)
open import Ffi.Hs.Data.Array.Unboxed    using (UArray)
open import Ffi.Hs.Data.Int              using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Word             using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.Foreign.Ptr           using (Ptr; FunPtr)
open import Ffi.Hs.Foreign.StablePtr     using (StablePtr)

open import Ffi.Hs.Data.Array.MArray public

{-# FOREIGN GHC
import qualified Data.Array.ST
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Array.MArray (AgdaMArray)
#-}

private
    variable
        aℓ : Level
        I E A : Set aℓ
        S : Set

postulate
    STArray : Set → Set aℓ → Set aℓ → Set aℓ

    MArray[STArray[S],E,SST[S]] : MArray (STArray S) E (StrictST S)
    MArray[STArray[S],E,LST[S]] : MArray (STArray S) E (LazyST S)
    Eq[STArray[S,I,E]]          : Eq (STArray S I E)

    runSTArray : (∀{S} → StrictST S (STArray S I E)) → Array I E

{-# FOREIGN GHC AgdaSTArray aℓ = Data.Array.ST.STArray #-}
{-# COMPILE GHC STArray = type(1) AgdaSTArray #-}

{-# COMPILE GHC MArray[STArray[S],E,SST[S]] = \ s aℓ e   -> AgdaMArray #-}
{-# COMPILE GHC MArray[STArray[S],E,LST[S]] = \ s aℓ e   -> AgdaMArray #-}
{-# COMPILE GHC Eq[STArray[S,I,E]]          = \ s aℓ i e -> AgdaEq     #-}

{-# COMPILE GHC runSTArray = \ aℓ i e f -> Data.Array.ST.runSTArray (f ()) #-}

postulate
    STUArray : Set → Set aℓ → Set aℓ → Set aℓ

    MArray[STUArray[S],Int,ST[S]]          : MArray (STUArray S) Int (StrictST S)
    MArray[STUArray[S],Int8,ST[S]]         : MArray (STUArray S) Int8 (StrictST S)
    MArray[STUArray[S],Int16,ST[S]]        : MArray (STUArray S) Int16 (StrictST S)
    MArray[STUArray[S],Int32,ST[S]]        : MArray (STUArray S) Int32 (StrictST S)
    MArray[STUArray[S],Int64,ST[S]]        : MArray (STUArray S) Int64 (StrictST S)
    MArray[STUArray[S],Word,ST[S]]         : MArray (STUArray S) Word (StrictST S)
    MArray[STUArray[S],Word8,ST[S]]        : MArray (STUArray S) Word8 (StrictST S)
    MArray[STUArray[S],Word16,ST[S]]       : MArray (STUArray S) Word16 (StrictST S)
    MArray[STUArray[S],Word32,ST[S]]       : MArray (STUArray S) Word32 (StrictST S)
    MArray[STUArray[S],Word64,ST[S]]       : MArray (STUArray S) Word64 (StrictST S)
    MArray[STUArray[S],Bool,ST[S]]         : MArray (STUArray S) Bool (StrictST S)
    MArray[STUArray[S],Char,ST[S]]         : MArray (STUArray S) Char (StrictST S)
    MArray[STUArray[S],Float,ST[S]]        : MArray (STUArray S) Float (StrictST S)
    MArray[STUArray[S],Double,ST[S]]       : MArray (STUArray S) Double (StrictST S)
    MArray[STUArray[S],StablePtr[A],ST[S]] : MArray (STUArray S) (StablePtr A) (StrictST S)
    MArray[STUArray[S],FunPtr[A],ST[S]]    : MArray (STUArray S) (FunPtr A) (StrictST S)
    MArray[STUArray[S],Ptr[A],ST[S]]       : MArray (STUArray S) (Ptr A) (StrictST S)
    Eq[STUArray[S,I,E]]                    : Eq (STUArray S I E)

    runSTUArray : (∀{S} → StrictST S (STUArray S I E)) → UArray I E

{-# FOREIGN GHC AgdaSTUArray aℓ = Data.Array.ST.STUArray #-}
{-# COMPILE GHC STUArray = type(1) AgdaSTUArray #-}

{-# COMPILE GHC MArray[STUArray[S],Int,ST[S]]          = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Int8,ST[S]]         = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Int16,ST[S]]        = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Int32,ST[S]]        = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Int64,ST[S]]        = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Word,ST[S]]         = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Word8,ST[S]]        = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Word16,ST[S]]       = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Word32,ST[S]]       = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Word64,ST[S]]       = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Bool,ST[S]]         = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Char,ST[S]]         = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Float,ST[S]]        = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Double,ST[S]]       = \ s        -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],StablePtr[A],ST[S]] = \ s aℓ a   -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],FunPtr[A],ST[S]]    = \ s aℓ a   -> AgdaMArray #-}
{-# COMPILE GHC MArray[STUArray[S],Ptr[A],ST[S]]       = \ s aℓ a   -> AgdaMArray #-}
{-# COMPILE GHC Eq[STUArray[S,I,E]]                    = \ s aℓ i e -> AgdaEq     #-}

{-# COMPILE GHC runSTUArray = \ aℓ i e f -> Data.Array.ST.runSTUArray (f ()) #-}
