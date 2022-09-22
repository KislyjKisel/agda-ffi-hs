{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.Byte where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Typeable; Data; Eq; Monoid; Semigroup; Show; Ord)
open import Ffi.Hs.GHC.Exts    using (ByteArray#; MutableByteArray#)
open import Ffi.Hs.GHC.IsList  using (IsList)

{-# FOREIGN GHC
import qualified Data.Array.Byte
import MAlonzo.Code.Ffi.Hs.GHC.IsList (AgdaIsList)
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        sℓ : Level
        S : Set sℓ

data ByteArray : Set where
    mkByteArray : ByteArray# → ByteArray

{-# COMPILE GHC ByteArray = data Data.Array.Byte.ByteArray (Data.Array.Byte.ByteArray) #-}

postulate
    Data[ByteArray]      : Data ByteArray
    Semigroup[ByteArray] : Semigroup ByteArray
    Monoid[ByteArray]    : Monoid ByteArray
    IsList[ByteArray]    : IsList ByteArray
    Show[ByteArray]      : Show ByteArray
    Eq[ByteArray]        : Eq ByteArray
    Ord[ByteArray]       : Ord ByteArray

{-# COMPILE GHC Data[ByteArray]      = AgdaData      #-}
{-# COMPILE GHC Semigroup[ByteArray] = AgdaSemigroup #-}
{-# COMPILE GHC Monoid[ByteArray]    = AgdaMonoid    #-}
{-# COMPILE GHC IsList[ByteArray]    = AgdaIsList    #-}
{-# COMPILE GHC Show[ByteArray]      = AgdaShow      #-}
{-# COMPILE GHC Eq[ByteArray]        = AgdaEq        #-}
{-# COMPILE GHC Ord[ByteArray]       = AgdaOrd       #-}

data MutableByteArray (S : Set sℓ) : Set sℓ where
    mkMutableByteArray : MutableByteArray# S → MutableByteArray S

{-# COMPILE GHC MutableByteArray = data Data.Array.Byte.MutableByteArray (Data.Array.Byte.MutableByteArray) #-}

postulate
    Data[MutableByteArray[S]] : ⦃ Typeable S ⦄ → Data (MutableByteArray S)
    Eq[MutableByteArray[S]]   : Eq (MutableByteArray S)

{-# COMPILE GHC Data[MutableByteArray[S]] = \ sℓ s AgdaTypeable -> AgdaData #-}
{-# COMPILE GHC Eq[MutableByteArray[S]]   = \ sℓ s              -> AgdaEq   #-}
