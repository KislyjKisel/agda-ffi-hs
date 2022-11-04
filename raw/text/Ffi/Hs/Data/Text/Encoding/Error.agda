{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Text.Encoding.Error where

open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.List      using (List)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Eq; Show; Exception)
open import Ffi.Hs.Control.DeepSeq using (NFData)
open import Ffi.Hs.Data.Word       using (Word8)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Text.Encoding.Error
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData (AgdaNFData))
#-}

private
    variable
        aℓ bℓ : Level
        A B : Set aℓ


data UnicodeException : Set where
    DecodeError : List Char → Maybe Word8 → UnicodeException
    EncodeError : List Char → Maybe Char  → UnicodeException

{-# COMPILE GHC UnicodeException = data Data.Text.Encoding.Error.UnicodeException
    ( Data.Text.Encoding.Error.DecodeError
    | Data.Text.Encoding.Error.EncodeError
    ) #-}

postulate
    Eq[UnicodeException]        : Eq UnicodeException
    Show[UnicodeException]      : Show UnicodeException
    Exception[UnicodeException] : Exception UnicodeException
    NFData[UnicodeException]    : NFData UnicodeException

{-# COMPILE GHC Eq[UnicodeException]        = AgdaEq        #-}
{-# COMPILE GHC Show[UnicodeException]      = AgdaShow      #-}
{-# COMPILE GHC Exception[UnicodeException] = AgdaException #-}
{-# COMPILE GHC NFData[UnicodeException]    = AgdaNFData    #-}


OnError : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)
OnError A B = List Char → Maybe A → Maybe B

OnDecodeError : Set
OnDecodeError = OnError Word8 Char

OnEncodeError : Set
OnEncodeError = OnError Char Word8


postulate
    lenientDecode : OnDecodeError
    strictDecode  : OnDecodeError
    ignore        : OnError A B
    replace       : B → OnError A B

{-# COMPILE GHC lenientDecode =                Data.Text.Encoding.Error.lenientDecode #-}
{-# COMPILE GHC strictDecode  =                Data.Text.Encoding.Error.strictDecode  #-}
{-# COMPILE GHC ignore        = \ aℓ a bℓ b -> Data.Text.Encoding.Error.ignore        #-}
{-# COMPILE GHC replace       = \ bℓ b aℓ a -> Data.Text.Encoding.Error.replace       #-}
