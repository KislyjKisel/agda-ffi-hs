{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.ByteString.Lazy.Internal where

open import Ffi.Hs.Data.ByteString.Internal using (StrictByteString)

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.List      using (List)
open import Agda.Primitive         using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.DeepSeq using (NFData)
open import Ffi.Hs.Data.Int        using (Int)
open import Ffi.Hs.Data.Word       using (Word8)
open import Ffi.Hs.GHC.IsList      using (IsList)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.ByteString.Lazy.Internal
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.GHC.IsList (AgdaIsList(AgdaIsList))
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData(AgdaNFData))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


data ByteString : Set where
    Empty : ByteString
    Chunk : StrictByteString → ByteString

{-# COMPILE GHC ByteString = data Data.ByteString.Internal.ByteString
    ( Data.ByteString.Internal.Empty
    | Data.ByteString.Internal.Chunk
    ) #-}

postulate
    IsList[ByteString]    : IsList ByteString
    Eq[ByteString]        : Eq ByteString
    Data[ByteString]      : Data ByteString
    Ord[ByteString]       : Ord ByteString
    Read[ByteString]      : Read ByteString
    Show[ByteString]      : Show ByteString
    IsString[ByteString]  : IsString ByteString
    Semigroup[ByteString] : Semigroup ByteString
    Monoid[ByteString]    : Monoid ByteString
    NFData[ByteString]    : NFData ByteString

{-# COMPILE GHC IsList[ByteString]    = AgdaIsList    #-}
{-# COMPILE GHC Eq[ByteString]        = AgdaEq        #-}
{-# COMPILE GHC Data[ByteString]      = AgdaData      #-}
{-# COMPILE GHC Ord[ByteString]       = AgdaOrd       #-}
{-# COMPILE GHC Read[ByteString]      = AgdaRead      #-}
{-# COMPILE GHC Show[ByteString]      = AgdaShow      #-}
{-# COMPILE GHC IsString[ByteString]  = AgdaIsString  #-}
{-# COMPILE GHC Semigroup[ByteString] = AgdaSemigroup #-}
{-# COMPILE GHC Monoid[ByteString]    = AgdaMonoid    #-}
{-# COMPILE GHC NFData[ByteString]    = AgdaNFData    #-}

LazyByteString : Set
LazyByteString = ByteString

postulate
    chunk       : StrictByteString → ByteString → ByteString
    foldrChunks : (StrictByteString → A → A) → A → ByteString → A
    foldlChunks : (A → StrictByteString → A) → A → ByteString → A

    invariant      : ByteString → Bool
    checkInvariant : ByteString → ByteString

    defaultChunkSize : Int
    smallChunkSize   : Int
    chunkOverhead    : Int

    packBytes   : List Word8 → ByteString
    packChars   : List Char → ByteString
    unpackBytes : ByteString → List Word8
    unpackChars : ByteString → List Char

    fromStrict : StrictByteString → LazyByteString
    toStrict   : LazyByteString → StrictByteString

{-# COMPILE GHC chunk       =           Data.ByteString.Internal.chunk       #-}
{-# COMPILE GHC foldrChunks = \ aℓ a -> Data.ByteString.Internal.foldrChunks #-}
{-# COMPILE GHC foldlChunks = \ aℓ a -> Data.ByteString.Internal.foldlChunks #-}

{-# COMPILE GHC invariant      = Data.ByteString.Internal.invariant      #-}
{-# COMPILE GHC checkInvariant = Data.ByteString.Internal.checkInvariant #-}

{-# COMPILE GHC defaultChunkSize = Data.ByteString.Internal.defaultChunkSize #-}
{-# COMPILE GHC smallChunkSize   = Data.ByteString.Internal.smallChunkSize   #-}
{-# COMPILE GHC chunkOverhead    = Data.ByteString.Internal.chunkOverhead    #-}

{-# COMPILE GHC packBytes   = Data.ByteString.Internal.packBytes   #-}
{-# COMPILE GHC packChars   = Data.ByteString.Internal.packChars   #-}
{-# COMPILE GHC unpackBytes = Data.ByteString.Internal.unpackBytes #-}
{-# COMPILE GHC unpackChars = Data.ByteString.Internal.unpackChars #-}

{-# COMPILE GHC fromStrict = Data.ByteString.Internal.fromStrict #-}
{-# COMPILE GHC toStrict   = Data.ByteString.Internal.toStrict   #-}
