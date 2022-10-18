{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Binary.Builder where

open import Agda.Builtin.Char            using (Char)
open import Agda.Builtin.List            using (List)
open import Ffi.Hs.-base.Class           using (Semigroup; Monoid)
open import Ffi.Hs.Data.ByteString       using (StrictByteString)
open import Ffi.Hs.Data.ByteString.Lazy  using (LazyByteString)
open import Ffi.Hs.Data.ByteString.Short using (ShortByteString)
open import Ffi.Hs.Data.Int              using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Word             using (Word; Word8; Word16; Word32; Word64)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Binary.Builder
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}


postulate
    Builder : Set

    Semigroup[Builder] : Semigroup Builder
    Monoid[Builder]    : Monoid Builder

{-# COMPILE GHC Builder = type Data.Binary.Builder.Builder #-}

{-# COMPILE GHC Semigroup[Builder] = AgdaSemigroup #-}
{-# COMPILE GHC Monoid[Builder]    = AgdaMonoid    #-}


postulate
    toLazyByteString    : Builder → LazyByteString
    empty               : Builder
    singleton           : Word8 → Builder
    append              : Builder → Builder → Builder
    fromByteString      : StrictByteString → Builder
    fromLazyByteString  : LazyByteString → Builder
    fromShortByteString : ShortByteString → Builder
    flush               : Builder
    putWord16be         : Word16 → Builder
    putWord32be         : Word32 → Builder
    putWord64be         : Word64 → Builder
    putInt16be          : Int16 → Builder
    putInt32be          : Int32 → Builder
    putInt64be          : Int64 → Builder
    putWord16le         : Word16 → Builder
    putWord32le         : Word32 → Builder
    putWord64le         : Word64 → Builder
    putInt16le          : Int16 → Builder
    putInt32le          : Int32 → Builder
    putInt64le          : Int64 → Builder
    putWordhost         : Word → Builder
    putWord16host       : Word16 → Builder
    putWord32host       : Word32 → Builder
    putWord64host       : Word64 → Builder
    putInthost          : Int → Builder
    putInt16host        : Int16 → Builder
    putInt32host        : Int32 → Builder
    putInt64host        : Int64 → Builder
    putCharUtf8         : Char → Builder
    putStringUtf8       : List Char → Builder

{-# COMPILE GHC toLazyByteString    = Data.Binary.Builder.toLazyByteString    #-}
{-# COMPILE GHC empty               = Data.Binary.Builder.empty               #-}
{-# COMPILE GHC singleton           = Data.Binary.Builder.singleton           #-}
{-# COMPILE GHC append              = Data.Binary.Builder.append              #-}
{-# COMPILE GHC fromByteString      = Data.Binary.Builder.fromByteString      #-}
{-# COMPILE GHC fromLazyByteString  = Data.Binary.Builder.fromLazyByteString  #-}
{-# COMPILE GHC fromShortByteString = Data.Binary.Builder.fromShortByteString #-}
{-# COMPILE GHC flush               = Data.Binary.Builder.flush               #-}
{-# COMPILE GHC putWord16be         = Data.Binary.Builder.putWord16be         #-}
{-# COMPILE GHC putWord32be         = Data.Binary.Builder.putWord32be         #-}
{-# COMPILE GHC putWord64be         = Data.Binary.Builder.putWord64be         #-}
{-# COMPILE GHC putInt16be          = Data.Binary.Builder.putInt16be          #-}
{-# COMPILE GHC putInt32be          = Data.Binary.Builder.putInt32be          #-}
{-# COMPILE GHC putInt64be          = Data.Binary.Builder.putInt64be          #-}
{-# COMPILE GHC putWord16le         = Data.Binary.Builder.putWord16le         #-}
{-# COMPILE GHC putWord32le         = Data.Binary.Builder.putWord32le         #-}
{-# COMPILE GHC putWord64le         = Data.Binary.Builder.putWord64le         #-}
{-# COMPILE GHC putInt16le          = Data.Binary.Builder.putInt16le          #-}
{-# COMPILE GHC putInt32le          = Data.Binary.Builder.putInt32le          #-}
{-# COMPILE GHC putInt64le          = Data.Binary.Builder.putInt64le          #-}
{-# COMPILE GHC putWordhost         = Data.Binary.Builder.putWordhost         #-}
{-# COMPILE GHC putWord16host       = Data.Binary.Builder.putWord16host       #-}
{-# COMPILE GHC putWord32host       = Data.Binary.Builder.putWord32host       #-}
{-# COMPILE GHC putWord64host       = Data.Binary.Builder.putWord64host       #-}
{-# COMPILE GHC putInthost          = Data.Binary.Builder.putInthost          #-}
{-# COMPILE GHC putInt16host        = Data.Binary.Builder.putInt16host        #-}
{-# COMPILE GHC putInt32host        = Data.Binary.Builder.putInt32host        #-}
{-# COMPILE GHC putInt64host        = Data.Binary.Builder.putInt64host        #-}
{-# COMPILE GHC putCharUtf8         = Data.Binary.Builder.putCharUtf8         #-}
{-# COMPILE GHC putStringUtf8       = Data.Binary.Builder.putStringUtf8       #-}
