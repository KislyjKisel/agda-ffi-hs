{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Binary.Put where

open import Agda.Builtin.Char            using (Char)
open import Agda.Builtin.List            using (List)
open import Agda.Builtin.Unit            using (⊤)
open import Agda.Primitive               using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Binary.Builder   using (Builder)
open import Ffi.Hs.Data.ByteString       using (StrictByteString)
open import Ffi.Hs.Data.ByteString.Lazy  using (LazyByteString)
open import Ffi.Hs.Data.ByteString.Short using (ShortByteString)
open import Ffi.Hs.Data.Int              using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Tuple            using (Tuple2)
open import Ffi.Hs.Data.Word             using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.GHC.Float             using (Float; Double)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Binary.Put
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


postulate
    PutM : Set aℓ → Set aℓ

    Monad[PutM]       : Monad {aℓ} PutM
    Functor[PutM]     : Functor {aℓ} PutM
    Applicative[PutM] : Applicative {aℓ} PutM

{-# FOREIGN GHC type AgdaPutM aℓ = Data.Binary.Put.PutM #-}
{-# COMPILE GHC PutM = type(1) AgdaPutM #-}

{-# COMPILE GHC Monad[PutM]       = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Functor[PutM]     = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Applicative[PutM] = \ aℓ -> AgdaApplicative #-}

Put : Set
Put = PutM ⊤

postulate
    Semigroup[Put] : Semigroup Put
    Monoid[Put]    : Monoid Put

{-# COMPILE GHC Semigroup[Put] = AgdaSemigroup #-}
{-# COMPILE GHC Monoid[Put]    = AgdaMonoid    #-}


postulate
    runPut             : Put → LazyByteString
    runPutM            : PutM A → Tuple2 A LazyByteString
    putBuilder         : Builder → Put
    execPut            : PutM A → Builder
    flush              : Put
    putWord8           : Word8 → Put
    putInt8            : Int8 → Put
    putByteString      : StrictByteString → Put
    putLazyByteString  : LazyByteString → Put
    putShortByteString : ShortByteString → Put
    putWord16be        : Word16 → Put
    putWord32be        : Word32 → Put
    putWord64be        : Word64 → Put
    putInt16be         : Int16 → Put
    putInt32be         : Int32 → Put
    putInt64be         : Int64 → Put
    putFloatbe         : Float → Put
    putDoublebe        : Double → Put
    putWord16le        : Word16 → Put
    putWord32le        : Word32 → Put
    putWord64le        : Word64 → Put
    putInt16le         : Int16 → Put
    putInt32le         : Int32 → Put
    putInt64le         : Int64 → Put
    putFloatle         : Float → Put
    putDoublele        : Double → Put
    putWordhost        : Word → Put
    putWord16host      : Word16 → Put
    putWord32host      : Word32 → Put
    putWord64host      : Word64 → Put
    putInthost         : Int → Put
    putInt16host       : Int16 → Put
    putInt32host       : Int32 → Put
    putInt64host       : Int64 → Put
    putFloathost       : Float → Put
    putDoublehost      : Double → Put
    putCharUtf8        : Char → Put
    putStringUtf8      : List Char → Put

{-# COMPILE GHC runPut             =           Data.Binary.Put.runPut             #-}
{-# COMPILE GHC runPutM            = \ aℓ a -> Data.Binary.Put.runPutM            #-}
{-# COMPILE GHC putBuilder         =           Data.Binary.Put.putBuilder         #-}
{-# COMPILE GHC execPut            = \ aℓ a -> Data.Binary.Put.execPut            #-}
{-# COMPILE GHC flush              =           Data.Binary.Put.flush              #-}
{-# COMPILE GHC putWord8           =           Data.Binary.Put.putWord8           #-}
{-# COMPILE GHC putInt8            =           Data.Binary.Put.putInt8            #-}
{-# COMPILE GHC putByteString      =           Data.Binary.Put.putByteString      #-}
{-# COMPILE GHC putLazyByteString  =           Data.Binary.Put.putLazyByteString  #-}
{-# COMPILE GHC putShortByteString =           Data.Binary.Put.putShortByteString #-}
{-# COMPILE GHC putWord16be        =           Data.Binary.Put.putWord16be        #-}
{-# COMPILE GHC putWord32be        =           Data.Binary.Put.putWord32be        #-}
{-# COMPILE GHC putWord64be        =           Data.Binary.Put.putWord64be        #-}
{-# COMPILE GHC putInt16be         =           Data.Binary.Put.putInt16be         #-}
{-# COMPILE GHC putInt32be         =           Data.Binary.Put.putInt32be         #-}
{-# COMPILE GHC putInt64be         =           Data.Binary.Put.putInt64be         #-}
{-# COMPILE GHC putFloatbe         =           Data.Binary.Put.putFloatbe         #-}
{-# COMPILE GHC putDoublebe        =           Data.Binary.Put.putDoublebe        #-}
{-# COMPILE GHC putWord16le        =           Data.Binary.Put.putWord16le        #-}
{-# COMPILE GHC putWord32le        =           Data.Binary.Put.putWord32le        #-}
{-# COMPILE GHC putWord64le        =           Data.Binary.Put.putWord64le        #-}
{-# COMPILE GHC putInt16le         =           Data.Binary.Put.putInt16le         #-}
{-# COMPILE GHC putInt32le         =           Data.Binary.Put.putInt32le         #-}
{-# COMPILE GHC putInt64le         =           Data.Binary.Put.putInt64le         #-}
{-# COMPILE GHC putFloatle         =           Data.Binary.Put.putFloatle         #-}
{-# COMPILE GHC putDoublele        =           Data.Binary.Put.putDoublele        #-}
{-# COMPILE GHC putWordhost        =           Data.Binary.Put.putWordhost        #-}
{-# COMPILE GHC putWord16host      =           Data.Binary.Put.putWord16host      #-}
{-# COMPILE GHC putWord32host      =           Data.Binary.Put.putWord32host      #-}
{-# COMPILE GHC putWord64host      =           Data.Binary.Put.putWord64host      #-}
{-# COMPILE GHC putInthost         =           Data.Binary.Put.putInthost         #-}
{-# COMPILE GHC putInt16host       =           Data.Binary.Put.putInt16host       #-}
{-# COMPILE GHC putInt32host       =           Data.Binary.Put.putInt32host       #-}
{-# COMPILE GHC putInt64host       =           Data.Binary.Put.putInt64host       #-}
{-# COMPILE GHC putFloathost       =           Data.Binary.Put.putFloathost       #-}
{-# COMPILE GHC putDoublehost      =           Data.Binary.Put.putDoublehost      #-}
{-# COMPILE GHC putCharUtf8        =           Data.Binary.Put.putCharUtf8        #-}
{-# COMPILE GHC putStringUtf8      =           Data.Binary.Put.putStringUtf8      #-}
