{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Binary.Get where

open import Agda.Builtin.Bool           using (Bool)
open import Agda.Builtin.Char           using (Char)
open import Agda.Builtin.List           using (List)
open import Agda.Builtin.Maybe          using (Maybe)
open import Agda.Builtin.Unit           using (⊤)
open import Agda.Primitive              using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.ByteString      using (StrictByteString)
open import Ffi.Hs.Data.ByteString.Lazy using (LazyByteString)
open import Ffi.Hs.Data.Either          using (Either)
open import Ffi.Hs.Data.Int             using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Tuple           using (Tuple3)
open import Ffi.Hs.Data.Word            using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.GHC.Float            using (Float; Double)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Binary.Get
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ


postulate
    Get : Set aℓ → Set aℓ

    Monad[Get]       : Monad {aℓ} Get
    Functor[Get]     : Functor {aℓ} Get
    MonadFail[Get]   : MonadFail {aℓ} Get
    Applicative[Get] : Applicative {aℓ} Get
    Alternative[Get] : Alternative {aℓ} Get
    MonadPlus[Get]   : MonadPlus {aℓ} Get

{-# FOREIGN GHC type AgdaGet aℓ = Data.Binary.Get.Get #-}
{-# COMPILE GHC Get = type(1) AgdaGet #-}

{-# COMPILE GHC Monad[Get]       = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Functor[Get]     = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC MonadFail[Get]   = \ aℓ -> AgdaMonadFail   #-}
{-# COMPILE GHC Applicative[Get] = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Alternative[Get] = \ aℓ -> AgdaAlternative #-}
{-# COMPILE GHC MonadPlus[Get]   = \ aℓ -> AgdaMonadPlus   #-}


ByteOffset : Set
ByteOffset = Int64

postulate
    runGet : Get A → LazyByteString → A
    runGetOrFail : Get A → LazyByteString → Either (Tuple3 LazyByteString ByteOffset (List Char)) (Tuple3 LazyByteString ByteOffset A)

{-# COMPILE GHC runGet       = \ aℓ a -> Data.Binary.Get.runGet       #-}
{-# COMPILE GHC runGetOrFail = \ aℓ a -> Data.Binary.Get.runGetOrFail #-}


data Decoder (A : Set aℓ) : Set aℓ where
    Fail    : StrictByteString → ByteOffset → List Char → Decoder A
    Partial : (Maybe StrictByteString → Decoder A) → Decoder A
    Done    : StrictByteString → ByteOffset → A → Decoder A

{-# FOREIGN GHC type AgdaDecoder aℓ = Data.Binary.Get.Decoder #-}
{-# COMPILE GHC Decoder = data(1) AgdaDecoder
    ( Data.Binary.Get.Fail
    | Data.Binary.Get.Partial
    | Data.Binary.Get.Done
    ) #-}

postulate
    runGetIncremental : Get A → Decoder A

{-# COMPILE GHC runGetIncremental = \ aℓ a -> Data.Binary.Get.runGetIncremental #-}


postulate
    pushChunk                  : Decoder A → StrictByteString → Decoder A
    pushChunks                 : Decoder A → LazyByteString → Decoder A
    pushEndOfInput             : Decoder A → Decoder A
    skip                       : Int → Get ⊤
    isEmpty                    : Get Bool
    bytesRead                  : Get Int64
    isolate                    : Int → Get A → Get A
    lookAhead                  : Get A → Get A
    lookAheadM                 : Get (Maybe A) → Get (Maybe A)
    lookAheadE                 : Get (Either A B) → Get (Either A B)
    label                      : List Char → Get A → Get A
    getByteString              : Int → Get StrictByteString
    getLazyByteString          : Int64 → Get LazyByteString
    getLazyByteStringNul       : Get LazyByteString
    getRemainingLazyByteString : Get LazyByteString
    getWord8                   : Get Word8
    getWord16be                : Get Word16
    getWord32be                : Get Word32
    getWord64be                : Get Word64
    getWord16le                : Get Word16
    getWord32le                : Get Word32
    getWord64le                : Get Word64
    getWordhost                : Get Word
    getWord16host              : Get Word16
    getWord32host              : Get Word32
    getWord64host              : Get Word64
    getInt8                    : Get Int8
    getInt16be                 : Get Int16
    getInt32be                 : Get Int32
    getInt64be                 : Get Int64
    getInt16le                 : Get Int16
    getInt32le                 : Get Int32
    getInt64le                 : Get Int64
    getInthost                 : Get Int
    getInt16host               : Get Int16
    getInt32host               : Get Int32
    getInt64host               : Get Int64
    getFloatbe                 : Get Float
    getFloatle                 : Get Float
    getFloathost               : Get Float
    getDoublebe                : Get Double
    getDoublele                : Get Double
    getDoublehost              : Get Double

{-# COMPILE GHC pushChunk                  = \ aℓ a      -> Data.Binary.Get.pushChunk                  #-}
{-# COMPILE GHC pushChunks                 = \ aℓ a      -> Data.Binary.Get.pushChunks                 #-}
{-# COMPILE GHC pushEndOfInput             = \ aℓ a      -> Data.Binary.Get.pushEndOfInput             #-}
{-# COMPILE GHC skip                       =                Data.Binary.Get.skip                       #-}
{-# COMPILE GHC isEmpty                    =                Data.Binary.Get.isEmpty                    #-}
{-# COMPILE GHC bytesRead                  =                Data.Binary.Get.bytesRead                  #-}
{-# COMPILE GHC isolate                    = \ aℓ a      -> Data.Binary.Get.isolate                    #-}
{-# COMPILE GHC lookAhead                  = \ aℓ a      -> Data.Binary.Get.lookAhead                  #-}
{-# COMPILE GHC lookAheadM                 = \ aℓ a      -> Data.Binary.Get.lookAheadM                 #-}
{-# COMPILE GHC lookAheadE                 = \ aℓ a bℓ b -> Data.Binary.Get.lookAheadE                 #-}
{-# COMPILE GHC label                      = \ aℓ a      -> Data.Binary.Get.label                      #-}
{-# COMPILE GHC getByteString              =                Data.Binary.Get.getByteString              #-}
{-# COMPILE GHC getLazyByteString          =                Data.Binary.Get.getLazyByteString          #-}
{-# COMPILE GHC getLazyByteStringNul       =                Data.Binary.Get.getLazyByteStringNul       #-}
{-# COMPILE GHC getRemainingLazyByteString =                Data.Binary.Get.getRemainingLazyByteString #-}
{-# COMPILE GHC getWord8                   =                Data.Binary.Get.getWord8                   #-}
{-# COMPILE GHC getWord16be                =                Data.Binary.Get.getWord16be                #-}
{-# COMPILE GHC getWord32be                =                Data.Binary.Get.getWord32be                #-}
{-# COMPILE GHC getWord64be                =                Data.Binary.Get.getWord64be                #-}
{-# COMPILE GHC getWord16le                =                Data.Binary.Get.getWord16le                #-}
{-# COMPILE GHC getWord32le                =                Data.Binary.Get.getWord32le                #-}
{-# COMPILE GHC getWord64le                =                Data.Binary.Get.getWord64le                #-}
{-# COMPILE GHC getWordhost                =                Data.Binary.Get.getWordhost                #-}
{-# COMPILE GHC getWord16host              =                Data.Binary.Get.getWord16host              #-}
{-# COMPILE GHC getWord32host              =                Data.Binary.Get.getWord32host              #-}
{-# COMPILE GHC getWord64host              =                Data.Binary.Get.getWord64host              #-}
{-# COMPILE GHC getInt8                    =                Data.Binary.Get.getInt8                    #-}
{-# COMPILE GHC getInt16be                 =                Data.Binary.Get.getInt16be                 #-}
{-# COMPILE GHC getInt32be                 =                Data.Binary.Get.getInt32be                 #-}
{-# COMPILE GHC getInt64be                 =                Data.Binary.Get.getInt64be                 #-}
{-# COMPILE GHC getInt16le                 =                Data.Binary.Get.getInt16le                 #-}
{-# COMPILE GHC getInt32le                 =                Data.Binary.Get.getInt32le                 #-}
{-# COMPILE GHC getInt64le                 =                Data.Binary.Get.getInt64le                 #-}
{-# COMPILE GHC getInthost                 =                Data.Binary.Get.getInthost                 #-}
{-# COMPILE GHC getInt16host               =                Data.Binary.Get.getInt16host               #-}
{-# COMPILE GHC getInt32host               =                Data.Binary.Get.getInt32host               #-}
{-# COMPILE GHC getInt64host               =                Data.Binary.Get.getInt64host               #-}
{-# COMPILE GHC getFloatbe                 =                Data.Binary.Get.getFloatbe                 #-}
{-# COMPILE GHC getFloatle                 =                Data.Binary.Get.getFloatle                 #-}
{-# COMPILE GHC getFloathost               =                Data.Binary.Get.getFloathost               #-}
{-# COMPILE GHC getDoublebe                =                Data.Binary.Get.getDoublebe                #-}
{-# COMPILE GHC getDoublele                =                Data.Binary.Get.getDoublele                #-}
{-# COMPILE GHC getDoublehost              =                Data.Binary.Get.getDoublehost              #-}
