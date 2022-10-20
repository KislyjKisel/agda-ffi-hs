{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Hashable where

open import Agda.Builtin.Bool            using (Bool)
open import Agda.Builtin.Char            using (Char)
open import Agda.Builtin.Int             using () renaming (Int to Integer)
open import Agda.Builtin.List            using (List)
open import Agda.Builtin.Maybe           using (Maybe)
open import Agda.Builtin.Unit            using (⊤)
open import Agda.Primitive               using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.Concurrent    using (ThreadId)
open import Ffi.Hs.Control.DeepSeq       using (NFData)
open import Ffi.Hs.Data.ByteString       using (ByteString)
open import Ffi.Hs.Data.Functor.Const    using (Const)
open import Ffi.Hs.Data.Functor.Identity using (Identity)
open import Ffi.Hs.Data.Int              using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Ord              using (Ordering)
open import Ffi.Hs.Data.Unique           using (Unique)
open import Ffi.Hs.Data.Version          using (Version)
open import Ffi.Hs.Data.Void             using (Void)
open import Ffi.Hs.Data.Word             using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.Foreign.Ptr           using (Ptr; FunPtr; IntPtr; WordPtr)
open import Ffi.Hs.GHC.Fingerprint.Type  using (Fingerprint)
open import Ffi.Hs.GHC.Float             using (Float; Double)
open import Ffi.Hs.Numeric.Natural       using (Natural)
-- open import Ffi.Hs.Data.ByteString.Short using (ShortByteString)
open import Agda.Builtin.IO              using (IO)
open import Agda.Builtin.String          using () renaming (String to Text)
open import Ffi.Hs.Data.ByteString.Lazy  using (LazyByteString)
open import Ffi.Hs.Data.Complex          using (Complex)
open import Ffi.Hs.Data.Either           using (Either)
open import Ffi.Hs.Data.Fixed            using (Fixed)
open import Ffi.Hs.Data.List.NonEmpty    using (NonEmpty)
open import Ffi.Hs.Data.Proxy            using (Proxy)
open import Ffi.Hs.Data.Ratio            using (Ratio)
open import Ffi.Hs.Data.Semigroup        using (Min; Max; First; Last; Arg)
open import Ffi.Hs.Data.Tuple            using (Tuple2; Tuple3; Tuple4; Tuple5)
open import Ffi.Hs.GHC.Exts              using (ByteArray#)
open import Ffi.Hs.System.Mem.StableName using (StableName)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Hashable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData(AgdaNFData))
#-}

private
    variable
        aℓ : Level
        A B C D E : Set aℓ
        F : Set aℓ → Set aℓ


postulate
    Hashable : Set aℓ → Set aℓ

    hashWithSalt : ⦃ Hashable A ⦄ → Int → A → Int
    hash         : ⦃ Hashable A ⦄ → A → Int

    Hashable[Bool]            : Hashable Bool
    Hashable[Char]            : Hashable Char
    Hashable[Double]          : Hashable Double
    Hashable[Float]           : Hashable Float
    Hashable[Int]             : Hashable Int
    Hashable[Int8]            : Hashable Int8
    Hashable[Int16]           : Hashable Int16
    Hashable[Int32]           : Hashable Int32
    Hashable[Int64]           : Hashable Int64
    Hashable[Word]            : Hashable Word
    Hashable[Word8]           : Hashable Word8
    Hashable[Word16]          : Hashable Word16
    Hashable[Word32]          : Hashable Word32
    Hashable[Word64]          : Hashable Word64
    Hashable[Integer]         : Hashable Integer
    Hashable[Natural]         : Hashable Natural
    Hashable[Ordering]        : Hashable Ordering
    Hashable[⊤]               : Hashable ⊤
    Hashable[Void]            : Hashable Void
    Hashable[Unique]          : Hashable Unique
    Hashable[Version]         : Hashable Version
    Hashable[ThreadId]        : Hashable ThreadId
    Hashable[WordPtr]         : Hashable WordPtr
    Hashable[IntPtr]          : Hashable IntPtr
    Hashable[Fingerprint]     : Hashable Fingerprint
    -- Hashable[ShortByteString] : Hashable ShortByteString
    Hashable[LazyByteString]  : Hashable LazyByteString
    Hashable[ByteString]      : Hashable ByteString
    Hashable[Text]            : Hashable Text

    Hashable[List[A]]           : ⦃ Hashable A ⦄ → Hashable (List A)
    Hashable[Maybe[A]]          : ⦃ Hashable A ⦄ → Hashable (Maybe A)
    Hashable[Ratio[A]]          : ⦃ Hashable A ⦄ → Hashable (Ratio A)
    Hashable[Ptr[A]]            : Hashable (Ptr A)
    Hashable[FunPtr[A]]         : Hashable (FunPtr A)
    Hashable[Complex[A]]        : ⦃ Hashable A ⦄ → Hashable (Complex A)
    Hashable[Fixed[A]]          : Hashable (Fixed A)
    Hashable[Min[A]]            : ⦃ Hashable A ⦄ → Hashable (Min A)
    Hashable[Max[A]]            : ⦃ Hashable A ⦄ → Hashable (Max A)
    Hashable[First[A]]          : ⦃ Hashable A ⦄ → Hashable (First A)
    Hashable[Last[A]]           : ⦃ Hashable A ⦄ → Hashable (Last A)
    Hashable[StableName[A]]     : Hashable (StableName A)
    Hashable[Identity[A]]       : ⦃ Hashable A ⦄ → Hashable (Identity A)
    Hashable[NonEmpty[A]]       : ⦃ Hashable A ⦄ → Hashable (NonEmpty A)
    Hashable[Either[A,B]]       : ⦃ Hashable A ⦄ → ⦃ Hashable B ⦄ → Hashable (Either A B)
    Hashable[Tuple2[A,B]]       : ⦃ Hashable A ⦄ → ⦃ Hashable B ⦄ → Hashable (Tuple2 A B)
    Hashable[Arg[A,B]]          : ⦃ Hashable A ⦄ → Hashable (Arg A B)
    Hashable[Proxy[A]]          : Hashable (Proxy A)
    Hashable[Const[A,B]]        : ⦃ Hashable A ⦄ → Hashable (Const A B)
    Hashable[Tuple3[A,B,C]]     : ⦃ Hashable A ⦄ → ⦃ Hashable B ⦄ → ⦃ Hashable C ⦄ → Hashable (Tuple3 A B C)
    Hashable[Tuple4[A,B,C,D]]   : ⦃ Hashable A ⦄ → ⦃ Hashable B ⦄ → ⦃ Hashable C ⦄ → ⦃ Hashable D ⦄ → Hashable (Tuple4 A B C D)
    Hashable[Tuple4[A,B,C,D,E]] : ⦃ Hashable A ⦄ → ⦃ Hashable B ⦄ → ⦃ Hashable C ⦄ → ⦃ Hashable D ⦄ → ⦃ Hashable E ⦄ → Hashable (Tuple5 A B C D E)

    -- todo: Hashable instances for BigNat, LazyText
    -- SomeTypeRep, TypeRep[A]

{-# FOREIGN GHC data AgdaHashable aℓ a = Data.Hashable.Hashable a => AgdaHashable #-}
{-# COMPILE GHC Hashable = type(0) AgdaHashable #-}

{-# COMPILE GHC hashWithSalt = \ aℓ a AgdaHashable -> Data.Hashable.hashWithSalt #-}
{-# COMPILE GHC hash         = \ aℓ a AgdaHashable -> Data.Hashable.hash         #-}

{-# COMPILE GHC Hashable[Bool]            = AgdaHashable #-}
{-# COMPILE GHC Hashable[Char]            = AgdaHashable #-}
{-# COMPILE GHC Hashable[Double]          = AgdaHashable #-}
{-# COMPILE GHC Hashable[Float]           = AgdaHashable #-}
{-# COMPILE GHC Hashable[Int]             = AgdaHashable #-}
{-# COMPILE GHC Hashable[Int8]            = AgdaHashable #-}
{-# COMPILE GHC Hashable[Int16]           = AgdaHashable #-}
{-# COMPILE GHC Hashable[Int32]           = AgdaHashable #-}
{-# COMPILE GHC Hashable[Int64]           = AgdaHashable #-}
{-# COMPILE GHC Hashable[Word]            = AgdaHashable #-}
{-# COMPILE GHC Hashable[Word8]           = AgdaHashable #-}
{-# COMPILE GHC Hashable[Word16]          = AgdaHashable #-}
{-# COMPILE GHC Hashable[Word32]          = AgdaHashable #-}
{-# COMPILE GHC Hashable[Word64]          = AgdaHashable #-}
{-# COMPILE GHC Hashable[Integer]         = AgdaHashable #-}
{-# COMPILE GHC Hashable[Natural]         = AgdaHashable #-}
{-# COMPILE GHC Hashable[Ordering]        = AgdaHashable #-}
{-# COMPILE GHC Hashable[⊤]               = AgdaHashable #-}
{-# COMPILE GHC Hashable[Void]            = AgdaHashable #-}
{-# COMPILE GHC Hashable[Unique]          = AgdaHashable #-}
{-# COMPILE GHC Hashable[Version]         = AgdaHashable #-}
{-# COMPILE GHC Hashable[ThreadId]        = AgdaHashable #-}
{-# COMPILE GHC Hashable[WordPtr]         = AgdaHashable #-}
{-# COMPILE GHC Hashable[IntPtr]          = AgdaHashable #-}
{-# COMPILE GHC Hashable[Fingerprint]     = AgdaHashable #-}
-- {-# COMPILE GHC Hashable[ShortByteString] = AgdaHashable #-}
{-# COMPILE GHC Hashable[LazyByteString]  = AgdaHashable #-}
{-# COMPILE GHC Hashable[ByteString]      = AgdaHashable #-}
{-# COMPILE GHC Hashable[Text]            = AgdaHashable #-}

{-# COMPILE GHC Hashable[List[A]]           = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Maybe[A]]          = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Ratio[A]]          = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Ptr[A]]            = \ aℓ a -> AgdaHashable #-}
{-# COMPILE GHC Hashable[FunPtr[A]]         = \ aℓ a -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Complex[A]]        = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Fixed[A]]          = \ aℓ a -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Min[A]]            = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Max[A]]            = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[First[A]]          = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Last[A]]           = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[StableName[A]]     = \ aℓ a -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Identity[A]]       = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[NonEmpty[A]]       = \ aℓ a AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Either[A,B]]       = \ aℓ a AgdaHashable AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Tuple2[A,B]]       = \ aℓ a AgdaHashable AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Arg[A,B]]          = \ aℓ a bℓ b AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Proxy[A]]          = \ aℓ a -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Const[A,B]]        = \ aℓ a bℓ b AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Tuple3[A,B,C]]     = \ aℓ a bℓ b cℓ c AgdaHashable AgdaHashable AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Tuple4[A,B,C,D]]   = \ aℓ a bℓ b cℓ c dℓ d AgdaHashable AgdaHashable AgdaHashable AgdaHashable -> AgdaHashable #-}
{-# COMPILE GHC Hashable[Tuple4[A,B,C,D,E]] = \ aℓ a bℓ b cℓ c dℓ d eℓ e AgdaHashable AgdaHashable AgdaHashable AgdaHashable AgdaHashable -> AgdaHashable #-}


postulate
    hashUsing             : ⦃ Hashable B ⦄ → (A → B) → Int → A → Int
    hashPtr               : Ptr A → Int → IO Int
    hashPtrWithSalt       : Ptr A → Int → Int → IO Int
    hashByteArray         : ByteArray# → Int → Int → Int
    hashByteArrayWithSalt : ByteArray# → Int → Int → Int → Int

{-# COMPILE GHC hashUsing             = \ bℓ b aℓ a AgdaHashable -> Data.Hashable.hashUsing             #-}
{-# COMPILE GHC hashPtr               = \ aℓ a                   -> Data.Hashable.hashPtr               #-}
{-# COMPILE GHC hashPtrWithSalt       = \ aℓ a                   -> Data.Hashable.hashPtrWithSalt       #-}
{-# COMPILE GHC hashByteArray         =                             Data.Hashable.hashByteArray         #-}
{-# COMPILE GHC hashByteArrayWithSalt =                             Data.Hashable.hashByteArrayWithSalt #-}


postulate
    Hashed : Set aℓ → Set aℓ

    Foldable[Hashed]    : Foldable {aℓ} Hashed
    Eq1[Hashed]         : Eq1 {aℓ} Hashed
    Ord1[Hashed]        : Ord1 {aℓ} Hashed
    Show1[Hashed]       : Show1 {aℓ} Hashed
    Eq[Hashed[A]]       : ⦃ Eq A ⦄ → Eq (Hashed A)
    Ord[Hashed[A]]      : ⦃ Ord A ⦄ → Ord (Hashed A)
    Show[Hashed[A]]     : ⦃ Show A ⦄ → Show (Hashed A)
    IsString[Hashed[A]] : ⦃ IsString A ⦄ → ⦃ Hashable A ⦄ → IsString (Hashed A)
    NFData[Hashed[A]]   : ⦃ NFData A ⦄ → NFData (Hashed A)
    Hashable[Hashed[A]] : Hashable (Hashed A)

    hashed                : ⦃ Hashable A ⦄ → A → Hashed A
    unhashed              : Hashed A → A
    mapHashed             : ⦃ Hashable B ⦄ → (A → B) → Hashed A → Hashed B
    traverseHashed        : ⦃ Hashable B ⦄ → ⦃ Functor F ⦄ → (A → F B) → Hashed A → F (Hashed B)

{-# FOREIGN GHC type AgdaHashed aℓ = Data.Hashable.Hashed #-}
{-# COMPILE GHC Hashed = type(1) AgdaHashed #-}

{-# COMPILE GHC Foldable[Hashed]    = \ aℓ                             -> AgdaFoldable  #-}
{-# COMPILE GHC Eq1[Hashed]         = \ aℓ                             -> AgdaEq1       #-}
{-# COMPILE GHC Ord1[Hashed]        = \ aℓ                             -> AgdaOrd1      #-}
{-# COMPILE GHC Show1[Hashed]       = \ aℓ                             -> AgdaShow1     #-}
{-# COMPILE GHC Eq[Hashed[A]]       = \ aℓ a AgdaEq                    -> AgdaEq        #-}
{-# COMPILE GHC Ord[Hashed[A]]      = \ aℓ a AgdaOrd                   -> AgdaOrd       #-}
{-# COMPILE GHC Show[Hashed[A]]     = \ aℓ a AgdaShow                  -> AgdaShow      #-}
{-# COMPILE GHC IsString[Hashed[A]] = \ aℓ a AgdaIsString AgdaHashable -> AgdaIsString  #-}
{-# COMPILE GHC NFData[Hashed[A]]   = \ aℓ a AgdaNFData                -> AgdaNFData    #-}
{-# COMPILE GHC Hashable[Hashed[A]] = \ aℓ a                           -> AgdaHashable  #-}

{-# COMPILE GHC hashed         = \ aℓ a AgdaHashable                    -> Data.Hashable.hashed         #-}
{-# COMPILE GHC unhashed       = \ aℓ a                                 -> Data.Hashable.unhashed       #-}
{-# COMPILE GHC mapHashed      = \ aℓ a bℓ b AgdaHashable               -> Data.Hashable.mapHashed      #-}
{-# COMPILE GHC traverseHashed = \ aℓ a bℓ b f AgdaHashable AgdaFunctor -> Data.Hashable.traverseHashed #-}
