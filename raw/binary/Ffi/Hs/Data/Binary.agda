{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Binary where

open import Agda.Builtin.Bool                           using (Bool)
open import Agda.Builtin.Char                           using (Char)
open import Agda.Builtin.Int                            using () renaming (Int to Integer)
open import Agda.Builtin.IO                             using (IO)
open import Agda.Builtin.List                           using (List)
open import Agda.Builtin.Maybe                          using (Maybe)
open import Agda.Builtin.Unit                           using (⊤)
open import Agda.Primitive                              using (Level)
open import Ffi.Hs.-base.Class                          using (Integral)
open import Ffi.Hs.Data.Binary.Get                      using (ByteOffset)
open import Ffi.Hs.Data.ByteString                      using (ByteString)
open import Ffi.Hs.Data.ByteString.Lazy                 using (LazyByteString)
open import Ffi.Hs.Data.ByteString.Short                using (ShortByteString)
open import Ffi.Hs.Data.Complex                         using (Complex)
open import Ffi.Hs.Data.Either                          using (Either)
open import Ffi.Hs.Data.Fixed                           using (Fixed)
open import Ffi.Hs.Data.Functor.Identity                using (Identity)
open import Ffi.Hs.Data.Int                             using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.List.NonEmpty                   using (NonEmpty)
open import Ffi.Hs.Data.Monoid             as Monoid    using (Any; All; Dual; Sum; Product; Alt)
open import Ffi.Hs.Data.Ord                             using (Ordering)
open import Ffi.Hs.Data.Ratio                           using (Ratio)
open import Ffi.Hs.Data.Semigroup          as Semigroup using (Arg; Min; Max)
open import Ffi.Hs.Data.Tuple                           using (Tuple2; Tuple3; Tuple4; Tuple5)
open import Ffi.Hs.Data.Version                         using (Version)
open import Ffi.Hs.Data.Void                            using (Void)
open import Ffi.Hs.Data.Word                            using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.GHC.Fingerprint.Type                 using (Fingerprint)
open import Ffi.Hs.GHC.Float                            using (Float; Double)
open import Ffi.Hs.Numeric.Natural                      using (Natural)

open import Ffi.Hs.Data.Word public

open Ffi.Hs.Data.Binary.Get public
    using
    ( Get
    ; getWord8
    )

open import Ffi.Hs.Data.Binary.Put public
    using
    ( Put
    ; putWord8
    )

{-# FOREIGN GHC
import qualified Data.Binary
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaIntegral(AgdaIntegral))
#-}

private
    variable
        aℓ : Level
        A B C D E : Set aℓ
        F : Set aℓ → Set aℓ


postulate
    Binary : Set aℓ → Set aℓ

    put     : ⦃ Binary A ⦄ → A → Put
    get     : ⦃ Binary A ⦄ → Get A
    putList : ⦃ Binary A ⦄ → List A → Put

    Binary[Bool]            : Binary Bool
    Binary[Char]            : Binary Char
    Binary[Double]          : Binary Double
    Binary[Float]           : Binary Float
    Binary[Int]             : Binary Int
    Binary[Int8]            : Binary Int8
    Binary[Int16]           : Binary Int16
    Binary[Int32]           : Binary Int32
    Binary[Int64]           : Binary Int64
    Binary[Integer]         : Binary Integer
    Binary[Natural]         : Binary Natural
    Binary[Ordering]        : Binary Ordering
    Binary[Word]            : Binary Word
    Binary[Word8]           : Binary Word8
    Binary[Word16]          : Binary Word16
    Binary[Word32]          : Binary Word32
    Binary[Word64]          : Binary Word64
    Binary[⊤]               : Binary ⊤
    Binary[Void]            : Binary Void
    Binary[Version]         : Binary Version
    Binary[All]             : Binary All
    Binary[Any]             : Binary Any
    Binary[Fingerprint]     : Binary Fingerprint
    Binary[ShortByteString] : Binary ShortByteString
    Binary[LazyByteString]  : Binary LazyByteString
    Binary[ByteString]      : Binary ByteString

    Binary[List[A]]     : ⦃ Binary A ⦄ → Binary (List A)
    Binary[Maybe[A]]    : ⦃ Binary A ⦄ → Binary (Maybe A)
    Binary[Ratio[A]]    : ⦃ Binary A ⦄ → ⦃ Integral A ⦄ → Binary (Ratio A)
    Binary[Complex[A]]  : ⦃ Binary A ⦄ → Binary (Complex A)
    Binary[Fixed[A]]    : Binary (Fixed A)
    Binary[Min[A]]      : ⦃ Binary A ⦄ → Binary (Min A)
    Binary[Max[A]]      : ⦃ Binary A ⦄ → Binary (Max A)
    Binary[SFirst[A]]   : ⦃ Binary A ⦄ → Binary (Semigroup.First A)
    Binary[SLast[A]]    : ⦃ Binary A ⦄ → Binary (Semigroup.Last A)
    Binary[Identity[A]] : ⦃ Binary A ⦄ → Binary (Identity A)
    Binary[MFirst[A]]   : ⦃ Binary A ⦄ → Binary (Monoid.First A)
    Binary[MLast[A]]    : ⦃ Binary A ⦄ → Binary (Monoid.Last A)
    Binary[Dual[A]]     : ⦃ Binary A ⦄ → Binary (Dual A)
    Binary[Sum[A]]      : ⦃ Binary A ⦄ → Binary (Sum A)
    Binary[Product[A]]  : ⦃ Binary A ⦄ → Binary (Product A)
    Binary[NonEmpty[A]] : ⦃ Binary A ⦄ → Binary (NonEmpty A)

    Binary[Either[A,B]] : ⦃ Binary A ⦄ → ⦃ Binary B ⦄ → Binary (Either A B)
    Binary[Tuple2[A,B]] : ⦃ Binary A ⦄ → ⦃ Binary B ⦄ → Binary (Tuple2 A B)
    Binary[Arg[A,B]]    : ⦃ Binary A ⦄ → ⦃ Binary B ⦄ → Binary (Arg A B)
    Binary[Alt[F,A]]    : ⦃ Binary (F A) ⦄ → Binary (Alt F A)

    Binary[Tuple3[A,B,C]] : ⦃ Binary A ⦄ → ⦃ Binary B ⦄ → ⦃ Binary C ⦄ → Binary (Tuple3 A B C)
    Binary[Tuple4[A,B,C,D]] : ⦃ Binary A ⦄ → ⦃ Binary B ⦄ → ⦃ Binary C ⦄ → ⦃ Binary D ⦄ → Binary (Tuple4 A B C D)
    Binary[Tuple5[A,B,C,D,E]] : ⦃ Binary A ⦄ → ⦃ Binary B ⦄ → ⦃ Binary C ⦄ → ⦃ Binary D ⦄ → ⦃ Binary E ⦄ → Binary (Tuple5 A B C D E)

    -- todo: Binary instances for UArray, Array, IntSet, IntMap, Tree, Seq, Set, Map
    -- RuntimeRep, VecCount, VecElem, SomeTypeRep, TypeRep

{-# FOREIGN GHC data AgdaBinary aℓ a = Data.Binary.Binary a => AgdaBinary #-}
{-# COMPILE GHC Binary = type(0) AgdaBinary #-}

{-# COMPILE GHC put     = \ aℓ a AgdaBinary -> Data.Binary.put     #-}
{-# COMPILE GHC get     = \ aℓ a AgdaBinary -> Data.Binary.get     #-}
{-# COMPILE GHC putList = \ aℓ a AgdaBinary -> Data.Binary.putList #-}

{-# COMPILE GHC Binary[Bool]            = AgdaBinary #-}
{-# COMPILE GHC Binary[Char]            = AgdaBinary #-}
{-# COMPILE GHC Binary[Double]          = AgdaBinary #-}
{-# COMPILE GHC Binary[Float]           = AgdaBinary #-}
{-# COMPILE GHC Binary[Int]             = AgdaBinary #-}
{-# COMPILE GHC Binary[Int8]            = AgdaBinary #-}
{-# COMPILE GHC Binary[Int16]           = AgdaBinary #-}
{-# COMPILE GHC Binary[Int32]           = AgdaBinary #-}
{-# COMPILE GHC Binary[Int64]           = AgdaBinary #-}
{-# COMPILE GHC Binary[Integer]         = AgdaBinary #-}
{-# COMPILE GHC Binary[Natural]         = AgdaBinary #-}
{-# COMPILE GHC Binary[Ordering]        = AgdaBinary #-}
{-# COMPILE GHC Binary[Word]            = AgdaBinary #-}
{-# COMPILE GHC Binary[Word8]           = AgdaBinary #-}
{-# COMPILE GHC Binary[Word16]          = AgdaBinary #-}
{-# COMPILE GHC Binary[Word32]          = AgdaBinary #-}
{-# COMPILE GHC Binary[Word64]          = AgdaBinary #-}
{-# COMPILE GHC Binary[⊤]               = AgdaBinary #-}
{-# COMPILE GHC Binary[Void]            = AgdaBinary #-}
{-# COMPILE GHC Binary[Version]         = AgdaBinary #-}
{-# COMPILE GHC Binary[All]             = AgdaBinary #-}
{-# COMPILE GHC Binary[Any]             = AgdaBinary #-}
{-# COMPILE GHC Binary[Fingerprint]     = AgdaBinary #-}
{-# COMPILE GHC Binary[ShortByteString] = AgdaBinary #-}
{-# COMPILE GHC Binary[LazyByteString]  = AgdaBinary #-}
{-# COMPILE GHC Binary[ByteString]      = AgdaBinary #-}

{-# COMPILE GHC Binary[List[A]]     = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[Maybe[A]]    = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[Ratio[A]]    = \ aℓ a AgdaBinary AgdaIntegral -> AgdaBinary #-}
{-# COMPILE GHC Binary[Complex[A]]  = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[Fixed[A]]    = \ aℓ a                         -> AgdaBinary #-}
{-# COMPILE GHC Binary[Min[A]]      = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[Max[A]]      = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[SFirst[A]]   = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[SLast[A]]    = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[Identity[A]] = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[MFirst[A]]   = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[MLast[A]]    = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[Dual[A]]     = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[Sum[A]]      = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[Product[A]]  = \ aℓ a AgdaBinary              -> AgdaBinary #-}
{-# COMPILE GHC Binary[NonEmpty[A]] = \ aℓ a AgdaBinary              -> AgdaBinary #-}

{-# COMPILE GHC Binary[Either[A,B]] = \ aℓ a bℓ b AgdaBinary AgdaBinary -> AgdaBinary #-}
{-# COMPILE GHC Binary[Tuple2[A,B]] = \ aℓ a bℓ b AgdaBinary AgdaBinary -> AgdaBinary #-}
{-# COMPILE GHC Binary[Arg[A,B]]    = \ aℓ a bℓ b AgdaBinary AgdaBinary -> AgdaBinary #-}
{-# COMPILE GHC Binary[Alt[F,A]]    = \ fℓ f a AgdaBinary               -> AgdaBinary #-}

{-# COMPILE GHC Binary[Tuple3[A,B,C]] = \ aℓ a bℓ b cℓ c AgdaBinary AgdaBinary AgdaBinary -> AgdaBinary #-}
{-# COMPILE GHC Binary[Tuple4[A,B,C,D]] = \ aℓ a bℓ b cℓ c dℓ d AgdaBinary AgdaBinary AgdaBinary AgdaBinary -> AgdaBinary #-}
{-# COMPILE GHC Binary[Tuple5[A,B,C,D,E]] = \ aℓ a bℓ b cℓ c dℓ d eℓ e AgdaBinary AgdaBinary AgdaBinary AgdaBinary AgdaBinary -> AgdaBinary #-}


postulate
    encode           : ⦃ Binary A ⦄ → A → LazyByteString
    decode           : ⦃ Binary A ⦄ → LazyByteString → A
    decodeOrFail     : ⦃ Binary A ⦄ → LazyByteString → Either (Tuple3 LazyByteString ByteOffset (List Char)) (Tuple3 LazyByteString ByteOffset A)
    encodeFile       : ⦃ Binary A ⦄ → List Char → A → IO ⊤
    decodeFile       : ⦃ Binary A ⦄ → List Char → IO A
    decodeFileOrFail : ⦃ Binary A ⦄ → List Char → IO (Either (Tuple2 ByteOffset (List Char)) A)

{-# COMPILE GHC encode           = \ aℓ a AgdaBinary -> Data.Binary.encode           #-}
{-# COMPILE GHC decode           = \ aℓ a AgdaBinary -> Data.Binary.decode           #-}
{-# COMPILE GHC decodeOrFail     = \ aℓ a AgdaBinary -> Data.Binary.decodeOrFail     #-}
{-# COMPILE GHC encodeFile       = \ aℓ a AgdaBinary -> Data.Binary.encodeFile       #-}
{-# COMPILE GHC decodeFile       = \ aℓ a AgdaBinary -> Data.Binary.decodeFile       #-}
{-# COMPILE GHC decodeFileOrFail = \ aℓ a AgdaBinary -> Data.Binary.decodeFileOrFail #-}
