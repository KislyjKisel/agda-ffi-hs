{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.DeepSeq where

open import Ffi.Hs.-base.Class using (Monad)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤; ⊤′)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.DeepSeq
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ cℓ dℓ eℓ fℓ gℓ : Level
        A B C D E : Set aℓ
        M : Set aℓ → Set aℓ

infixl 4 _<$!!>_
infixr 0 _$!!_

postulate
    NFData : Set aℓ → Set aℓ
    rnf     : ⦃ NFData A ⦄ → A → ⊤′ {lzero}
    deepseq : ⦃ NFData A ⦄ → A → B → B
    force   : ⦃ NFData A ⦄ → A → A
    _$!!_   : ⦃ NFData A ⦄ → (A → B) → A → B
    _<$!!>_ : ⦃ Monad M ⦄ → ⦃ NFData B ⦄ → (A → B) → M A → M B
    rwhnf   : A → ⊤′ {lzero}

{-# FOREIGN GHC data AgdaNFData aℓ a = Control.DeepSeq.NFData a => AgdaNFData #-}
{-# COMPILE GHC NFData = type(0) AgdaNFData #-}

{-# COMPILE GHC rnf     = \ aℓ a AgdaNFData               -> Control.DeepSeq.rnf     #-}
{-# COMPILE GHC deepseq = \ aℓ a bℓ b AgdaNFData          -> Control.DeepSeq.deepseq #-}
{-# COMPILE GHC force   = \ aℓ a AgdaNFData               -> Control.DeepSeq.force   #-}
{-# COMPILE GHC _$!!_   = \ aℓ a bℓ b AgdaNFData          -> (Control.DeepSeq.$!!)   #-}
{-# COMPILE GHC _<$!!>_ = \ mℓ m b a AgdaMonad AgdaNFData -> (Control.DeepSeq.<$!!>) #-}
{-# COMPILE GHC rwhnf   = \ aℓ a                          -> Control.DeepSeq.rwhnf   #-}

module _ where
    private variable F : Set aℓ → Set bℓ
    postulate
        NFData1 : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
        liftRnf : ⦃ NFData1 F ⦄ → (A → ⊤′ {lzero}) → F A → ⊤′ {lzero}
        rnf1    : ⦃ NFData1 F ⦄ → ⦃ NFData A ⦄ → F A → ⊤′ {lzero}

module _ where
    private variable F : Set aℓ → Set bℓ → Set cℓ
    postulate
        NFData2 : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
        liftRnf2 : ⦃ NFData2 F ⦄ → (A → ⊤′ {lzero}) → (B → ⊤′ {lzero}) → F A B → ⊤′ {lzero}
        rnf2     : ⦃ NFData2 F ⦄ → ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → F A B → ⊤′ {lzero}

{-# FOREIGN GHC data AgdaNFData1 aℓ bℓ f = Control.DeepSeq.NFData1 f => AgdaNFData1 #-}
{-# COMPILE GHC NFData1 = type(0) AgdaNFData1 #-}

{-# COMPILE GHC liftRnf = \ aℓ bℓ f a AgdaNFData1            -> Control.DeepSeq.liftRnf #-}
{-# COMPILE GHC rnf1    = \ aℓ bℓ f a AgdaNFData1 AgdaNFData -> Control.DeepSeq.rnf1    #-}

{-# FOREIGN GHC data AgdaNFData2 aℓ bℓ cℓ f = Control.DeepSeq.NFData2 f => AgdaNFData2 #-}
{-# COMPILE GHC NFData2 = type(0) AgdaNFData2 #-}

{-# COMPILE GHC liftRnf2 = \ aℓ bℓ cℓ f a b AgdaNFData2                       -> Control.DeepSeq.liftRnf2 #-}
{-# COMPILE GHC rnf2     = \ aℓ bℓ cℓ f a b AgdaNFData2 AgdaNFData AgdaNFData -> Control.DeepSeq.rnf2     #-}

open import Agda.Builtin.Bool                           using (Bool)
open import Agda.Builtin.Char                           using (Char)
open import Agda.Builtin.Int                            using () renaming (Int to Integer)
open import Agda.Builtin.List                           using (List)
open import Agda.Builtin.Maybe                          using (Maybe)
open import Ffi.Hs.-base.Float                          using (Float; Double)
open import Ffi.Hs.Control.Applicative                  using (ZipList)
open import Ffi.Hs.Control.Concurrent                   using (ThreadId)
open import Ffi.Hs.Control.Concurrent.MVar              using (MVar)
open import Ffi.Hs.Control.Exception.Base               using (MaskingState)
open import Ffi.Hs.Data.Array                           using (Array)
open import Ffi.Hs.Data.Complex                         using (Complex)
open import Ffi.Hs.Data.Either                          using (Either)
open import Ffi.Hs.Data.Fixed                           using (Fixed)
open import Ffi.Hs.Data.Functor.Compose                 using (Compose)
open import Ffi.Hs.Data.Functor.Const                   using (Const)
open import Ffi.Hs.Data.Functor.Identity                using (Identity)
open import Ffi.Hs.Data.Functor.Compose    as Fun       using ()
open import Ffi.Hs.Data.Functor.Product    as Fun       using ()
open import Ffi.Hs.Data.Functor.Sum        as Fun       using ()
open import Ffi.Hs.Data.Int                             using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.IORef                           using (IORef)
open import Ffi.Hs.Data.List.NonEmpty                   using (NonEmpty)
open import Ffi.Hs.Data.Monoid             as Monoid    using (Any; All; Dual; Sum; Product)
open import Ffi.Hs.Data.Ord                             using (Ordering; Down)
open import Ffi.Hs.Data.Proxy                           using (Proxy)
open import Ffi.Hs.Data.Ratio                           using (Ratio)
open import Ffi.Hs.Data.Semigroup          as Semigroup using (Arg; Min; Max)
open import Ffi.Hs.Data.STRef                           using (STRef)
open import Ffi.Hs.Data.Tuple                           using (Tuple2; Tuple3; Tuple4; Tuple5)
open import Ffi.Hs.Data.Type.Equality                   using (_:~:_; _:~~:_)
open import Ffi.Hs.Data.Typeable                        using (TypeRep)
open import Ffi.Hs.Data.Unique                          using (Unique)
open import Ffi.Hs.Data.Version                         using (Version)
open import Ffi.Hs.Data.Void                            using (Void)
open import Ffi.Hs.Data.Word                            using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.Foreign.C.Types
open import Ffi.Hs.Foreign.Ptr                          using (Ptr; FunPtr)
open import Ffi.Hs.GHC.Fingerprint.Type                 using (Fingerprint)
open import Ffi.Hs.GHC.Stack                            using (CallStack; SrcLoc)
open import Ffi.Hs.Numeric.Natural                      using (Natural)
open import Ffi.Hs.System.Exit                          using (ExitCode)
open import Ffi.Hs.System.Mem.StableName                using (StableName)
open import Ffi.Hs.Type.Reflection                      using (TyCon)

postulate
    NFData[Bool]              : NFData Bool
    NFData[Char]              : NFData Char
    NFData[Double]            : NFData Double
    NFData[Float]             : NFData Float
    NFData[Int]               : NFData Int
    NFData[Int8]              : NFData Int8
    NFData[Int16]             : NFData Int16
    NFData[Int32]             : NFData Int32
    NFData[Int64]             : NFData Int64
    NFData[Integer]           : NFData Integer
    NFData[Natural]           : NFData Natural
    NFData[Ordering]          : NFData Ordering
    NFData[Word]              : NFData Word
    NFData[Word8]             : NFData Word8
    NFData[Word16]            : NFData Word16
    NFData[Word32]            : NFData Word32
    NFData[Word64]            : NFData Word64
    NFData[CallStack]         : NFData CallStack
    NFData[⊤]                 : NFData (⊤′ {aℓ})
    NFData[TyCon]             : NFData TyCon
    NFData[Void]              : NFData Void
    NFData[Unique]            : NFData Unique
    NFData[Version]           : NFData Version
    NFData[ThreadId]          : NFData ThreadId
    NFData[ExitCode]          : NFData ExitCode
    NFData[MaskingState]      : NFData MaskingState
    NFData[TypeRep]           : NFData (TypeRep {aℓ})
    NFData[All]               : NFData All
    NFData[Any]               : NFData Any
    NFData[CChar]             : NFData CChar
    NFData[CSChar]            : NFData CSChar
    NFData[CUChar]            : NFData CUChar
    NFData[CShort]            : NFData CShort
    NFData[CUShort]           : NFData CUShort
    NFData[CInt]              : NFData CInt
    NFData[CUInt]             : NFData CUInt
    NFData[CLong]             : NFData CLong
    NFData[CULong]            : NFData CULong
    NFData[CLLong]            : NFData CLLong
    NFData[CULLong]           : NFData CULLong
    NFData[CBool]             : NFData CBool
    NFData[CFloat]            : NFData CFloat
    NFData[CDouble]           : NFData CDouble
    NFData[CPtrdiff]          : NFData CPtrdiff
    NFData[CSize]             : NFData CSize
    NFData[CWchar]            : NFData CWchar
    NFData[CSigAtomic]        : NFData CSigAtomic
    NFData[CClock]            : NFData CClock
    NFData[CTime]             : NFData CTime
    NFData[CUSeconds]         : NFData CUSeconds
    NFData[CSUSeconds]        : NFData CSUSeconds
    NFData[CFile]             : NFData CFile
    NFData[CFpos]             : NFData CFpos
    NFData[CJmpBuf]           : NFData CJmpBuf
    NFData[CIntPtr]           : NFData CIntPtr
    NFData[CUIntPtr]          : NFData CUIntPtr
    NFData[Fingerprint]       : NFData Fingerprint
    NFData[SrcLoc]            : NFData SrcLoc
    NFData[List[A]]           : ⦃ NFData A ⦄ → NFData (List A)
    NFData[Maybe[A]]          : ⦃ NFData A ⦄ → NFData (Maybe A)
    NFData[Ratio[A]]          : ⦃ NFData A ⦄ → NFData (Ratio A)
    NFData[Ptr[A]]            : NFData (Ptr A)
    NFData[FunPtr[A]]         : NFData (FunPtr A)
    NFData[Complex[A]]        : ⦃ NFData A ⦄ → NFData (Complex A)
    NFData[Min[A]]            : ⦃ NFData A ⦄ → NFData (Min A)
    NFData[Max[A]]            : ⦃ NFData A ⦄ → NFData (Max A)
    NFData[SFirst[A]]         : ⦃ NFData A ⦄ → NFData (Semigroup.First A)
    NFData[SLast[A]]          : ⦃ NFData A ⦄ → NFData (Semigroup.Last A)
    NFData[StableName[A]]     : NFData (StableName A)
    NFData[ZipList[A]]        : ⦃ NFData A ⦄ → NFData (ZipList A)
    NFData[Identity[A]]       : ⦃ NFData A ⦄ → NFData (Identity A)
    NFData[IORef[A]]          : NFData (IORef A)
    NFData[MFirst[A]]         : ⦃ NFData A ⦄ → NFData (Monoid.First A)
    NFData[MLast[A]]          : ⦃ NFData A ⦄ → NFData (Monoid.Last A)
    NFData[Dual[A]]           : ⦃ NFData A ⦄ → NFData (Dual A)
    NFData[MSum[A]]           : ⦃ NFData A ⦄ → NFData (Sum A)
    NFData[MProduct[A]]       : ⦃ NFData A ⦄ → NFData (Product A)
    NFData[Down[A]]           : ⦃ NFData A ⦄ → NFData (Down A)
    NFData[MVar[A]]           : NFData (MVar A)
    NFData[NonEmpty[A]]       : ⦃ NFData A ⦄ → NFData (NonEmpty A)
    NFData[A⟶B]               : NFData (A → B)
    NFData[Either[A,B]]       : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → NFData (Either A B)
    NFData[Array[A,B]]        : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → NFData (Array A B)
    NFData[Fixed[A]]          : NFData (Fixed A)
    NFData[Arg[A,B]]          : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → NFData (Arg A B)
    NFData[Proxy[A]]          : NFData (Proxy A)
    NFData[STRef[S,A]]        : ∀{S} → NFData (STRef S A)
    NFData[Tuple2[A,B]]       : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → NFData (Tuple2 A B)
    NFData[Tuple3[A,B,C]]     : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → ⦃ NFData C ⦄ → NFData (Tuple3 A B C)
    NFData[Tuple4[A,B,C,D]]   : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → ⦃ NFData C ⦄ → ⦃ NFData D ⦄ → NFData (Tuple4 A B C D)
    NFData[Tuple5[A,B,C,D,E]] : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → ⦃ NFData C ⦄ → ⦃ NFData D ⦄ → ⦃ NFData E ⦄ → NFData (Tuple5 A B C D E)
    NFData[Const[A,B]]        : ⦃ NFData A ⦄ → NFData (Const A B)
    NFData[A:~:B]             : NFData (A :~: B)
    NFData[A:~~:B]            : NFData (A :~~: B)
    NFData[FProduct]          : {F : Set aℓ → Set fℓ} → {G : Set aℓ → Set gℓ} → ⦃ NFData1 F ⦄ → ⦃ NFData1 G ⦄ → ⦃ NFData A ⦄ → NFData (Fun.Product F G A)
    NFData[FSum]              : {F : Set aℓ → Set fℓ} → {G : Set aℓ → Set gℓ} → ⦃ NFData1 F ⦄ → ⦃ NFData1 G ⦄ → ⦃ NFData A ⦄ → NFData (Fun.Sum F G A)
    NFData[Compose]           : {F : Set gℓ → Set fℓ} → {G : Set aℓ → Set gℓ} → ⦃ NFData1 F ⦄ → ⦃ NFData1 G ⦄ → ⦃ NFData A ⦄ → NFData (Fun.Compose F G A)

{-# COMPILE GHC NFData[Bool]              = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Char]              = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Double]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Float]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Int]               = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Int8]              = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Int16]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Int32]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Int64]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Integer]           = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Natural]           = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Ordering]          = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Word]              = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Word8]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Word16]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Word32]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Word64]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CallStack]         = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[⊤]                 = \ aℓ                                                                              -> AgdaNFData #-}
{-# COMPILE GHC NFData[TyCon]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Void]              = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Unique]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Version]           = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[ThreadId]          = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[ExitCode]          = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[MaskingState]      = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[TypeRep]           = \ aℓ                                                                              -> AgdaNFData #-}
{-# COMPILE GHC NFData[All]               = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Any]               = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CChar]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CSChar]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CUChar]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CShort]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CUShort]           = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CInt]              = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CUInt]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CLong]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CULong]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CLLong]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CULLong]           = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CBool]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CFloat]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CDouble]           = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CPtrdiff]          = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CSize]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CWchar]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CSigAtomic]        = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CClock]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CTime]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CUSeconds]         = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CSUSeconds]        = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CFile]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CFpos]             = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CJmpBuf]           = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CIntPtr]           = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[CUIntPtr]          = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[Fingerprint]       = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[SrcLoc]            = AgdaNFData                                                                                      #-}
{-# COMPILE GHC NFData[List[A]]           = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Maybe[A]]          = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Ratio[A]]          = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Ptr[A]]            = \ aℓ a                                                                            -> AgdaNFData #-}
{-# COMPILE GHC NFData[FunPtr[A]]         = \ aℓ a                                                                            -> AgdaNFData #-}
{-# COMPILE GHC NFData[Complex[A]]        = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Min[A]]            = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Max[A]]            = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[SFirst[A]]         = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[SLast[A]]          = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[StableName[A]]     = \ aℓ a                                                                            -> AgdaNFData #-}
{-# COMPILE GHC NFData[ZipList[A]]        = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Identity[A]]       = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[IORef[A]]          = \ aℓ a                                                                            -> AgdaNFData #-}
{-# COMPILE GHC NFData[MFirst[A]]         = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[MLast[A]]          = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Dual[A]]           = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[MSum[A]]           = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[MProduct[A]]       = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Down[A]]           = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[MVar[A]]           = \ aℓ a                                                                            -> AgdaNFData #-}
{-# COMPILE GHC NFData[NonEmpty[A]]       = \ aℓ a AgdaNFData                                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[A⟶B]               = \ aℓ a bℓ b                                                                       -> AgdaNFData #-}
{-# COMPILE GHC NFData[Either[A,B]]       = \ aℓ a bℓ b AgdaNFData AgdaNFData                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Array[A,B]]        = \ aℓ a bℓ b AgdaNFData AgdaNFData                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Fixed[A]]          = \ aℓ a                                                                            -> AgdaNFData #-}
{-# COMPILE GHC NFData[Arg[A,B]]          = \ aℓ a bℓ b AgdaNFData AgdaNFData                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Proxy[A]]          = \ aℓ a                                                                            -> AgdaNFData #-}
{-# COMPILE GHC NFData[STRef[S,A]]        = \ s aℓ a                                                                          -> AgdaNFData #-}
{-# COMPILE GHC NFData[Tuple2[A,B]]       = \ aℓ a bℓ b AgdaNFData AgdaNFData                                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Tuple3[A,B,C]]     = \ aℓ a bℓ b cℓ c AgdaNFData AgdaNFData AgdaNFData                                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Tuple4[A,B,C,D]]   = \ aℓ a bℓ b cℓ c dℓ d AgdaNFData AgdaNFData AgdaNFData AgdaNFData                 -> AgdaNFData #-}
{-# COMPILE GHC NFData[Tuple5[A,B,C,D,E]] = \ aℓ a bℓ b cℓ c dℓ d eℓ e AgdaNFData AgdaNFData AgdaNFData AgdaNFData AgdaNFData -> AgdaNFData #-}
{-# COMPILE GHC NFData[Const[A,B]]        = \ aℓ a bℓ b AgdaNFData                                                            -> AgdaNFData #-}
{-# COMPILE GHC NFData[A:~:B]             = \ aℓ a b                                                                          -> AgdaNFData #-}
{-# COMPILE GHC NFData[A:~~:B]            = \ aℓ a b                                                                          -> AgdaNFData #-}
{-# COMPILE GHC NFData[FProduct]          = \ aℓ fℓ f gℓ g a AgdaNFData1 AgdaNFData1 AgdaNFData                               -> AgdaNFData #-}
{-# COMPILE GHC NFData[FSum]              = \ aℓ fℓ f gℓ g a AgdaNFData1 AgdaNFData1 AgdaNFData                               -> AgdaNFData #-}
{-# COMPILE GHC NFData[Compose]           = \ gℓ fℓ f aℓ g a AgdaNFData1 AgdaNFData1 AgdaNFData                               -> AgdaNFData #-}

postulate
    NFData1[List]          : NFData1 {aℓ} List
    NFData1[Maybe]         : NFData1 {aℓ} Maybe
    NFData1[Ratio]         : NFData1 {aℓ} Ratio
    NFData1[Ptr]           : NFData1 {aℓ} Ptr
    NFData1[FunPtr]        : NFData1 {aℓ} FunPtr
    NFData1[Min]           : NFData1 {aℓ} Min
    NFData1[Max]           : NFData1 {aℓ} Max
    NFData1[SFirst]        : NFData1 {aℓ} Semigroup.First
    NFData1[SLast]         : NFData1 {aℓ} Semigroup.Last
    NFData1[StableName]    : NFData1 {aℓ} StableName
    NFData1[ZipList]       : NFData1 {aℓ} ZipList
    NFData1[Identity]      : NFData1 {aℓ} Identity
    NFData1[IORef]         : NFData1 {aℓ} IORef
    NFData1[MFirst]        : NFData1 {aℓ} Monoid.First
    NFData1[MLast]         : NFData1 {aℓ} Monoid.Last
    NFData1[Dual]          : NFData1 {aℓ} Dual
    NFData1[MSum]          : NFData1 {aℓ} Sum
    NFData1[MProduct]      : NFData1 {aℓ} Product
    NFData1[Down]          : NFData1 {aℓ} Down
    NFData1[MVar]          : NFData1 {aℓ} MVar
    NFData1[NonEmpty]      : NFData1 {aℓ} NonEmpty
    NFData1[Either[A]]     : ⦃ NFData A ⦄ → NFData1 (Either {bℓ = bℓ} A)
    NFData1[Array[A]]      : ⦃ NFData A ⦄ → NFData1 (Array {eℓ = bℓ} A)
    NFData1[Fixed]         : NFData1 {aℓ} Fixed
    NFData1[Arg[A]]        : ⦃ NFData A ⦄ → NFData1 (Arg {bℓ = bℓ} A)
    NFData1[Proxy]         : NFData1 {aℓ} Proxy
    NFData1[STRef[S]]      : ∀{S} → NFData1 {aℓ} (STRef S)
    NFData1[Const[A]]      : ⦃ NFData A ⦄ → NFData1 (Const {bℓ = bℓ} A)
    NFData1[FSum[F,G]]     : {F : Set aℓ → Set fℓ} → {G : Set aℓ → Set gℓ} → ⦃ NFData1 F ⦄ → ⦃ NFData1 G ⦄ → NFData1 (Fun.Sum F G)
    NFData1[FProduct[F,G]] : {F : Set aℓ → Set fℓ} → {G : Set aℓ → Set gℓ} → ⦃ NFData1 F ⦄ → ⦃ NFData1 G ⦄ → NFData1 (Fun.Product F G)
    NFData1[Compose[F,G]]  : {F : Set gℓ → Set fℓ} → {G : Set aℓ → Set gℓ} → ⦃ NFData1 F ⦄ → ⦃ NFData1 G ⦄ → NFData1 (Fun.Compose F G)
    NFData1[A:~:]          : NFData1 (A :~:_)
    NFData1[A:~~:]         : NFData1 (A :~~:_)
    NFData1[Tuple2[A]]     : ⦃ NFData A ⦄ → NFData1 (Tuple2 {aℓ} {bℓ} A)
    NFData1[Tuple3[A]]     : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → NFData1 (Tuple3 {aℓ} {bℓ} {cℓ} A B)
    NFData1[Tuple4[A]]     : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → ⦃ NFData C ⦄ → NFData1 (Tuple4 {aℓ} {bℓ} {cℓ} {dℓ} A B C)
    NFData1[Tuple5[A]]     : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → ⦃ NFData C ⦄ → ⦃ NFData D ⦄ → NFData1 (Tuple5 {aℓ} {bℓ} {cℓ} {dℓ} {eℓ} A B C D)

{-# COMPILE GHC NFData1[List]          = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Maybe]         = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Ratio]         = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Ptr]           = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[FunPtr]        = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Min]           = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Max]           = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[SFirst]        = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[SLast]         = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[StableName]    = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[ZipList]       = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Identity]      = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[IORef]         = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[MFirst]        = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[MLast]         = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Dual]          = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[MSum]          = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[MProduct]      = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Down]          = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[MVar]          = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[NonEmpty]      = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Either[A]]     = \ aℓ a bℓ AgdaNFData                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Array[A]]      = \ aℓ a bℓ AgdaNFData                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Fixed]         = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Arg[A]]        = \ aℓ a bℓ AgdaNFData                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Proxy]         = \ aℓ                                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[STRef[S]]      = \ aℓ s                                                               -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Const[A]]      = \ bℓ aℓ a AgdaNFData                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[FSum[F,G]]     = \ aℓ fℓ gℓ f g AgdaNFData1 AgdaNFData1                               -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[FProduct[F,G]] = \ aℓ fℓ gℓ f g AgdaNFData1 AgdaNFData1                               -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Compose[F,G]]  = \ aℓ fℓ gℓ f g AgdaNFData1 AgdaNFData1                               -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[A:~:]          = \ aℓ a                                                               -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[A:~~:]         = \ aℓ a                                                               -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Tuple2[A]]     = \ aℓ bℓ a AgdaNFData                                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Tuple3[A]]     = \ aℓ bℓ cℓ a b AgdaNFData AgdaNFData                                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Tuple4[A]]     = \ aℓ bℓ cℓ dℓ a b c AgdaNFData AgdaNFData AgdaNFData                 -> AgdaNFData1 #-}
{-# COMPILE GHC NFData1[Tuple5[A]]     = \ aℓ bℓ cℓ dℓ eℓ a b c d AgdaNFData AgdaNFData AgdaNFData AgdaNFData -> AgdaNFData1 #-}

postulate
    NFData2[Either] : NFData2 {aℓ} {bℓ} Either
    NFData2[Array]  : NFData2 {aℓ} {bℓ} Array
    NFData2[Arg]    : NFData2 {aℓ} {bℓ} Arg
    NFData2[STRef]  : NFData2 {bℓ = aℓ} STRef
    NFData2[Tuple2] : NFData2 {aℓ} {bℓ} Tuple2
    NFData2[Tuple3] : ⦃ NFData A ⦄ → NFData2 {bℓ} {cℓ} (Tuple3 A)
    NFData2[Tuple4] : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → NFData2 {cℓ} {dℓ} (Tuple4 A B)
    NFData2[Tuple5] : ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → ⦃ NFData C ⦄ → NFData2 {dℓ} {eℓ} (Tuple5 A B C)
    NFData2[Const]  : NFData2 {aℓ} {bℓ} Const
    NFData2[:~:]    : NFData2 {aℓ} _:~:_
    -- todo: NFData2[:~~:]   : NFData2 (λ A → _:~~:_ {K₁ = Set (lsuc aℓ)} A {K₂ = Set (lsuc aℓ)})

{-# COMPILE GHC NFData2[Either] = \ aℓ bℓ                                                 -> AgdaNFData2 #-}
{-# COMPILE GHC NFData2[Array]  = \ aℓ bℓ                                                 -> AgdaNFData2 #-}
{-# COMPILE GHC NFData2[Arg]    = \ aℓ bℓ                                                 -> AgdaNFData2 #-}
{-# COMPILE GHC NFData2[STRef]  = \ aℓ                                                    -> AgdaNFData2 #-}
{-# COMPILE GHC NFData2[Tuple2] = \ aℓ bℓ                                                 -> AgdaNFData2 #-}
{-# COMPILE GHC NFData2[Tuple3] = \ aℓ bℓ cℓ a AgdaNFData                                 -> AgdaNFData2 #-}
{-# COMPILE GHC NFData2[Tuple4] = \ aℓ bℓ cℓ dℓ a b AgdaNFData AgdaNFData                 -> AgdaNFData2 #-}
{-# COMPILE GHC NFData2[Tuple5] = \ aℓ bℓ cℓ dℓ eℓ a b c AgdaNFData AgdaNFData AgdaNFData -> AgdaNFData2 #-}
{-# COMPILE GHC NFData2[Const]  = \ aℓ bℓ                                                 -> AgdaNFData2 #-}
{-# COMPILE GHC NFData2[:~:]    = \ aℓ                                                    -> AgdaNFData2 #-}
-- {-# COMPILE GHC NFData2[:~~:]   = \ aℓ                                                    -> AgdaNFData2 #-}
