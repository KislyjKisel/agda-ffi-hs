{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Ptr where

open import Agda.Builtin.IO    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Word   using (Word)

{-# FOREIGN GHC
import qualified Foreign.Ptr
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

data IntPtr : Set where
    mkIntPtr : Int → IntPtr

{-# COMPILE GHC IntPtr = data Foreign.Ptr.IntPtr (Foreign.Ptr.IntPtr) #-}

data WordPtr : Set where
    mkWordPtr : Word → WordPtr

{-# COMPILE GHC WordPtr = data Foreign.Ptr.WordPtr (Foreign.Ptr.WordPtr) #-}

postulate
    Ptr      : Set aℓ → Set aℓ
    nullPtr  : Ptr A
    castPtr  : Ptr A → Ptr B
    plusPtr  : Ptr A → Int → Ptr B
    alignPtr : Ptr A → Int → Ptr A
    minusPtr : Ptr A → Ptr B → Int

    FunPtr            : Set aℓ → Set aℓ
    nullFunPtr        : FunPtr A
    castFunPtr        : FunPtr A → FunPtr B
    castFunPtrToPtr   : FunPtr A → Ptr B
    castPtrToFunPtr   : Ptr A → FunPtr B
    freeHaskellFunPtr : FunPtr A → IO (⊤ {lzero})

    ptrToIntPtr  : Ptr A → IntPtr
    intPtrToPtr  : IntPtr → Ptr A
    ptrToWordPtr : Ptr A → WordPtr
    wordPtrToPtr : WordPtr → Ptr A

{-# FOREIGN GHC type AgdaPtr aℓ = Foreign.Ptr.Ptr #-}
{-# COMPILE GHC Ptr = type(1) AgdaPtr #-}

{-# COMPILE GHC nullPtr  = \ aℓ a      -> Foreign.Ptr.nullPtr  #-}
{-# COMPILE GHC castPtr  = \ aℓ a bℓ b -> Foreign.Ptr.castPtr  #-}
{-# COMPILE GHC plusPtr  = \ aℓ a bℓ b -> Foreign.Ptr.plusPtr  #-}
{-# COMPILE GHC alignPtr = \ aℓ a      -> Foreign.Ptr.alignPtr #-}
{-# COMPILE GHC minusPtr = \ aℓ a bℓ b -> Foreign.Ptr.minusPtr #-}

{-# FOREIGN GHC type AgdaFunPtr aℓ = Foreign.Ptr.FunPtr #-}
{-# COMPILE GHC FunPtr = type(1) AgdaFunPtr #-}

{-# COMPILE GHC nullFunPtr        = \ aℓ a      -> Foreign.Ptr.nullFunPtr        #-}
{-# COMPILE GHC castFunPtr        = \ aℓ a bℓ b -> Foreign.Ptr.castFunPtr        #-}
{-# COMPILE GHC castFunPtrToPtr   = \ aℓ a bℓ b -> Foreign.Ptr.castFunPtrToPtr   #-}
{-# COMPILE GHC castPtrToFunPtr   = \ aℓ a bℓ b -> Foreign.Ptr.castPtrToFunPtr   #-}
{-# COMPILE GHC freeHaskellFunPtr = \ aℓ a      -> Foreign.Ptr.freeHaskellFunPtr #-}

{-# COMPILE GHC ptrToIntPtr  = \ aℓ a -> Foreign.Ptr.ptrToIntPtr  #-}
{-# COMPILE GHC intPtrToPtr  = \ aℓ a -> Foreign.Ptr.intPtrToPtr  #-}
{-# COMPILE GHC ptrToWordPtr = \ aℓ a -> Foreign.Ptr.ptrToWordPtr #-}
{-# COMPILE GHC wordPtrToPtr = \ aℓ a -> Foreign.Ptr.wordPtrToPtr #-}

postulate
    Data[Ptr[A]]     : ⦃ Data A ⦄ → Data (Ptr A)
    Storable[Ptr[A]] : Storable (Ptr A)
    Show[Ptr[A]]     : Show (Ptr A)
    Eq[Ptr[A]]       : Eq (Ptr A)
    Ord[Ptr[A]]      : Ord (Ptr A)

    Storable[FunPtr[A]] : Storable (FunPtr A)
    Show[FunPtr[A]]     : Show (FunPtr A)
    Eq[FunPtr[A]]       : Eq (FunPtr A)
    Ord[FunPtr[A]]      : Ord (FunPtr A)

    Data[IntPtr]       : Data IntPtr
    Storable[IntPtr]   : Storable IntPtr
    Bits[IntPtr]       : Bits IntPtr
    FiniteBits[IntPtr] : FiniteBits IntPtr
    Bounded[IntPtr]    : Bounded IntPtr
    Enum[IntPtr]       : Enum IntPtr
    Ix[IntPtr]         : Ix IntPtr
    Num[IntPtr]        : Num IntPtr
    Read[IntPtr]       : Read IntPtr
    Integral[IntPtr]   : Integral IntPtr
    Real[IntPtr]       : Real IntPtr
    Show[IntPtr]       : Show IntPtr
    Eq[IntPtr]         : Eq IntPtr
    Ord[IntPtr]        : Ord IntPtr

    Data[WordPtr]       : Data WordPtr
    Storable[WordPtr]   : Storable WordPtr
    Bits[WordPtr]       : Bits WordPtr
    FiniteBits[WordPtr] : FiniteBits WordPtr
    Bounded[WordPtr]    : Bounded WordPtr
    Enum[WordPtr]       : Enum WordPtr
    Ix[WordPtr]         : Ix WordPtr
    Num[WordPtr]        : Num WordPtr
    Read[WordPtr]       : Read WordPtr
    Integral[WordPtr]   : Integral WordPtr
    Real[WordPtr]       : Real WordPtr
    Show[WordPtr]       : Show WordPtr
    Eq[WordPtr]         : Eq WordPtr
    Ord[WordPtr]        : Ord WordPtr

{-# COMPILE GHC Data[Ptr[A]]     = \ aℓ a AgdaData -> AgdaData     #-}
{-# COMPILE GHC Storable[Ptr[A]] = \ aℓ a          -> AgdaStorable #-}
{-# COMPILE GHC Show[Ptr[A]]     = \ aℓ a          -> AgdaShow     #-}
{-# COMPILE GHC Eq[Ptr[A]]       = \ aℓ a          -> AgdaEq       #-}
{-# COMPILE GHC Ord[Ptr[A]]      = \ aℓ a          -> AgdaOrd      #-}

{-# COMPILE GHC Storable[FunPtr[A]] = \ aℓ a -> AgdaStorable #-}
{-# COMPILE GHC Show[FunPtr[A]]     = \ aℓ a -> AgdaShow     #-}
{-# COMPILE GHC Eq[FunPtr[A]]       = \ aℓ a -> AgdaEq       #-}
{-# COMPILE GHC Ord[FunPtr[A]]      = \ aℓ a -> AgdaOrd      #-}

{-# COMPILE GHC Data[IntPtr]       = AgdaData       #-}
{-# COMPILE GHC Storable[IntPtr]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[IntPtr]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[IntPtr] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[IntPtr]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[IntPtr]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[IntPtr]         = AgdaIx         #-}
{-# COMPILE GHC Num[IntPtr]        = AgdaNum        #-}
{-# COMPILE GHC Read[IntPtr]       = AgdaRead       #-}
{-# COMPILE GHC Integral[IntPtr]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[IntPtr]       = AgdaReal       #-}
{-# COMPILE GHC Show[IntPtr]       = AgdaShow       #-}
{-# COMPILE GHC Eq[IntPtr]         = AgdaEq         #-}
{-# COMPILE GHC Ord[IntPtr]        = AgdaOrd        #-}

{-# COMPILE GHC Data[WordPtr]       = AgdaData       #-}
{-# COMPILE GHC Storable[WordPtr]   = AgdaStorable   #-}
{-# COMPILE GHC Bits[WordPtr]       = AgdaBits       #-}
{-# COMPILE GHC FiniteBits[WordPtr] = AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[WordPtr]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[WordPtr]       = AgdaEnum       #-}
{-# COMPILE GHC Ix[WordPtr]         = AgdaIx         #-}
{-# COMPILE GHC Num[WordPtr]        = AgdaNum        #-}
{-# COMPILE GHC Read[WordPtr]       = AgdaRead       #-}
{-# COMPILE GHC Integral[WordPtr]   = AgdaIntegral   #-}
{-# COMPILE GHC Real[WordPtr]       = AgdaReal       #-}
{-# COMPILE GHC Show[WordPtr]       = AgdaShow       #-}
{-# COMPILE GHC Eq[WordPtr]         = AgdaEq         #-}
{-# COMPILE GHC Ord[WordPtr]        = AgdaOrd        #-}
