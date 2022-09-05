{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Ptr where

open import Agda.Builtin.IO    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Storable)
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Word   using (Word)

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

module Instances where
    postulate

open Instances public
