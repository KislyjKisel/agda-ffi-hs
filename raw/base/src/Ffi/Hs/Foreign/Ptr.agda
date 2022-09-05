{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Ptr where

open import Agda.Builtin.IO using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Storable)
open import Ffi.Hs.Data.Int using (Int)
open import Ffi.Hs.-base.Unit  using (⊤)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    Ptr : Set aℓ → Set aℓ
    nullPtr : Ptr A
    castPtr : Ptr A → Ptr B
    plusPtr : Ptr A → Int → Ptr B
    alignPtr : Ptr A → Int → Ptr A
    minusPtr : Ptr A → Ptr B → Int

    FunPtr : Set aℓ → Set bℓ
    nullFunPtr : FunPtr A
    castFunPtr : FunPtr A → FunPtr B
    castFunPtrToPtr : FunPtr A → Ptr B
    castPtrToFunPtr : Ptr A → FunPtr B
    freeHaskellFunPtr : FunPtr A → IO (⊤ {lzero})

module Instances where
    postulate
