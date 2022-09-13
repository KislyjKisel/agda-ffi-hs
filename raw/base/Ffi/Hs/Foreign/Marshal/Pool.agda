{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Marshal.Pool where

open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Storable)
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Foreign.Ptr using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.Marshal.Pool
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Pool     : Set
    newPool  : IO Pool
    freePool : Pool → IO (⊤ {lzero})
    withPool : (Pool → IO A) → IO A

    pooledMalloc        : ⦃ Storable A ⦄ → Pool → IO (Ptr A)
    pooledMallocBytes   : Pool → Int → IO (Ptr A)
    pooledRealloc       : ⦃ Storable A ⦄ → Pool → Ptr A → IO (Ptr A)
    pooledReallocBytes  : Pool → Ptr A → Int → IO (Ptr A)
    pooledMallocArray   : ⦃ Storable A ⦄ → Pool → Int → IO (Ptr A)
    pooledMallocArray0  : ⦃ Storable A ⦄ → Pool → Int → IO (Ptr A)
    pooledReallocArray  : ⦃ Storable A ⦄ → Pool → Ptr A → Int → IO (Ptr A)
    pooledReallocArray0 : ⦃ Storable A ⦄ → Pool → Ptr A → Int → IO (Ptr A)

    pooledNew       : ⦃ Storable A ⦄ → Pool → A → IO (Ptr A)
    pooledNewArray  : ⦃ Storable A ⦄ → Pool → List A → IO (Ptr A)
    pooledNewArray0 : ⦃ Storable A ⦄ → Pool → A → List A → IO (Ptr A)

{-# COMPILE GHC freePool =           Foreign.Marshal.Pool.freePool #-}
{-# COMPILE GHC newPool  =           Foreign.Marshal.Pool.newPool  #-}
{-# COMPILE GHC Pool     = type      Foreign.Marshal.Pool.Pool     #-}
{-# COMPILE GHC withPool = \ aℓ a -> Foreign.Marshal.Pool.withPool #-}

{-# COMPILE GHC pooledMalloc        = \ aℓ a AgdaStorable -> Foreign.Marshal.Pool.pooledMalloc        #-}
{-# COMPILE GHC pooledMallocBytes   = \ aℓ a              -> Foreign.Marshal.Pool.pooledMallocBytes   #-}
{-# COMPILE GHC pooledRealloc       = \ aℓ a AgdaStorable -> Foreign.Marshal.Pool.pooledRealloc       #-}
{-# COMPILE GHC pooledReallocBytes  = \ aℓ a              -> Foreign.Marshal.Pool.pooledReallocBytes  #-}
{-# COMPILE GHC pooledMallocArray   = \ aℓ a AgdaStorable -> Foreign.Marshal.Pool.pooledMallocArray   #-}
{-# COMPILE GHC pooledMallocArray0  = \ aℓ a AgdaStorable -> Foreign.Marshal.Pool.pooledMallocArray0  #-}
{-# COMPILE GHC pooledReallocArray  = \ aℓ a AgdaStorable -> Foreign.Marshal.Pool.pooledReallocArray  #-}
{-# COMPILE GHC pooledReallocArray0 = \ aℓ a AgdaStorable -> Foreign.Marshal.Pool.pooledReallocArray0 #-}

{-# COMPILE GHC pooledNew       = \ aℓ a AgdaStorable -> Foreign.Marshal.Pool.pooledNew       #-}
{-# COMPILE GHC pooledNewArray  = \ aℓ a AgdaStorable -> Foreign.Marshal.Pool.pooledNewArray  #-}
{-# COMPILE GHC pooledNewArray0 = \ aℓ a AgdaStorable -> Foreign.Marshal.Pool.pooledNewArray0 #-}
