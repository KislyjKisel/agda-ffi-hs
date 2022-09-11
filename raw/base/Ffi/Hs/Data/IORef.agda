{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.IORef where

open import Agda.Builtin.IO        using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit      using (⊤)
open import Ffi.Hs.Data.Tuple      using (Tuple2)
open import Ffi.Hs.System.Mem.Weak using (Weak)

{-# FOREIGN GHC
import qualified Data.IORef
#-}

private
    variable
        aℓ ℓ : Level
        A B : Set aℓ

postulate
    IORef : Set aℓ → Set aℓ
    newIORef           : A → IO (IORef A)
    readIORef          : IORef A → IO A
    writeIORef         : IORef A → A → IO A
    modifyIORef        : IORef A → (A → A) → IO (⊤ {lzero})
    modifyIORef'       : IORef A → (A → A) → IO (⊤ {lzero})
    atomicModifyIORef  : IORef A → (A → Tuple2 A B) → IO B
    atomicModifyIORef' : IORef A → (A → Tuple2 A B) → IO B
    atomicWriteIORef   : IORef A → A → IO (⊤ {lzero})
    mkWeakIORef        : IORef A → IO (⊤ {ℓ}) → IO (Weak (IORef A))

{-# FOREIGN GHC type AgdaIORef aℓ = Data.IORef.IORef #-}
{-# COMPILE GHC IORef = type(1) AgdaIORef #-}

{-# COMPILE GHC newIORef           = \ aℓ a      -> Data.IORef.newIORef           #-}
{-# COMPILE GHC readIORef          = \ aℓ a      -> Data.IORef.readIORef          #-}
{-# COMPILE GHC writeIORef         = \ aℓ a      -> Data.IORef.writeIORef         #-}
{-# COMPILE GHC modifyIORef        = \ aℓ a      -> Data.IORef.modifyIORef        #-}
{-# COMPILE GHC modifyIORef'       = \ aℓ a      -> Data.IORef.modifyIORef'       #-}
{-# COMPILE GHC atomicModifyIORef  = \ aℓ a bℓ b -> Data.IORef.atomicModifyIORef  #-}
{-# COMPILE GHC atomicModifyIORef' = \ aℓ a bℓ b -> Data.IORef.atomicModifyIORef' #-}
{-# COMPILE GHC atomicWriteIORef   = \ aℓ a      -> Data.IORef.atomicWriteIORef   #-}
{-# COMPILE GHC mkWeakIORef        = \ aℓ a ℓ    -> Data.IORef.mkWeakIORef        #-}
