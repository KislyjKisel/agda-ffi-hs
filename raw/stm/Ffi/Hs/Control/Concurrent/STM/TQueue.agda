{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.STM.TQueue where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq)
open import Ffi.Hs.-base.Unit        using (⊤)
open import Ffi.Hs.Control.Monad.STM using (STM)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Concurrent.STM.TQueue
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    TQueue : Set aℓ → Set aℓ
    Eq[TQueue[A]] : Eq (TQueue A)

    newTQueue     : STM (TQueue A)
    newTQueueIO   : IO (TQueue A)
    readTQueue    : TQueue A → STM A
    tryReadTQueue : TQueue A → STM (Maybe A)
    flushTQueue   : TQueue A → STM (List A)
    peekTQueue    : TQueue A → STM A
    tryPeekTQueue : TQueue A → STM (Maybe A)
    writeTQueue   : TQueue A → A → STM (⊤ {lzero})
    unGetTQueue   : TQueue A → A → STM (⊤ {lzero})
    isEmptyTQueue : TQueue A → STM Bool

{-# FOREIGN GHC type AgdaTQueue aℓ = Control.Concurrent.STM.TQueue.TQueue #-}
{-# COMPILE GHC TQueue = type(1) AgdaTQueue #-}

{-# COMPILE GHC Eq[TQueue[A]] = \ aℓ a -> AgdaEq #-}

{-# COMPILE GHC newTQueue     = \ aℓ a -> Control.Concurrent.STM.TQueue.newTQueue     #-}
{-# COMPILE GHC newTQueueIO   = \ aℓ a -> Control.Concurrent.STM.TQueue.newTQueueIO   #-}
{-# COMPILE GHC readTQueue    = \ aℓ a -> Control.Concurrent.STM.TQueue.readTQueue    #-}
{-# COMPILE GHC tryReadTQueue = \ aℓ a -> Control.Concurrent.STM.TQueue.tryReadTQueue #-}
{-# COMPILE GHC flushTQueue   = \ aℓ a -> Control.Concurrent.STM.TQueue.flushTQueue   #-}
{-# COMPILE GHC peekTQueue    = \ aℓ a -> Control.Concurrent.STM.TQueue.peekTQueue    #-}
{-# COMPILE GHC tryPeekTQueue = \ aℓ a -> Control.Concurrent.STM.TQueue.tryPeekTQueue #-}
{-# COMPILE GHC writeTQueue   = \ aℓ a -> Control.Concurrent.STM.TQueue.writeTQueue   #-}
{-# COMPILE GHC unGetTQueue   = \ aℓ a -> Control.Concurrent.STM.TQueue.unGetTQueue   #-}
{-# COMPILE GHC isEmptyTQueue = \ aℓ a -> Control.Concurrent.STM.TQueue.isEmptyTQueue #-}
