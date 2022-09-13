{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.STM.TBQueue where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq)
open import Ffi.Hs.-base.Unit        using (⊤)
open import Ffi.Hs.Control.Monad.STM using (STM)
open import Ffi.Hs.Numeric.Natural   using (Natural)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Concurrent.STM.TBQueue
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    TBQueue : Set aℓ → Set aℓ
    Eq[TBQueue[A]] : Eq (TBQueue A)

    newTBQueue     : Natural → STM (TBQueue A)
    newTBQueueIO   : Natural → IO (TBQueue A)
    readTBQueue    : TBQueue A → STM A
    tryReadTBQueue : TBQueue A → STM (Maybe A)
    flushTBQueue   : TBQueue A → STM (List A)
    peekTBQueue    : TBQueue A → STM A
    tryPeekTBQueue : TBQueue A → STM (Maybe A)
    writeTBQueue   : TBQueue A → A → STM (⊤ {lzero})
    unGetTBQueue   : TBQueue A → A → STM (⊤ {lzero})
    lengthTBQueue  : TBQueue A → STM Natural
    isEmptyTBQueue : TBQueue A → STM Bool
    isFullTBQueue  : TBQueue A → STM Bool

{-# FOREIGN GHC type AgdaTBQueue aℓ = Control.Concurrent.STM.TBQueue.TBQueue #-}
{-# COMPILE GHC TBQueue = type(1) AgdaTBQueue #-}

{-# COMPILE GHC Eq[TBQueue[A]] = \ aℓ a -> AgdaEq #-}

{-# COMPILE GHC newTBQueue     = \ aℓ a -> Control.Concurrent.STM.TBQueue.newTBQueue     #-}
{-# COMPILE GHC newTBQueueIO   = \ aℓ a -> Control.Concurrent.STM.TBQueue.newTBQueueIO   #-}
{-# COMPILE GHC readTBQueue    = \ aℓ a -> Control.Concurrent.STM.TBQueue.readTBQueue    #-}
{-# COMPILE GHC tryReadTBQueue = \ aℓ a -> Control.Concurrent.STM.TBQueue.tryReadTBQueue #-}
{-# COMPILE GHC flushTBQueue   = \ aℓ a -> Control.Concurrent.STM.TBQueue.flushTBQueue   #-}
{-# COMPILE GHC peekTBQueue    = \ aℓ a -> Control.Concurrent.STM.TBQueue.peekTBQueue    #-}
{-# COMPILE GHC tryPeekTBQueue = \ aℓ a -> Control.Concurrent.STM.TBQueue.tryPeekTBQueue #-}
{-# COMPILE GHC writeTBQueue   = \ aℓ a -> Control.Concurrent.STM.TBQueue.writeTBQueue   #-}
{-# COMPILE GHC unGetTBQueue   = \ aℓ a -> Control.Concurrent.STM.TBQueue.unGetTBQueue   #-}
{-# COMPILE GHC lengthTBQueue  = \ aℓ a -> Control.Concurrent.STM.TBQueue.lengthTBQueue  #-}
{-# COMPILE GHC isEmptyTBQueue = \ aℓ a -> Control.Concurrent.STM.TBQueue.isEmptyTBQueue #-}
{-# COMPILE GHC isFullTBQueue  = \ aℓ a -> Control.Concurrent.STM.TBQueue.isFullTBQueue  #-}
