{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.STM.TChan where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq)
open import Ffi.Hs.-base.Unit        using (⊤)
open import Ffi.Hs.Control.Monad.STM using (STM)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Concurrent.STM.TChan
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    TChan : Set aℓ → Set aℓ
    Eq[TChan[A]] : Eq (TChan A)

    newTChan            : STM (TChan A)
    newTChanIO          : IO (TChan A)
    newBroadcastTChan   : STM (TChan A)
    newBroadcastTChanIO : IO (TChan A)
    dupTChan            : TChan A → STM (TChan A)
    cloneTChan          : TChan A → STM (TChan A)
    readTChan           : TChan A → STM A
    tryReadTChan        : TChan A → STM (Maybe A)
    peekTChan           : TChan A → STM A
    tryPeekTChan        : TChan A → STM (Maybe A)
    writeTChan          : TChan A → A → STM (⊤ {lzero})
    unGetTChan          : TChan A → A → STM (⊤ {lzero})
    isEmptyTChan        : TChan A → STM Bool

{-# FOREIGN GHC type AgdaTChan aℓ = Control.Concurrent.STM.TChan.TChan #-}
{-# COMPILE GHC TChan = type(1) AgdaTChan #-}

{-# COMPILE GHC Eq[TChan[A]] = \ aℓ a -> AgdaEq #-}

{-# COMPILE GHC newTChan            = \ aℓ a -> Control.Concurrent.STM.TChan.newTChan            #-}
{-# COMPILE GHC newTChanIO          = \ aℓ a -> Control.Concurrent.STM.TChan.newTChanIO          #-}
{-# COMPILE GHC newBroadcastTChan   = \ aℓ a -> Control.Concurrent.STM.TChan.newBroadcastTChan   #-}
{-# COMPILE GHC newBroadcastTChanIO = \ aℓ a -> Control.Concurrent.STM.TChan.newBroadcastTChanIO #-}
{-# COMPILE GHC dupTChan            = \ aℓ a -> Control.Concurrent.STM.TChan.dupTChan            #-}
{-# COMPILE GHC cloneTChan          = \ aℓ a -> Control.Concurrent.STM.TChan.cloneTChan          #-}
{-# COMPILE GHC readTChan           = \ aℓ a -> Control.Concurrent.STM.TChan.readTChan           #-}
{-# COMPILE GHC tryReadTChan        = \ aℓ a -> Control.Concurrent.STM.TChan.tryReadTChan        #-}
{-# COMPILE GHC peekTChan           = \ aℓ a -> Control.Concurrent.STM.TChan.peekTChan           #-}
{-# COMPILE GHC tryPeekTChan        = \ aℓ a -> Control.Concurrent.STM.TChan.tryPeekTChan        #-}
{-# COMPILE GHC writeTChan          = \ aℓ a -> Control.Concurrent.STM.TChan.writeTChan          #-}
{-# COMPILE GHC unGetTChan          = \ aℓ a -> Control.Concurrent.STM.TChan.unGetTChan          #-}
{-# COMPILE GHC isEmptyTChan        = \ aℓ a -> Control.Concurrent.STM.TChan.isEmptyTChan        #-}
