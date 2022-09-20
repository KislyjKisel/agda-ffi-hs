{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.STM.TMVar where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq)
open import Ffi.Hs.-base.Unit        using (⊤; ⊤′)
open import Ffi.Hs.Control.Monad.STM using (STM)
open import Ffi.Hs.System.Mem.Weak   using (Weak)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Concurrent.STM.TMVar
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ ℓ : Level
        A : Set aℓ

postulate
    TMVar : Set aℓ → Set aℓ
    Eq[TMVar[A]] : Eq (TMVar A)

    newTMVar        : A → STM (TMVar A)
    newEmptyTMVar   : STM (TMVar A)
    newTMVarIO      : A → IO (TMVar A)
    newEmptyTMVarIO : IO (TMVar A)
    takeTMVar       : TMVar A → STM A
    putTMVar        : TMVar A → A → STM ⊤
    readTMVar       : TMVar A → STM A
    writeTMVar      : TMVar A → A → STM ⊤
    tryReadTMVar    : TMVar A → STM (Maybe A)
    swapTMVar       : TMVar A → A → STM A
    tryTakeTMVar    : TMVar A → STM (Maybe A)
    tryPutTMVar     : TMVar A → A → STM Bool
    isEmptyTMVar    : TMVar A → STM Bool
    mkWeakTMVar     : TMVar A → IO (⊤′ {ℓ}) → IO (Weak (TMVar A))

{-# FOREIGN GHC type AgdaTMVar aℓ = Control.Concurrent.STM.TMVar.TMVar #-}
{-# COMPILE GHC TMVar = type(1) AgdaTMVar #-}

{-# COMPILE GHC Eq[TMVar[A]] = \ aℓ a -> AgdaEq #-}

{-# COMPILE GHC newTMVar        = \ aℓ a   -> Control.Concurrent.STM.TMVar.newTMVar        #-}
{-# COMPILE GHC newEmptyTMVar   = \ aℓ a   -> Control.Concurrent.STM.TMVar.newEmptyTMVar   #-}
{-# COMPILE GHC newTMVarIO      = \ aℓ a   -> Control.Concurrent.STM.TMVar.newTMVarIO      #-}
{-# COMPILE GHC newEmptyTMVarIO = \ aℓ a   -> Control.Concurrent.STM.TMVar.newEmptyTMVarIO #-}
{-# COMPILE GHC takeTMVar       = \ aℓ a   -> Control.Concurrent.STM.TMVar.takeTMVar       #-}
{-# COMPILE GHC putTMVar        = \ aℓ a   -> Control.Concurrent.STM.TMVar.putTMVar        #-}
{-# COMPILE GHC readTMVar       = \ aℓ a   -> Control.Concurrent.STM.TMVar.readTMVar       #-}
{-# COMPILE GHC writeTMVar      = \ aℓ a   -> Control.Concurrent.STM.TMVar.writeTMVar      #-}
{-# COMPILE GHC tryReadTMVar    = \ aℓ a   -> Control.Concurrent.STM.TMVar.tryReadTMVar    #-}
{-# COMPILE GHC swapTMVar       = \ aℓ a   -> Control.Concurrent.STM.TMVar.swapTMVar       #-}
{-# COMPILE GHC tryTakeTMVar    = \ aℓ a   -> Control.Concurrent.STM.TMVar.tryTakeTMVar    #-}
{-# COMPILE GHC tryPutTMVar     = \ aℓ a   -> Control.Concurrent.STM.TMVar.tryPutTMVar     #-}
{-# COMPILE GHC isEmptyTMVar    = \ aℓ a   -> Control.Concurrent.STM.TMVar.isEmptyTMVar    #-}
{-# COMPILE GHC mkWeakTMVar     = \ aℓ a ℓ -> Control.Concurrent.STM.TMVar.mkWeakTMVar     #-}
