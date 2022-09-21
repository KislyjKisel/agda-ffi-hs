{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.STM.TVar where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq)
open import Ffi.Hs.-base.Unit        using (⊤; ⊤′)
open import Ffi.Hs.Control.Monad.STM using (STM)
open import Ffi.Hs.Data.Int          using (Int)
open import Ffi.Hs.Data.Tuple        using (Tuple2)
open import Ffi.Hs.System.Mem.Weak   using (Weak)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Concurrent.STM.TVar
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ ℓ : Level
        A S : Set aℓ

postulate
    TVar : Set aℓ → Set aℓ
    Eq[TVar[A]] : Eq (TVar A)

    newTVar       : A → STM (TVar A)
    newTVarIO     : A → IO (TVar A)
    readTVar      : TVar A → STM A
    readTVarIO    : TVar A → IO A
    writeTVar     : TVar A → A → STM ⊤
    modifyTVar    : TVar A → (A → A) → STM ⊤
    modifyTVar'   : TVar A → (A → A) → STM ⊤
    stateTVar     : TVar S → (S → Tuple2 A S) → STM A
    swapTVar      : TVar A → A → STM A
    registerDelay : Int → IO (TVar Bool)
    mkWeakTVar    : TVar A → IO (⊤′ {ℓ}) → IO (Weak (TVar A))

{-# FOREIGN GHC type AgdaTVar aℓ = Control.Concurrent.STM.TVar.TVar #-}
{-# COMPILE GHC TVar = type(1) AgdaTVar #-}

{-# COMPILE GHC Eq[TVar[A]] = \ aℓ a -> AgdaEq #-}

{-# COMPILE GHC newTVar       = \ aℓ a      -> Control.Concurrent.STM.TVar.newTVar       #-}
{-# COMPILE GHC newTVarIO     = \ aℓ a      -> Control.Concurrent.STM.TVar.newTVarIO     #-}
{-# COMPILE GHC readTVar      = \ aℓ a      -> Control.Concurrent.STM.TVar.readTVar      #-}
{-# COMPILE GHC readTVarIO    = \ aℓ a      -> Control.Concurrent.STM.TVar.readTVarIO    #-}
{-# COMPILE GHC writeTVar     = \ aℓ a      -> Control.Concurrent.STM.TVar.writeTVar     #-}
{-# COMPILE GHC modifyTVar    = \ aℓ a      -> Control.Concurrent.STM.TVar.modifyTVar    #-}
{-# COMPILE GHC modifyTVar'   = \ aℓ a      -> Control.Concurrent.STM.TVar.modifyTVar'   #-}
{-# COMPILE GHC stateTVar     = \ sℓ s aℓ a -> Control.Concurrent.STM.TVar.stateTVar     #-}
{-# COMPILE GHC swapTVar      = \ aℓ a      -> Control.Concurrent.STM.TVar.swapTVar      #-}
{-# COMPILE GHC registerDelay =                Control.Concurrent.STM.TVar.registerDelay #-}
{-# COMPILE GHC mkWeakTVar    = \ aℓ a ℓ    -> Control.Concurrent.STM.TVar.mkWeakTVar    #-}
