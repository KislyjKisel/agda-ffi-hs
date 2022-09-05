{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.MVar where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.IO        using (IO)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Eq)
open import Ffi.Hs.-base.Unit      using (⊤)
open import Ffi.Hs.Data.Tuple      using (Tuple2)
open import Ffi.Hs.System.Mem.Weak using (Weak)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    MVar : Set aℓ → Set aℓ

    newEmptyMVar     : IO (MVar A)
    newMVar          : A → IO (MVar A)
    takeMVar         : MVar A → IO A
    putMVar          : MVar A → A → IO (⊤ {lzero})
    readMVar         : MVar A → IO A
    swapMVar         : MVar A → A → IO A
    tryTakeMVar      : MVar A → IO (Maybe A)
    tryPutMVar       : MVar A → A → IO Bool
    isEmptyMVar      : MVar A → IO Bool
    withMVar         : MVar A → (A → IO B) → IO B
    withMVarMasked   : MVar A → (A → IO B) → IO B
    modifyMVar       : MVar A → (A → IO (Tuple2 A B)) → IO B
    modifyMVarMasked : MVar A → (A → IO (Tuple2 A B)) → IO B
    tryReadMVar      : MVar A → IO (Maybe A)
    mkWeakMVar       : MVar A → IO (⊤ {bℓ}) → IO (Weak (MVar A))


{-# FOREIGN GHC import qualified Control.Concurrent.MVar #-}

{-# FOREIGN GHC type AgdaMVar ℓ = Control.Concurrent.MVar.MVar #-}
{-# COMPILE GHC MVar = type(1) AgdaMVar #-}

{-# COMPILE GHC newEmptyMVar     = \ aℓ a      -> Control.Concurrent.MVar.newEmptyMVar     #-}
{-# COMPILE GHC newMVar          = \ aℓ a      -> Control.Concurrent.MVar.newMVar          #-}
{-# COMPILE GHC takeMVar         = \ aℓ a      -> Control.Concurrent.MVar.takeMVar         #-}
{-# COMPILE GHC putMVar          = \ aℓ a      -> Control.Concurrent.MVar.putMVar          #-}
{-# COMPILE GHC readMVar         = \ aℓ a      -> Control.Concurrent.MVar.readMVar         #-}
{-# COMPILE GHC swapMVar         = \ aℓ a      -> Control.Concurrent.MVar.swapMVar         #-}
{-# COMPILE GHC tryTakeMVar      = \ aℓ a      -> Control.Concurrent.MVar.tryTakeMVar      #-}
{-# COMPILE GHC tryPutMVar       = \ aℓ a      -> Control.Concurrent.MVar.tryPutMVar       #-}
{-# COMPILE GHC isEmptyMVar      = \ aℓ a      -> Control.Concurrent.MVar.isEmptyMVar      #-}
{-# COMPILE GHC withMVar         = \ aℓ bℓ a b -> Control.Concurrent.MVar.withMVar         #-}
{-# COMPILE GHC withMVarMasked   = \ aℓ bℓ a b -> Control.Concurrent.MVar.withMVarMasked   #-}
{-# COMPILE GHC modifyMVar       = \ aℓ bℓ a b -> Control.Concurrent.MVar.modifyMVar       #-}
{-# COMPILE GHC modifyMVarMasked = \ aℓ bℓ a b -> Control.Concurrent.MVar.modifyMVarMasked #-}
{-# COMPILE GHC tryReadMVar      = \ aℓ    a   -> Control.Concurrent.MVar.tryReadMVar      #-}
{-# COMPILE GHC mkWeakMVar       = \ aℓ bℓ a   -> Control.Concurrent.MVar.mkWeakMVar       #-}

postulate
    Eq[MVar[A]] : Eq (MVar A)

{-# FOREIGN GHC import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaEq) #-}
{-# COMPILE GHC Eq[MVar[A]] = \ aℓ a -> AgdaEq #-}
