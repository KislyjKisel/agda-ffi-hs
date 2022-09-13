{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.StateVar where

open import Agda.Builtin.IO                    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class                 using (MonadIO; Storable; Contravariant)
open import Ffi.Hs.-base.Unit                  using (⊤)
open import Ffi.Hs.Control.Concurrent.STM.TVar using (TVar)
open import Ffi.Hs.Control.Monad.STM           using (STM)
open import Ffi.Hs.Data.IORef                  using (IORef)
open import Ffi.Hs.Foreign.ForeignPtr          using (ForeignPtr)
open import Ffi.Hs.Foreign.Ptr                 using (Ptr)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.StateVar
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        tℓ aℓ bℓ : Level
        T : Set tℓ
        A B : Set aℓ
        M : Set aℓ → Set aℓ

data StateVar (A : Set aℓ) : Set aℓ where
    mkStateVar : IO A → (A → IO (⊤ {lzero})) → StateVar A

{-# FOREIGN GHC type AgdaStateVar aℓ = Data.StateVar.StateVar #-}
{-# COMPILE GHC StateVar = data(1) AgdaStateVar (Data.StateVar.StateVar) #-}

postulate
    makeStateVar : IO A → (A → IO (⊤ {lzero})) → StateVar A
    mapStateVar  : (B → A) → (A → B) → StateVar A → StateVar B

{-# COMPILE GHC makeStateVar = \ aℓ a      -> Data.StateVar.makeStateVar #-}
{-# COMPILE GHC mapStateVar  = \ aℓ a bℓ b -> Data.StateVar.mapStateVar  #-}

postulate
    HasGetter : Set tℓ → Set aℓ → Set (tℓ ⊔ aℓ)
    get : ⦃ HasGetter T A ⦄ → ⦃ MonadIO M ⦄ → T → M A

    HasGetter[IO[A],A]         : HasGetter (IO A) A
    HasGetter[Ptr[A],A]        : ⦃ Storable A ⦄ → HasGetter (Ptr A) A
    HasGetter[ForeignPtr[A],A] : ⦃ Storable A ⦄ → HasGetter (ForeignPtr A) A
    HasGetter[STM[A],A]        : HasGetter (STM A) A
    HasGetter[TVar[A],A]       : HasGetter (TVar A) A
    HasGetter[IORef[A],A]      : HasGetter (IORef A) A
    HasGetter[StateVar[A],A]   : HasGetter (StateVar A) A

{-# FOREIGN GHC data AgdaHasGetter tℓ aℓ t a = Data.StateVar.HasGetter t a => AgdaHasGetter #-}
{-# COMPILE GHC HasGetter = type(0) AgdaHasGetter #-}

{-# COMPILE GHC get = \ tℓ t aℓ a m AgdaHasGetter AgdaMonadIO -> Data.StateVar.get #-}

{-# COMPILE GHC HasGetter[IO[A],A]         = \ aℓ a              -> AgdaHasGetter #-}
{-# COMPILE GHC HasGetter[Ptr[A],A]        = \ aℓ a AgdaStorable -> AgdaHasGetter #-}
{-# COMPILE GHC HasGetter[ForeignPtr[A],A] = \ aℓ a AgdaStorable -> AgdaHasGetter #-}
{-# COMPILE GHC HasGetter[STM[A],A]        = \ aℓ a              -> AgdaHasGetter #-}
{-# COMPILE GHC HasGetter[TVar[A],A]       = \ aℓ a              -> AgdaHasGetter #-}
{-# COMPILE GHC HasGetter[IORef[A],A]      = \ aℓ a              -> AgdaHasGetter #-}
{-# COMPILE GHC HasGetter[StateVar[A],A]   = \ aℓ a              -> AgdaHasGetter #-}

GettableStateVar : Set aℓ → Set aℓ
GettableStateVar = IO

makeGettableStateVar : IO A → GettableStateVar A
makeGettableStateVar x = x

data SettableStateVar (A : Set aℓ) : Set aℓ where
    mkSettableStateVar : (A → IO (⊤ {lzero})) → SettableStateVar A

{-# FOREIGN GHC type AgdaSettableStateVar aℓ = Data.StateVar.SettableStateVar #-}
{-# COMPILE GHC SettableStateVar = data(1) AgdaSettableStateVar (Data.StateVar.SettableStateVar) #-}

infixr 2 _$=_ _$=!_

postulate
    HasSetter : Set tℓ → Set aℓ → Set (tℓ ⊔ aℓ)
    _$=_ : ⦃ HasSetter T A ⦄ → ⦃ MonadIO M ⦄ → T → A → M ⊤
    _$=!_ : ⦃ HasSetter T A ⦄ → ⦃ MonadIO M ⦄ → T → A → M ⊤

    HasSetter[Ptr[A],A]              : ⦃ Storable A ⦄ → HasSetter (Ptr A) A
    HasSetter[ForeignPtr[A],A]       : ⦃ Storable A ⦄ → HasSetter (ForeignPtr A) A
    HasSetter[TVar[A],A]             : HasSetter (TVar A) A
    HasSetter[IORef[A],A]            : HasSetter (IORef A) A
    HasSetter[SettableStateVar[A],A] : HasSetter (SettableStateVar A) A
    HasSetter[StateVar[A],A]         : HasSetter (StateVar A) A

    Contravariant[SettableStateVar] : Contravariant {aℓ} SettableStateVar
    makeSettableStateVar : (A → IO (⊤ {lzero})) → SettableStateVar A

{-# FOREIGN GHC data AgdaHasSetter tℓ aℓ t a = Data.StateVar.HasSetter t a => AgdaHasSetter #-}
{-# COMPILE GHC HasSetter = type(0) AgdaHasSetter #-}

{-# COMPILE GHC _$=_  = \ tℓ aℓ t a m AgdaHasSetter AgdaMonadIO -> (Data.StateVar.$=)  #-}
{-# COMPILE GHC _$=!_ = \ tℓ aℓ t a m AgdaHasSetter AgdaMonadIO -> (Data.StateVar.$=!) #-}

{-# COMPILE GHC HasSetter[Ptr[A],A]              = \ aℓ a AgdaStorable -> AgdaHasSetter #-}
{-# COMPILE GHC HasSetter[ForeignPtr[A],A]       = \ aℓ a AgdaStorable -> AgdaHasSetter #-}
{-# COMPILE GHC HasSetter[TVar[A],A]             = \ aℓ a              -> AgdaHasSetter #-}
{-# COMPILE GHC HasSetter[IORef[A],A]            = \ aℓ a              -> AgdaHasSetter #-}
{-# COMPILE GHC HasSetter[SettableStateVar[A],A] = \ aℓ a              -> AgdaHasSetter #-}
{-# COMPILE GHC HasSetter[StateVar[A],A]         = \ aℓ a              -> AgdaHasSetter #-}

{-# COMPILE GHC Contravariant[SettableStateVar] = \ aℓ -> AgdaContravariant #-}
{-# COMPILE GHC makeSettableStateVar = \ aℓ a -> Data.StateVar.makeSettableStateVar #-}

infixr 2 _$~_ _$~!_

postulate
    HasUpdate : Set tℓ → Set aℓ → Set bℓ → Set (tℓ ⊔ aℓ ⊔ bℓ)
    _$~_  : ⦃ HasUpdate T A B ⦄ → ⦃ MonadIO M ⦄ → T → (A → B) → M ⊤
    _$~!_ : ⦃ HasUpdate T A B ⦄ → ⦃ MonadIO M ⦄ → T → (A → B) → M ⊤

    HasUpdate[Ptr[A],A,A]        : ⦃ Storable A ⦄ → HasUpdate (Ptr A) A A
    HasUpdate[ForeignPtr[A],A,A] : ⦃ Storable A ⦄ → HasUpdate (ForeignPtr A) A A
    HasUpdate[TVar[A],A,A]       : HasUpdate (TVar A) A A
    HasUpdate[IORef[A],A,A]      : HasUpdate (IORef A) A A
    HasUpdate[StateVar[A],A,A]   : HasUpdate (StateVar A) A A

    HasUpdate[T,A,B]⇒HasSetter[T,B] : ⦃ HasUpdate T A B ⦄ → HasSetter T B

{-# FOREIGN GHC data AgdaHasUpdate tℓ aℓ bℓ t a b = Data.StateVar.HasUpdate t a b => AgdaHasUpdate #-}
{-# COMPILE GHC HasUpdate = type(0) AgdaHasUpdate #-}

{-# COMPILE GHC _$~_  = \ tℓ aℓ bℓ t a b m AgdaHasUpdate AgdaMonadIO -> (Data.StateVar.$~)  #-}
{-# COMPILE GHC _$~!_ = \ tℓ aℓ bℓ t a b m AgdaHasUpdate AgdaMonadIO -> (Data.StateVar.$~!) #-}

{-# COMPILE GHC HasUpdate[Ptr[A],A,A]        = \ aℓ a AgdaStorable -> AgdaHasUpdate #-}
{-# COMPILE GHC HasUpdate[ForeignPtr[A],A,A] = \ aℓ a AgdaStorable -> AgdaHasUpdate #-}
{-# COMPILE GHC HasUpdate[TVar[A],A,A]       = \ aℓ a -> AgdaHasUpdate #-}
{-# COMPILE GHC HasUpdate[IORef[A],A,A]      = \ aℓ a -> AgdaHasUpdate #-}
{-# COMPILE GHC HasUpdate[StateVar[A],A,A]   = \ aℓ a -> AgdaHasUpdate #-}

{-# COMPILE GHC HasUpdate[T,A,B]⇒HasSetter[T,B] = \ tℓ t aℓ a bℓ b AgdaHasUpdate -> AgdaHasSetter #-}
