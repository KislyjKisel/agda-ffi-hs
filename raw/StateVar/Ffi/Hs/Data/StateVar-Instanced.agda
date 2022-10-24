{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.StateVar-Instanced where

open import Ffi.Hs.Data.StateVar

instance
    inst:HasGetter[IO[A],A]         = HasGetter[IO[A],A]
    inst:HasGetter[Ptr[A],A]        = HasGetter[Ptr[A],A]
    inst:HasGetter[ForeignPtr[A],A] = HasGetter[ForeignPtr[A],A]
    inst:HasGetter[STM[A],A]        = HasGetter[STM[A],A]
    inst:HasGetter[TVar[A],A]       = HasGetter[TVar[A],A]
    inst:HasGetter[IORef[A],A]      = HasGetter[IORef[A],A]
    inst:HasGetter[StateVar[A],A]   = HasGetter[StateVar[A],A]

    inst:HasSetter[Ptr[A],A]              = HasSetter[Ptr[A],A]
    inst:HasSetter[ForeignPtr[A],A]       = HasSetter[ForeignPtr[A],A]
    inst:HasSetter[TVar[A],A]             = HasSetter[TVar[A],A]
    inst:HasSetter[IORef[A],A]            = HasSetter[IORef[A],A]
    inst:HasSetter[SettableStateVar[A],A] = HasSetter[SettableStateVar[A],A]
    inst:HasSetter[StateVar[A],A]         = HasSetter[StateVar[A],A]

    inst:HasUpdate[Ptr[A],A,A]        = HasUpdate[Ptr[A],A,A]
    inst:HasUpdate[ForeignPtr[A],A,A] = HasUpdate[ForeignPtr[A],A,A]
    inst:HasUpdate[TVar[A],A,A]       = HasUpdate[TVar[A],A,A]
    inst:HasUpdate[IORef[A],A,A]      = HasUpdate[IORef[A],A,A]
    inst:HasUpdate[StateVar[A],A,A]   = HasUpdate[StateVar[A],A,A]
