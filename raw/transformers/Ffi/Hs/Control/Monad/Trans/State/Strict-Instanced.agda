{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Trans.State.Strict-Instanced where

open import Ffi.Hs.Control.Monad.Trans.State.Strict

instance
    inst:MonadTrans[StateT[S]]      = MonadTrans[StateT[S]]
    inst:Monad[StateT[S,M]]         = Monad[StateT[S,M]]
    inst:Functor[StateT[S,M]]       = Functor[StateT[S,M]]
    inst:MonadFix[StateT[S,M]]      = MonadFix[StateT[S,M]]
    inst:MonadFail[StateT[S,M]]     = MonadFail[StateT[S,M]]
    inst:Applicative[StateT[S,M]]   = Applicative[StateT[S,M]]
    inst:Contravariant[StateT[S,M]] = Contravariant[StateT[S,M]]
    inst:MonadIO[StateT[S,M]]       = MonadIO[StateT[S,M]]
    inst:Alternative[StateT[S,M]]   = Alternative[StateT[S,M]]
    inst:MonadPlus[StateT[S,M]]     = MonadPlus[StateT[S,M]]
