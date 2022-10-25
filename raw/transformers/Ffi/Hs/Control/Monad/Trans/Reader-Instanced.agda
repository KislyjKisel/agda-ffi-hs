{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Trans.Reader-Instanced where

open import Ffi.Hs.Control.Monad.Trans.Reader

instance
    inst:MonadTrans[ReaderT[R]]      = MonadTrans[ReaderT[R]]
    inst:Monad[ReaderT[R,M]]         = Monad[ReaderT[R,M]]
    inst:Functor[ReaderT[R,M]]       = Functor[ReaderT[R,M]]
    inst:MonadFix[ReaderT[R,M]]      = MonadFix[ReaderT[R,M]]
    inst:MonadFail[ReaderT[R,M]]     = MonadFail[ReaderT[R,M]]
    inst:Applicative[ReaderT[R,M]]   = Applicative[ReaderT[R,M]]
    inst:Contravariant[ReaderT[R,M]] = Contravariant[ReaderT[R,M]]
    inst:MonadIO[ReaderT[R,M]]       = MonadIO[ReaderT[R,M]]
    inst:Alternative[ReaderT[R,M]]   = Alternative[ReaderT[R,M]]
    inst:MonadPlus[ReaderT[R,M]]     = MonadPlus[ReaderT[R,M]]
