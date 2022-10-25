{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Trans.Except-Instanced where

open import Ffi.Hs.Control.Monad.Trans.Except

instance
    inst:MonadTrans[ExceptT[E]]      = MonadTrans[ExceptT[E]]
    inst:Monad[ExceptT[E,M]]         = Monad[ExceptT[E,M]]
    inst:Functor[ExceptT[E,M]]       = Functor[ExceptT[E,M]]
    inst:MonadFix[ExceptT[E,M]]      = MonadFix[ExceptT[E,M]]
    inst:MonadFail[ExceptT[E,M]]     = MonadFail[ExceptT[E,M]]
    inst:Applicative[ExceptT[E,M]]   = Applicative[ExceptT[E,M]]
    inst:Foldable[ExceptT[E,M]]      = Foldable[ExceptT[E,M]]
    inst:Traversable[ExceptT[E,M]]   = Traversable[ExceptT[E,M]]
    inst:Contravariant[ExceptT[E,M]] = Contravariant[ExceptT[E,M]]
    inst:Eq1[ExceptT[E,M]]           = Eq1[ExceptT[E,M]]
    inst:Ord1[ExceptT[E,M]]          = Ord1[ExceptT[E,M]]
    inst:Read1[ExceptT[E,M]]         = Read1[ExceptT[E,M]]
    inst:Show1[ExceptT[E,M]]         = Show1[ExceptT[E,M]]
    inst:MonadZip[ExceptT[E,M]]      = MonadZip[ExceptT[E,M]]
    inst:MonadIO[ExceptT[E,M]]       = MonadIO[ExceptT[E,M]]
    inst:Alternative[ExceptT[E,M]]   = Alternative[ExceptT[E,M]]
    inst:MonadPlus[ExceptT[E,M]]     = MonadPlus[ExceptT[E,M]]
    inst:Eq[ExceptT[E,M,A]]          = Eq[ExceptT[E,M,A]]
    inst:Ord[ExceptT[E,M,A]]         = Ord[ExceptT[E,M,A]]
    inst:Read[ExceptT[E,M,A]]        = Read[ExceptT[E,M,A]]
    inst:Show[ExceptT[E,M,A]]        = Show[ExceptT[E,M,A]]
