{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Binary.Put-Instanced where

open import Ffi.Hs.Data.Binary.Put

instance
    inst:Monad[PutM]       = Monad[PutM]
    inst:Functor[PutM]     = Functor[PutM]
    inst:Applicative[PutM] = Applicative[PutM]
    inst:Semigroup[Put]    = Semigroup[Put]
    inst:Monoid[Put]       = Monoid[Put]
