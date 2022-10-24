{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Primitive-Instanced where

open import Ffi.Hs.Control.Monad.Primitive

instance
    inst:PrimMonad[IO]  = PrimMonad[IO]
    inst:PrimMonad[SST] = PrimMonad[SST]
    inst:PrimMonad[LST] = PrimMonad[LST]
