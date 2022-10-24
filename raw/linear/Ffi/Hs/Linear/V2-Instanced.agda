{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.V2-Instanced where

open import Ffi.Hs.Linear.V2

instance
    inst:Functor[V2] = Functor[V2]
