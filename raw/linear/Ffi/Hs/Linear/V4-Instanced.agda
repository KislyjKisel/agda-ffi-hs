{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.V4-Instanced where

open import Ffi.Hs.Linear.V4

instance
    inst:Functor[V4]  = Functor[V4]
    inst:Additive[V4] = Additive[V4]
    inst:Num[V4[A]]   = Num[V4[A]]
