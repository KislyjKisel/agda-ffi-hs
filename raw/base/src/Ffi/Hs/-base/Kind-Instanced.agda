{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Kind-Instanced where

open import Ffi.Hs.-base.Kind

instance
    inst:IsKind[Set] = IsKind[Set]
    inst:IsKind[A⟶B] = IsKind[A⟶B]
