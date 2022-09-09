{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Dynamic where

open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.Type.Reflection using (SomeTypeRep)

open Ffi.Hs.Type.Reflection public
    using (TypeRep)

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Dynamic : Set
    toDyn : ⦃ TypeRep A ⦄ → A → Dynamic
    fromDyn : ⦃ TypeRep A ⦄ → Dynamic → A → A
    fromDynamic : ⦃ TypeRep A ⦄ → Dynamic → Maybe A

    dynApply : Dynamic → Dynamic → Maybe Dynamic
    dynApp : Dynamic → Dynamic → Dynamic
    dynTypeRep : Dynamic → SomeTypeRep {kℓ = ?} -- todo: kℓ
