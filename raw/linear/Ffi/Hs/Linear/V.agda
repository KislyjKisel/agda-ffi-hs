{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.V where

open import Ffi.Hs.-base.Class
open import Ffi.Hs.GHC.TypeNats as TypeNats using (`Nat)
open import Ffi.Hs.Data.Int using (Int)
open import Agda.Primitive
open import Ffi.Hs.Data.Vector using (Vector)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.V
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        F : Set aℓ → Set aℓ
        N : Set

postulate
    Dim : (N : Set) → Set
    Dim[N]⇒`Nat[N] : ⦃ Dim N ⦄ → `Nat N
    reflectDim : ∀{pℓ} {P : Set → Set pℓ} → ⦃ Dim N ⦄ → P N → Int

    Finite : (Set aℓ → Set aℓ) → Set aℓ

    Size : (F : Set aℓ → Set aℓ) → Set
    `Nat[Size[F]] : `Nat (Size F)

    Dim[0] : Dim TypeNats.0#
    Dim[1] : Dim TypeNats.1#
    Dim[2] : Dim TypeNats.2#
    Dim[3] : Dim TypeNats.3#
    Dim[4] : Dim TypeNats.4#
    Dim[5] : Dim TypeNats.5#

record V (N : Set) (A : Set aℓ) : Set aℓ where
    constructor mkV
    field
        toVector : Vector A

open V public

postulate
    toV : F A → V (Size F) A
    fromV : V (Size F) A → F A

    dim : ⦃ Dim N ⦄ → V N A → Int

    reifyDim       : Int → (∀{N} → ⦃ Dim N ⦄ → Proxy n → r) → r
    reifyVector    : forall a r. Vector a → (forall (n : Type). Dim n => V n a → r) → r
    reifyDimNat    : Int → (forall (n : Nat). KnownNat n => Proxy n → r) → r
    reifyVectorNat : forall a r. Vector a → (forall (n : Nat). KnownNat n => V n a → r) → r
    fromVector     : forall n a. Dim n => Vector a → Maybe (V n a) 