{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.V0 where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Linear.Vector using (Additive)
open import Ffi.Hs.Linear.Metric using (Metric)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.V0
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level


data V0 (A : Set aℓ) : Set aℓ where
    mkV0 : V0 A

{-# FOREIGN GHC type AgdaV0 aℓ = Linear.V0.V0 #-}
{-# COMPILE GHC V0 = data(1) AgdaV0 (Linear.V0.V0) #-}

postulate
    Monad[V0]         : Monad {aℓ} V0
    Functor[V0]       : Functor {aℓ} V0
    MonadFix[V0]      : MonadFix {aℓ} V0
    Applicative[V0]   : Applicative {aℓ} V0
    Foldable[V0]      : Foldable {aℓ} V0
    Traversable[V0]   : Traversable {aℓ} V0
    Eq1[V0]           : Eq1 {aℓ} V0
    Ord1[V0]          : Ord1 {aℓ} V0
    Read1[V0]         : Read1 {aℓ} V0
    Show1[V0]         : Show1 {aℓ} V0
    MonadZip[V0]      : MonadZip {aℓ} V0
    Additive[V0]      : Additive {aℓ} V0
    Metric[V0]        : Metric {aℓ} V0
    Finite[V0]        : Finite {aℓ} V0
    Bounded[V0[A]]    : Bounded (V0 A)
    Enum[V0[A]]       : Enum (V0 A)
    Eq[V0[A]]         : Eq (V0 A)
    Floating[V0[A]]   : Floating (V0 A)
    Fractional[V0[A]] : Fractional (V0 A)
    Data[V0[A]]       : ⦃ Data A ⦄ → Data (V0 A)
    Num[V0[A]]        : Num (V0 A)
    Ord[V0[A]]        : Ord (V0 A)
    Read[V0[A]]       : Read (V0 A)
    Show[V0[A]]       : Show (V0 A)
    Ix[V0[A]]         : Ix (V0 A)
    Semigroup[V0[A]]  : Semigroup (V0 A)
    Monoid[V0[A]]     : Monoid (V0 A)
    Storable[V0[A]]   : Storable (V0 A)
    NFData[V0[A]]     : NFData (V0 A)
    Epsilon[V0[A]]    : Epsilon (V0 A)

-- todo: instances of
-- Binary, Serial, Serialize, Hashable, Distributive, Apply
-- Representable, Serial1, Hashable1, Bind, Vector, MVector
-- Unbox, Ixed, Random, FunctorWithIndex, FoldableWithIndex
-- TraversableWithIndex, Each
