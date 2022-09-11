{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Contravariant where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Ord    using (Ordering)

open import Ffi.Hs.-base.Class public
    using (Contravariant)

{-# FOREIGN GHC
import qualified Data.Functor.Contravariant
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A B : Set aℓ
        F : Set aℓ → Set aℓ

infixl 4 _>$_ _>$<_ _>$$<_ _$<_

postulate
    contramap : ⦃ Contravariant F ⦄ → (A → B) → F B → F A
    _>$_      : ⦃ Contravariant F ⦄ → B → F B → F A

    phantom : ⦃ Functor F ⦄ → ⦃ Contravariant F ⦄ → F A → F B
    _>$<_   : ⦃ Contravariant F ⦄ → (A → B) → F B → F A
    _>$$<_  : ⦃ Contravariant F ⦄ → F B → (A → B) → F A
    _$<_    : ⦃ Contravariant F ⦄ → F B → B → F A

{-# COMPILE GHC contramap = \ aℓ f a b AgdaContravariant -> Data.Functor.Contravariant.contramap #-}
{-# COMPILE GHC _>$_      = \ aℓ f a b AgdaContravariant -> (Data.Functor.Contravariant.>$)      #-}

{-# COMPILE GHC phantom = \ aℓ f a b AgdaFunctor AgdaContravariant -> Data.Functor.Contravariant.phantom #-}
{-# COMPILE GHC _>$<_   = \ aℓ f a b             AgdaContravariant -> (Data.Functor.Contravariant.>$<)   #-}
{-# COMPILE GHC _>$$<_  = \ aℓ f a b             AgdaContravariant -> (Data.Functor.Contravariant.>$$<)  #-}
{-# COMPILE GHC _$<_    = \ aℓ f a b             AgdaContravariant -> (Data.Functor.Contravariant.$<)    #-}

record Predicate (A : Set aℓ) : Set aℓ where
    constructor mkPredicate
    field
        getPredicate : A → Bool

{-# FOREIGN GHC type AgdaPredicate aℓ = Data.Functor.Contravariant.Predicate #-}
{-# COMPILE GHC Predicate = data(1) AgdaPredicate (Data.Functor.Contravariant.Predicate) #-}

postulate
    Contravariant[Predicate] : Contravariant {aℓ} Predicate
    Monoid[Predicate[A]]     : Monoid (Predicate A)
    Semigroup[Predicate[A]]  : Semigroup (Predicate A)

{-# COMPILE GHC Contravariant[Predicate] = \ aℓ   -> AgdaContravariant #-}
{-# COMPILE GHC Monoid[Predicate[A]]     = \ aℓ a -> AgdaMonoid        #-}
{-# COMPILE GHC Semigroup[Predicate[A]]  = \ aℓ a -> AgdaSemigroup     #-}

record Comparison (A : Set aℓ) : Set aℓ where
    constructor mkComparison
    field
        getComparison : A → A → Ordering

{-# FOREIGN GHC type AgdaComparison aℓ = Data.Functor.Contravariant.Comparison #-}
{-# COMPILE GHC Comparison = data(1) AgdaComparison (Data.Functor.Contravariant.Comparison) #-}

postulate
    Contravariant[Comparison] : Contravariant {aℓ} Comparison
    Monoid[Comparison[A]]     : Monoid (Comparison A)
    Semigroup[Comparison[A]]  : Semigroup (Comparison A)

{-# COMPILE GHC Contravariant[Comparison] = \ aℓ   -> AgdaContravariant #-}
{-# COMPILE GHC Monoid[Comparison[A]]     = \ aℓ a -> AgdaMonoid        #-}
{-# COMPILE GHC Semigroup[Comparison[A]]  = \ aℓ a -> AgdaSemigroup     #-}

record Equivalence (A : Set aℓ) : Set aℓ where
    constructor mkEquivalence
    field
        getEquivalence : A → Bool

{-# FOREIGN GHC type AgdaEquivalence aℓ = Data.Functor.Contravariant.Equivalence #-}
{-# COMPILE GHC Equivalence = data(1) AgdaEquivalence (Data.Functor.Contravariant.Equivalence) #-}

postulate
    Contravariant[Equivalence] : Contravariant {aℓ} Equivalence
    Monoid[Equivalence[A]]     : Monoid (Equivalence A)
    Semigroup[Equivalence[A]]  : Semigroup (Equivalence A)

{-# COMPILE GHC Contravariant[Equivalence] = \ aℓ   -> AgdaContravariant #-}
{-# COMPILE GHC Monoid[Equivalence[A]]     = \ aℓ a -> AgdaMonoid        #-}
{-# COMPILE GHC Semigroup[Equivalence[A]]  = \ aℓ a -> AgdaSemigroup     #-}

postulate
    defaultComparison     : ⦃ Ord A ⦄ → Comparison A
    defaultEquivalence    : ⦃ Eq A ⦄ → Equivalence A
    comparisonEquivalence : Comparison A → Equivalence A

{-# COMPILE GHC defaultComparison     = \ aℓ a AgdaOrd -> Data.Functor.Contravariant.defaultComparison     #-}
{-# COMPILE GHC defaultEquivalence    = \ aℓ a AgdaEq  -> Data.Functor.Contravariant.defaultEquivalence    #-}
{-# COMPILE GHC comparisonEquivalence = \ aℓ a         -> Data.Functor.Contravariant.comparisonEquivalence #-}

record Op (A : Set aℓ) (B : Set bℓ) : Set (aℓ ⊔ bℓ) where
    constructor mkOp
    field
        getOp : B → A

{-# FOREIGN GHC type AgdaOp aℓ bℓ = Data.Functor.Contravariant.Op #-}
{-# COMPILE GHC Op = data(2) AgdaOp (Data.Functor.Contravariant.Op) #-}

postulate
    Category[Op]         : Category {aℓ} {bℓ} Op
    Contravariant[Op[A]] : Contravariant (Op {aℓ} {aℓ} A)
    Monoid[Op[A,B]]      : ⦃ Monoid A ⦄ → Monoid (Op A B)
    Semigroup[Op[A,B]]   : ⦃ Semigroup A ⦄ → Semigroup (Op A B)
    Floating[Op[A,B]]    : ⦃ Floating A ⦄ → Floating (Op A B)
    Num[Op[A,B]]         : ⦃ Num A ⦄ → Num (Op A B)
    Fractional[Op[A,B]]  : ⦃ Fractional A ⦄ → Fractional (Op A B)

{-# COMPILE GHC Category[Op]         = \ aℓ bℓ                    -> AgdaCategory      #-}
{-# COMPILE GHC Contravariant[Op[A]] = \ aℓ                       -> AgdaContravariant #-}
{-# COMPILE GHC Monoid[Op[A,B]]      = \ aℓ a bℓ b AgdaMonoid     -> AgdaMonoid        #-}
{-# COMPILE GHC Semigroup[Op[A,B]]   = \ aℓ a bℓ b AgdaSemigroup  -> AgdaSemigroup     #-}
{-# COMPILE GHC Floating[Op[A,B]]    = \ aℓ a bℓ b AgdaFloating   -> AgdaFloating      #-}
{-# COMPILE GHC Num[Op[A,B]]         = \ aℓ a bℓ b AgdaNum        -> AgdaNum           #-}
{-# COMPILE GHC Fractional[Op[A,B]]  = \ aℓ a bℓ b AgdaFractional -> AgdaFractional    #-}
