{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Class where

open import Agda.Primitive
open import Ffi.Hs.-base.Kind using (IsKind)

private
    variable
        aℓ bℓ cℓ : Level

postulate
    Num        : Set aℓ → Set aℓ
    Integral   : Set aℓ → Set aℓ
    Real       : Set aℓ → Set aℓ
    Fractional : Set aℓ → Set aℓ
    RealFrac   : Set aℓ → Set aℓ
    Floating   : Set aℓ → Set aℓ
    RealFloat  : Set aℓ → Set aℓ
    
    Bits       : Set aℓ → Set aℓ
    FiniteBits : Set aℓ → Set aℓ

    Read : Set aℓ → Set aℓ
    Show : Set aℓ → Set aℓ

    Bounded : Set aℓ → Set aℓ
    Enum    : Set aℓ → Set aℓ

    Eq  : Set aℓ → Set aℓ
    Ord : Set aℓ → Set aℓ
    Ix  : Set aℓ → Set aℓ

    Semigroup : Set aℓ → Set aℓ
    Monoid    : Set aℓ → Set aℓ

    Foldable    : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
    Traversable : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)

    Functor       : (Set aℓ → Set aℓ) → Set aℓ
    Contravariant : (Set aℓ → Set aℓ) → Set aℓ
    Applicative   : (Set aℓ → Set aℓ) → Set aℓ
    Alternative   : (Set aℓ → Set aℓ) → Set aℓ
    Monad         : (Set aℓ → Set aℓ) → Set aℓ
    MonadPlus     : (Set aℓ → Set aℓ) → Set aℓ
    MonadFail     : (Set aℓ → Set aℓ) → Set aℓ
    MonadFix      : (Set aℓ → Set aℓ) → Set aℓ
    MonadZip      : (Set aℓ → Set aℓ) → Set aℓ
    MonadIO       : (Set aℓ → Set aℓ) → Set aℓ
    
    Category    : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    Arrow       : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowZero   : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowChoice : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowApply  : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowPlus   : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowLoop   : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    
    Bifunctor     : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    Bifoldable    : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    Bitraversable : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    
    Storable  : Set aℓ → Set aℓ
    Exception : Set aℓ → Set aℓ

    Data         : {K : Set (lsuc aℓ)} → K → Set aℓ
    Typeable     : {K : Set (lsuc aℓ)} → K → Set aℓ
    TestEquality : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
    TestCoercion : {K₁ : Set (lsuc aℓ)} {K₂ : Set (lsuc bℓ)} → (K₁ → K₂) → Set (aℓ ⊔ bℓ)
    Coercible    : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)

{-# FOREIGN GHC
data AgdaTypeable kℓ k (a :: k) = Type.Reflection.Typeable a => AgdaTypeable
#-}
{-# COMPILE GHC Typeable = type(0) AgdaTypeable #-}

{-# FOREIGN GHC
data AgdaTestEquality aℓ bℓ f = Data.Type.Equality.TestEquality f => AgdaTestEquality
#-}
{-# COMPILE GHC TestEquality = type(0) AgdaTestEquality #-}

{-# FOREIGN GHC import qualified Text.Read #-}
{-# FOREIGN GHC data AgdaRead aℓ a = Text.Read.Read a => AgdaRead #-}
{-# COMPILE GHC Read = type(0) AgdaRead #-}

{-# FOREIGN GHC import qualified Data.Semigroup #-}
{-# FOREIGN GHC data AgdaSemigroup aℓ a = Data.Semigroup.Semigroup a => AgdaSemigroup #-}
{-# COMPILE GHC Semigroup = type(0) AgdaSemigroup #-}

{-# FOREIGN GHC import qualified Text.Show #-}
{-# FOREIGN GHC data AgdaShow aℓ a = Text.Show a => AgdaShow #-}
{-# COMPILE GHC Show = type(0) AgdaShow #-}

{-# FOREIGN GHC import qualified Data.Bifunctor #-}
{-# FOREIGN GHC data AgdaBifunctor f1ℓ f2ℓ bℓ f = Data.Bifunctor.Bifunctor f => AgdaBifunctor #-}
{-# COMPILE GHC Bifunctor = type(0) AgdaBifunctor #-}

{-# FOREIGN GHC import qualified GHC.Num #-}
{-# FOREIGN GHC data AgdaNum aℓ a = GHC.Num.Num a => AgdaNum #-}
{-# COMPILE GHC Num = type(0) AgdaNum #-}

{-# FOREIGN GHC data AgdaEq aℓ a = Eq a => AgdaEq #-}
{-# COMPILE GHC Eq = type(0) AgdaEq #-}

{-# FOREIGN GHC data AgdaOrd aℓ a = Ord a => AgdaOrd #-}
{-# COMPILE GHC Ord = type(0) AgdaOrd #-}

{-# FOREIGN GHC import qualified Data.Functor #-}
{-# FOREIGN GHC data AgdaFunctor fℓ f = Data.Functor.Functor f => AgdaFunctor #-}
{-# COMPILE GHC Functor = type(0) AgdaFunctor #-}

{-# FOREIGN GHC import qualified Control.Applicative #-}
{-# FOREIGN GHC data AgdaApplicative aℓ f = Control.Applicative.Applicative f => AgdaApplicative #-}
{-# FOREIGN GHC data AgdaAlternative aℓ f = Control.Applicative.Alternative f => AgdaAlternative #-}
{-# COMPILE GHC Applicative = type(0) AgdaApplicative #-}
{-# COMPILE GHC Alternative = type(0) AgdaAlternative #-}

{-# FOREIGN GHC import qualified Control.Monad.Fail #-}
{-# FOREIGN GHC data AgdaMonadFail fℓ f = Control.Monad.Fail.MonadFail f => AgdaMonadFail #-}
{-# COMPILE GHC MonadFail = type(0) AgdaMonadFail #-}
