{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Coerce where

{-# WARNING_ON_IMPORT "Does not compile, can't alias `coerce` in a levity polymorphic way" #-}
-- todo: `coerce` for values with monomorphic RuntimeReps

open import Ffi.Hs.GHC.Exts using (TYPE; RuntimeRep)

open import Agda.Primitive

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    Coercible : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)
    coerce : ∀{k : RuntimeRep} → ⦃ TYPE k A ⦄ → ⦃ TYPE k B ⦄ → ⦃ Coercible A B ⦄ → A → B

    Coercible[A,A] : Coercible A A

{-# FOREIGN GHC import qualified Data.Coerce #-}
{-# FOREIGN GHC data AgdaCoercible aℓ bℓ a b = Data.Coerce.Coercible a b => AgdaCoercible #-}
{-# COMPILE GHC Coercible = type(0) AgdaCoercible #-}
{-# COMPILE GHC coerce = \ aℓ bℓ a b k ? ? AgdaCoercible -> Data.Coerce.coerce #-}
{-# COMPILE GHC Coercible[A,A] = \ aℓ a -> AgdaCoercible #-}