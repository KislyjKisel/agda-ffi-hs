{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Traversable where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Applicative; Monad; Monoid)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

open Ffi.Hs.-base.Class public
    using (Traversable)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Traversable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A B S : Set aℓ
        F M : Set aℓ → Set aℓ
        T : Set aℓ → Set bℓ

postulate
    traverse  : ⦃ Traversable T ⦄ → ⦃ Applicative F ⦄ → (A → F B) → T A → F (T B)
    sequenceA : ⦃ Traversable T ⦄ → ⦃ Applicative F ⦄ → T (F A) → F (T A)
    mapM      : ⦃ Traversable T ⦄ → ⦃ Monad M ⦄ → (A → M B) → T A → M (T B)
    sequence  : ⦃ Traversable T ⦄ → ⦃ Monad M ⦄ → T (M A) → M (T A)

    for       : ⦃ Traversable T ⦄ → ⦃ Applicative F ⦄ → T A → (A → F B) → F (T B)
    forM      : ⦃ Traversable T ⦄ → ⦃ Monad M ⦄ → T A → (A → M B) → M (T B)
    mapAccumL : ⦃ Traversable T ⦄ → (S → A → Tuple2 S B) → S → T A → Tuple2 S (T B)
    mapAccumR : ⦃ Traversable T ⦄ → (S → A → Tuple2 S B) → S → T A → Tuple2 S (T B)

    fmapDefault    : ⦃ Traversable T ⦄ → (A → B) → T A → T B
    foldMapDefault : ⦃ Traversable T ⦄ → ⦃ Monoid B ⦄ → (A → B) → T A → B

{-# COMPILE GHC traverse  = \ ℓ t f a b AgdaTraversable AgdaApplicative -> Data.Traversable.traverse  #-}
{-# COMPILE GHC sequenceA = \ ℓ t f a AgdaTraversable AgdaApplicative   -> Data.Traversable.sequenceA #-}
{-# COMPILE GHC mapM      = \ ℓ t m a b AgdaTraversable AgdaMonad       -> Data.Traversable.mapM      #-}
{-# COMPILE GHC sequence  = \ ℓ t m a AgdaTraversable AgdaMonad         -> Data.Traversable.sequence  #-}

{-# COMPILE GHC for       = \ ℓ t f a b AgdaTraversable AgdaApplicative -> Data.Traversable.for       #-}
{-# COMPILE GHC forM      = \ ℓ t m a b AgdaTraversable AgdaMonad       -> Data.Traversable.forM      #-}
{-# COMPILE GHC mapAccumL = \ tℓ1 tℓ2 t sℓ s a b AgdaTraversable        -> Data.Traversable.mapAccumL #-}
{-# COMPILE GHC mapAccumR = \ tℓ1 tℓ2 t sℓ s a b AgdaTraversable        -> Data.Traversable.mapAccumR #-}

{-# COMPILE GHC fmapDefault    = \ tℓ1 tℓ2 t a b AgdaTraversable               -> Data.Traversable.fmapDefault    #-}
{-# COMPILE GHC foldMapDefault = \ tℓ1 tℓ2 t a bℓ b AgdaTraversable AgdaMonoid -> Data.Traversable.foldMapDefault #-}
