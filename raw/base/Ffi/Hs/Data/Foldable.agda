{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Foldable where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level using (Liftℓ)
open import Ffi.Hs.-base.Unit  using (⊤′)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Ord    using (Ordering)

open Ffi.Hs.-base.Class public
    using (Foldable)

{-# FOREIGN GHC
import qualified Data.Foldable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ
        F : Set aℓ → Set bℓ
        M : Set aℓ → Set aℓ

postulate
    fold       : ⦃ Foldable F ⦄ → ⦃ Monoid A ⦄ → F A → A
    foldMap    : ⦃ Foldable F ⦄ → ⦃ Monoid B ⦄ → (A → B) → F A → B
    foldMap'   : ⦃ Foldable F ⦄ → ⦃ Monoid B ⦄ → (A → B) → F A → B
    foldl      : ⦃ Foldable F ⦄ → (B → A → B) → B → F A → B
    foldl'     : ⦃ Foldable F ⦄ → (B → A → B) → B → F A → B
    foldl1     : ⦃ Foldable F ⦄ → (A → A → A) → F A → A
    foldr      : ⦃ Foldable F ⦄ → (A → B → B) → B → F A → B
    foldr'     : ⦃ Foldable F ⦄ → (A → B → B) → B → F A → B
    foldr1     : ⦃ Foldable F ⦄ → (A → A → A) → F A → A
    toList     : ⦃ Foldable F ⦄ → F A → List A
    null       : ⦃ Foldable F ⦄ → F A → Bool
    length     : ⦃ Foldable F ⦄ → F A → Int
    elem       : ⦃ Foldable F ⦄ → ⦃ Eq A ⦄ → A → F A → Bool
    maximum    : ⦃ Foldable F ⦄ → ⦃ Ord A ⦄ → F A → A
    minimum    : ⦃ Foldable F ⦄ → ⦃ Ord A ⦄ → F A → A
    sum        : ⦃ Foldable F ⦄ → ⦃ Num A ⦄ → F A → A
    product    : ⦃ Foldable F ⦄ → ⦃ Num A ⦄ → F A → A
    foldrM     : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → (A → B → M B) → B → F A → M B
    foldlM     : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → (B → A → M B) → B → F A → M B
    traverse-  : ⦃ Foldable F ⦄ → ⦃ Applicative M ⦄ → (A → M B) → F A → M ⊤′
    for-       : ⦃ Foldable F ⦄ → ⦃ Applicative M ⦄ → F A → (A → M B) → M ⊤′
    sequenceA- : ⦃ Foldable F ⦄ → ⦃ Applicative M ⦄ → F (M A) → M ⊤′
    asum       : ⦃ Foldable F ⦄ → ⦃ Alternative M ⦄ → F (M A) → M A
    mapM-      : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → (A → M B) → F A → M ⊤′
    forM-      : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → F A → (A → M B) → M ⊤′
    sequence-  : ⦃ Foldable F ⦄ → ⦃ Monad M ⦄ → F (M A) → M ⊤′
    msum       : ⦃ Foldable F ⦄ → ⦃ MonadPlus M ⦄ → F (M A) → M A
    concat     : ⦃ Foldable F ⦄ → F (List A) → List A
    concatMap  : ⦃ Foldable F ⦄ → (A → List B) → F A → List B
    and        : ⦃ Foldable F ⦄ → F (Liftℓ _ Bool) → Bool
    or         : ⦃ Foldable F ⦄ → F (Liftℓ _ Bool) → Bool
    any        : ⦃ Foldable F ⦄ → (A → Bool) → F A → Bool
    all        : ⦃ Foldable F ⦄ → (A → Bool) → F A → Bool
    maximumBy  : ⦃ Foldable F ⦄ → (A → A → Ordering) → F A → A
    minimumBy  : ⦃ Foldable F ⦄ → (A → A → Ordering) → F A → A
    notElem    : ⦃ Foldable F ⦄ → ⦃ Eq A ⦄ → A → F A → Bool
    find       : ⦃ Foldable F ⦄ → (A → Bool) → F A → Maybe A

{-# COMPILE GHC fold       = \ fℓ1 fℓ2 f a        AgdaFoldable AgdaMonoid      -> Data.Foldable.fold       #-}
{-# COMPILE GHC foldMap    = \ fℓ1 fℓ2 f a bℓ b   AgdaFoldable AgdaMonoid      -> Data.Foldable.foldMap    #-}
{-# COMPILE GHC foldMap'   = \ fℓ1 fℓ2 f a bℓ b   AgdaFoldable AgdaMonoid      -> Data.Foldable.foldMap'   #-}
{-# COMPILE GHC foldl      = \ fℓ1 fℓ2 f a bℓ b   AgdaFoldable                 -> Data.Foldable.foldl      #-}
{-# COMPILE GHC foldl'     = \ fℓ1 fℓ2 f a bℓ b   AgdaFoldable                 -> Data.Foldable.foldl'     #-}
{-# COMPILE GHC foldl1     = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.foldl1     #-}
{-# COMPILE GHC foldr      = \ fℓ1 fℓ2 f a bℓ b   AgdaFoldable                 -> Data.Foldable.foldr      #-}
{-# COMPILE GHC foldr'     = \ fℓ1 fℓ2 f a bℓ b   AgdaFoldable                 -> Data.Foldable.foldr'     #-}
{-# COMPILE GHC foldr1     = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.foldr1     #-}
{-# COMPILE GHC toList     = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.toList     #-}
{-# COMPILE GHC null       = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.null       #-}
{-# COMPILE GHC length     = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.length     #-}
{-# COMPILE GHC elem       = \ fℓ1 fℓ2 f a        AgdaFoldable AgdaEq          -> Data.Foldable.elem       #-}
{-# COMPILE GHC maximum    = \ fℓ1 fℓ2 f a        AgdaFoldable AgdaOrd         -> Data.Foldable.maximum    #-}
{-# COMPILE GHC minimum    = \ fℓ1 fℓ2 f a        AgdaFoldable AgdaOrd         -> Data.Foldable.minimum    #-}
{-# COMPILE GHC sum        = \ fℓ1 fℓ2 f a        AgdaFoldable AgdaNum         -> Data.Foldable.sum        #-}
{-# COMPILE GHC product    = \ fℓ1 fℓ2 f a        AgdaFoldable AgdaNum         -> Data.Foldable.product    #-}
{-# COMPILE GHC foldrM     = \ fℓ1 fℓ2 f mℓ m a b AgdaFoldable AgdaMonad       -> Data.Foldable.foldrM     #-}
{-# COMPILE GHC foldlM     = \ fℓ1 fℓ2 f mℓ m a b AgdaFoldable AgdaMonad       -> Data.Foldable.foldlM     #-}
{-# COMPILE GHC traverse-  = \ fℓ1 fℓ2 f mℓ m a b AgdaFoldable AgdaApplicative -> Data.Foldable.traverse_  #-}
{-# COMPILE GHC for-       = \ fℓ1 fℓ2 f mℓ m a b AgdaFoldable AgdaApplicative -> Data.Foldable.for_       #-}
{-# COMPILE GHC sequenceA- = \ fℓ1 fℓ2 f m a      AgdaFoldable AgdaApplicative -> Data.Foldable.sequenceA_ #-}
{-# COMPILE GHC asum       = \ fℓ1 fℓ2 f m a      AgdaFoldable AgdaAlternative -> Data.Foldable.asum       #-}
{-# COMPILE GHC mapM-      = \ fℓ1 fℓ2 f mℓ m a b AgdaFoldable AgdaMonad       -> Data.Foldable.mapM_      #-}
{-# COMPILE GHC forM-      = \ fℓ1 fℓ2 f mℓ m a b AgdaFoldable AgdaMonad       -> Data.Foldable.forM_      #-}
{-# COMPILE GHC sequence-  = \ fℓ1 fℓ2 f m a      AgdaFoldable AgdaMonad       -> Data.Foldable.sequence_  #-}
{-# COMPILE GHC msum       = \ fℓ1 fℓ2 f m a      AgdaFoldable AgdaMonadPlus   -> Data.Foldable.msum       #-}
{-# COMPILE GHC concat     = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.concat     #-}
{-# COMPILE GHC concatMap  = \ fℓ1 fℓ2 f a bℓ b   AgdaFoldable                 -> Data.Foldable.concatMap  #-}
{-# COMPILE GHC and        = \ fℓ1 fℓ2 f          AgdaFoldable                 -> Data.Foldable.and        #-}
{-# COMPILE GHC or         = \ fℓ1 fℓ2 f          AgdaFoldable                 -> Data.Foldable.or         #-}
{-# COMPILE GHC any        = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.any        #-}
{-# COMPILE GHC all        = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.all        #-}
{-# COMPILE GHC maximumBy  = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.maximumBy  #-}
{-# COMPILE GHC minimumBy  = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.minimumBy  #-}
{-# COMPILE GHC notElem    = \ fℓ1 fℓ2 f a        AgdaFoldable AgdaEq          -> Data.Foldable.notElem    #-}
{-# COMPILE GHC find       = \ fℓ1 fℓ2 f a        AgdaFoldable                 -> Data.Foldable.find       #-}
