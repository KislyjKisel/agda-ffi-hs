{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Constant where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Functor.Constant
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A B : Set aℓ

record Constant (A : Set aℓ) (B : Set (aℓ ⊔ bℓ)) : Set (aℓ ⊔ bℓ) where
    constructor mkConstant
    field
        getConstant : A

open Constant public

{-# FOREIGN GHC type AgdaConstant aℓ bℓ = Data.Functor.Constant.Constant #-}
{-# COMPILE GHC Constant = data(2) AgdaConstant (Data.Functor.Constant.Constant) #-}

postulate
    Bifoldable[Constant]       : Bifoldable (Constant {aℓ} {bℓ})
    Bifunctor[Constant]        : Bifunctor (Constant {aℓ} {bℓ})
    Bitraversable[Constant]    : Bitraversable (Constant {aℓ} {bℓ})
    Eq2[Constant]              : Eq2 (Constant {aℓ} {bℓ})
    Ord2[Constant]             : Ord2 (Constant {aℓ} {bℓ})
    Read2[Constant]            : Read2 (Constant {aℓ} {bℓ})
    Show2[Constant]            : Show2 (Constant {aℓ} {bℓ})
    Foldable[Constant[A]]      : Foldable (Constant {bℓ = bℓ} A)
    Eq1[Constant[A]]           : ⦃ Eq A ⦄ → Eq1 (Constant {bℓ = bℓ} A)
    Ord1[Constant[A]]          : ⦃ Ord A ⦄ → Ord1 (Constant {bℓ = bℓ} A)
    Read1[Constant[A]]         : ⦃ Read A ⦄ → Read1 (Constant {bℓ = bℓ} A)
    Show1[Constant[A]]         : ⦃ Show A ⦄ → Show1 (Constant {bℓ = bℓ} A)
    Contravariant[Constant[A]] : Contravariant (Constant {bℓ = bℓ} A)
    Traversable[Constant[A]]   : Traversable (Constant {bℓ = bℓ} A)
    Applicative[Constant[A]]   : ⦃ Monoid A ⦄ → Applicative (Constant {bℓ = bℓ} A)
    Functor[Constant[A]]       : Functor (Constant {bℓ = bℓ} A)
    Monoid[Constant[A,B]]      : ⦃ Monoid A ⦄     → Monoid (Constant A B)
    Semigroup[Constant[A,B]]   : ⦃ Semigroup A ⦄  → Semigroup (Constant A B)
    Read[Constant[A,B]]        : ⦃ Read A ⦄       → Read (Constant A B)
    Show[Constant[A,B]]        : ⦃ Show A ⦄       → Show (Constant A B)
    Eq[Constant[A,B]]          : ⦃ Eq A ⦄         → Eq (Constant A B)
    Ord[Constant[A,B]]         : ⦃ Ord A ⦄        → Ord (Constant A B)

{-# COMPILE GHC Bifoldable[Constant]       = \ aℓ bℓ                           -> AgdaBifoldable    #-}
{-# COMPILE GHC Bifunctor[Constant]        = \ aℓ bℓ                           -> AgdaBifunctor     #-}
{-# COMPILE GHC Bitraversable[Constant]    = \ aℓ bℓ                           -> AgdaBitraversable #-}
{-# COMPILE GHC Eq2[Constant]              = \ aℓ bℓ                           -> AgdaEq2           #-}
{-# COMPILE GHC Ord2[Constant]             = \ aℓ bℓ                           -> AgdaOrd2          #-}
{-# COMPILE GHC Read2[Constant]            = \ aℓ bℓ                           -> AgdaRead2         #-}
{-# COMPILE GHC Show2[Constant]            = \ aℓ bℓ                           -> AgdaShow2         #-}
{-# COMPILE GHC Foldable[Constant[A]]      = \ bℓ aℓ a                         -> AgdaFoldable      #-}
{-# COMPILE GHC Eq1[Constant[A]]           = \ aℓ a bℓ AgdaEq                  -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[Constant[A]]          = \ aℓ a bℓ AgdaOrd                 -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[Constant[A]]         = \ aℓ a bℓ AgdaRead                -> AgdaRead1         #-}
{-# COMPILE GHC Show1[Constant[A]]         = \ aℓ a bℓ AgdaShow                -> AgdaShow1         #-}
{-# COMPILE GHC Contravariant[Constant[A]] = \ aℓ a                            -> AgdaContravariant #-}
{-# COMPILE GHC Traversable[Constant[A]]   = \ bℓ aℓ a                         -> AgdaTraversable   #-}
{-# COMPILE GHC Applicative[Constant[A]]   = \ aℓ a AgdaMonoid                 -> AgdaApplicative   #-}
{-# COMPILE GHC Functor[Constant[A]]       = \ aℓ a                            -> AgdaFunctor       #-}
{-# COMPILE GHC Monoid[Constant[A,B]]      = \ aℓ a bℓ b AgdaMonoid            -> AgdaMonoid        #-}
{-# COMPILE GHC Semigroup[Constant[A,B]]   = \ aℓ a bℓ b AgdaSemigroup         -> AgdaSemigroup     #-}
{-# COMPILE GHC Read[Constant[A,B]]        = \ aℓ a bℓ b AgdaRead              -> AgdaRead          #-}
{-# COMPILE GHC Show[Constant[A,B]]        = \ aℓ a bℓ b AgdaShow              -> AgdaShow          #-}
{-# COMPILE GHC Eq[Constant[A,B]]          = \ aℓ a bℓ b AgdaEq                -> AgdaEq            #-}
{-# COMPILE GHC Ord[Constant[A,B]]         = \ aℓ a bℓ b AgdaOrd               -> AgdaOrd           #-}
