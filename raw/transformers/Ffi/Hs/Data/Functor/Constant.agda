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
        aℓ : Level
        A B : Set aℓ

record Constant (A : Set aℓ) (B : Set aℓ) : Set aℓ where
    constructor mkConstant
    field
        getConstant : A

open Constant public

{-# FOREIGN GHC type AgdaConstant aℓ = Data.Functor.Constant.Constant #-}
{-# COMPILE GHC Constant = data(1) AgdaConstant (Data.Functor.Constant.Constant) #-}

postulate
    Bifoldable[Constant]       : Bifoldable (Constant {aℓ})
    Bifunctor[Constant]        : Bifunctor (Constant {aℓ})
    Bitraversable[Constant]    : Bitraversable (Constant {aℓ})
    Eq2[Constant]              : Eq2 (Constant {aℓ})
    Ord2[Constant]             : Ord2 (Constant {aℓ})
    Read2[Constant]            : Read2 (Constant {aℓ})
    Show2[Constant]            : Show2 (Constant {aℓ})
    Foldable[Constant[A]]      : Foldable (Constant A)
    Eq1[Constant[A]]           : ⦃ Eq A ⦄ → Eq1 (Constant A)
    Ord1[Constant[A]]          : ⦃ Ord A ⦄ → Ord1 (Constant A)
    Read1[Constant[A]]         : ⦃ Read A ⦄ → Read1 (Constant A)
    Show1[Constant[A]]         : ⦃ Show A ⦄ → Show1 (Constant A)
    Contravariant[Constant[A]] : Contravariant (Constant A)
    Traversable[Constant[A]]   : Traversable (Constant A)
    Applicative[Constant[A]]   : ⦃ Monoid A ⦄ → Applicative (Constant A)
    Functor[Constant[A]]       : Functor (Constant A)
    Monoid[Constant[A,B]]      : ⦃ Monoid A ⦄     → Monoid (Constant A B)
    Semigroup[Constant[A,B]]   : ⦃ Semigroup A ⦄  → Semigroup (Constant A B)
    Read[Constant[A,B]]        : ⦃ Read A ⦄       → Read (Constant A B)
    Show[Constant[A,B]]        : ⦃ Show A ⦄       → Show (Constant A B)
    Eq[Constant[A,B]]          : ⦃ Eq A ⦄         → Eq (Constant A B)
    Ord[Constant[A,B]]         : ⦃ Ord A ⦄        → Ord (Constant A B)

{-# COMPILE GHC Bifoldable[Constant]       = \ aℓ                           -> AgdaBifoldable    #-}
{-# COMPILE GHC Bifunctor[Constant]        = \ aℓ                           -> AgdaBifunctor     #-}
{-# COMPILE GHC Bitraversable[Constant]    = \ aℓ                           -> AgdaBitraversable #-}
{-# COMPILE GHC Eq2[Constant]              = \ aℓ                           -> AgdaEq2           #-}
{-# COMPILE GHC Ord2[Constant]             = \ aℓ                           -> AgdaOrd2          #-}
{-# COMPILE GHC Read2[Constant]            = \ aℓ                           -> AgdaRead2         #-}
{-# COMPILE GHC Show2[Constant]            = \ aℓ                           -> AgdaShow2         #-}
{-# COMPILE GHC Foldable[Constant[A]]      = \ aℓ a                         -> AgdaFoldable      #-}
{-# COMPILE GHC Eq1[Constant[A]]           = \ aℓ a AgdaEq                  -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[Constant[A]]          = \ aℓ a AgdaOrd                 -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[Constant[A]]         = \ aℓ a AgdaRead                -> AgdaRead1         #-}
{-# COMPILE GHC Show1[Constant[A]]         = \ aℓ a AgdaShow                -> AgdaShow1         #-}
{-# COMPILE GHC Contravariant[Constant[A]] = \ aℓ a                         -> AgdaContravariant #-}
{-# COMPILE GHC Traversable[Constant[A]]   = \ aℓ a                         -> AgdaTraversable   #-}
{-# COMPILE GHC Applicative[Constant[A]]   = \ aℓ a AgdaMonoid              -> AgdaApplicative   #-}
{-# COMPILE GHC Functor[Constant[A]]       = \ aℓ a                         -> AgdaFunctor       #-}
{-# COMPILE GHC Monoid[Constant[A,B]]      = \ aℓ a b AgdaMonoid            -> AgdaMonoid        #-}
{-# COMPILE GHC Semigroup[Constant[A,B]]   = \ aℓ a b AgdaSemigroup         -> AgdaSemigroup     #-}
{-# COMPILE GHC Read[Constant[A,B]]        = \ aℓ a b AgdaRead              -> AgdaRead          #-}
{-# COMPILE GHC Show[Constant[A,B]]        = \ aℓ a b AgdaShow              -> AgdaShow          #-}
{-# COMPILE GHC Eq[Constant[A,B]]          = \ aℓ a b AgdaEq                -> AgdaEq            #-}
{-# COMPILE GHC Ord[Constant[A,B]]         = \ aℓ a b AgdaOrd               -> AgdaOrd           #-}
