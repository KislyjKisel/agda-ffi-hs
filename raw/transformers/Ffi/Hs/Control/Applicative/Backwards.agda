{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Applicative.Backwards where

open import Agda.Primitive     using (Level)
open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Applicative.Backwards
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        F : Set aℓ → Set aℓ


record Backwards (F : Set aℓ → Set aℓ) (A : Set aℓ) : Set aℓ where
    constructor mkBackwards
    field
        forwards : F A

open Backwards public

{-# FOREIGN GHC type AgdaBackwards aℓ = Control.Applicative.Backwards.Backwards #-}
{-# COMPILE GHC Backwards = data(1) AgdaBackwards (Control.Applicative.Backwards.Backwards) #-}

postulate
    Functor[Backwards[F]]       : ⦃ Functor F ⦄ → Functor (Backwards F)
    Applicative[Backwards[F]]   : ⦃ Applicative F ⦄ → Applicative (Backwards F)
    Foldable[Backwards[F]]      : ⦃ Foldable F ⦄ → Foldable (Backwards F)
    Traversable[Backwards[F]]   : ⦃ Traversable F ⦄ → Traversable (Backwards F)
    Contravariant[Backwards[F]] : ⦃ Contravariant F ⦄ → Contravariant (Backwards F)
    Eq1[Backwards[F]]           : ⦃ Eq1 F ⦄ → Eq1 (Backwards F)
    Ord1[Backwards[F]]          : ⦃ Ord1 F ⦄ → Ord1 (Backwards F)
    Show1[Backwards[F]]         : ⦃ Show1 F ⦄ → Show1 (Backwards F)
    Read1[Backwards[F]]         : ⦃ Read1 F ⦄ → Read1 (Backwards F)
    Alternative[Backwards[F]]   : ⦃ Alternative F ⦄ → Alternative (Backwards F)
    Eq[Backwards[F,A]]          : ⦃ Eq1 F ⦄ → ⦃ Eq A ⦄ → Eq (Backwards F A)
    Ord[Backwards[F,A]]         : ⦃ Ord1 F ⦄ → ⦃ Ord A ⦄ → Ord (Backwards F A)
    Read[Backwards[F,A]]        : ⦃ Read1 F ⦄ → ⦃ Read A ⦄ → Read (Backwards F A)
    Show[Backwards[F,A]]        : ⦃ Show1 F ⦄ → ⦃ Show A ⦄ → Show (Backwards F A)

{-# COMPILE GHC Functor[Backwards[F]]       = \ fℓ f AgdaFunctor          -> AgdaFunctor       #-}
{-# COMPILE GHC Applicative[Backwards[F]]   = \ fℓ f AgdaApplicative      -> AgdaApplicative   #-}
{-# COMPILE GHC Foldable[Backwards[F]]      = \ fℓ f AgdaFoldable         -> AgdaFoldable      #-}
{-# COMPILE GHC Traversable[Backwards[F]]   = \ fℓ f AgdaTraversable      -> AgdaTraversable   #-}
{-# COMPILE GHC Contravariant[Backwards[F]] = \ fℓ f AgdaContravariant    -> AgdaContravariant #-}
{-# COMPILE GHC Eq1[Backwards[F]]           = \ fℓ f AgdaEq1              -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[Backwards[F]]          = \ fℓ f AgdaOrd1             -> AgdaOrd1          #-}
{-# COMPILE GHC Show1[Backwards[F]]         = \ fℓ f AgdaShow1            -> AgdaShow1         #-}
{-# COMPILE GHC Read1[Backwards[F]]         = \ fℓ f AgdaRead1            -> AgdaRead1         #-}
{-# COMPILE GHC Alternative[Backwards[F]]   = \ fℓ f AgdaAlternative      -> AgdaAlternative   #-}
{-# COMPILE GHC Eq[Backwards[F,A]]          = \ fℓ f a AgdaEq1 AgdaEq     -> AgdaEq            #-}
{-# COMPILE GHC Ord[Backwards[F,A]]         = \ fℓ f a AgdaOrd1 AgdaOrd   -> AgdaOrd           #-}
{-# COMPILE GHC Read[Backwards[F,A]]        = \ fℓ f a AgdaRead1 AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Show[Backwards[F,A]]        = \ fℓ f a AgdaShow1 AgdaShow -> AgdaShow          #-}
