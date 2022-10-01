{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Reverse where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Functor.Reverse
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        F : Set aℓ → Set aℓ

record Reverse (F : Set aℓ → Set aℓ) (A : Set aℓ) : Set aℓ where
    constructor mkReverse
    field
        getReverse : F A

open Reverse public

{-# FOREIGN GHC type AgdaReverse aℓ = Data.Functor.Reverse.Reverse #-}
{-# COMPILE GHC Reverse = data(1) AgdaReverse (Data.Functor.Reverse.Reverse) #-}

postulate
    Monad[Reverse[F]]         : ⦃ Monad F ⦄ → Monad (Reverse F)
    Functor[Reverse[F]]       : ⦃ Functor F ⦄ → Functor (Reverse F)
    MonadFail[Reverse[F]]     : ⦃ MonadFail F ⦄ → MonadFail (Reverse F)
    Applicative[Reverse[F]]   : ⦃ Applicative F ⦄ → Applicative (Reverse F)
    Foldable[Reverse[F]]      : ⦃ Foldable F ⦄ → Foldable (Reverse F)
    Traversable[Reverse[F]]   : ⦃ Traversable F ⦄ → Traversable (Reverse F)
    Contravariant[Reverse[F]] : ⦃ Contravariant F ⦄ → Contravariant (Reverse F)
    Eq1[Reverse[F]]           : ⦃ Eq1 F ⦄ → Eq1 (Reverse F)
    Ord1[Reverse[F]]          : ⦃ Ord1 F ⦄ → Ord1 (Reverse F)
    Read1[Reverse[F]]         : ⦃ Read1 F ⦄ → Read1 (Reverse F)
    Show1[Reverse[F]]         : ⦃ Show1 F ⦄ → Show1 (Reverse F)
    Alternative[Reverse[F]]   : ⦃ Alternative F ⦄ → Alternative (Reverse F)
    MonadPlus[Reverse[F]]     : ⦃ MonadPlus F ⦄ → MonadPlus (Reverse F)
    Eq[Reverse[F,A]]          : ⦃ Eq1 F ⦄ → ⦃ Eq A ⦄ → Eq (Reverse F A)
    Ord[Reverse[F,A]]         : ⦃ Ord1 F ⦄ → ⦃ Ord A ⦄ → Ord (Reverse F A)
    Read[Reverse[F,A]]        : ⦃ Read1 F ⦄ → ⦃ Read A ⦄ → Read (Reverse F A)
    Show[Reverse[F,A]]        : ⦃ Show1 F ⦄ → ⦃ Show A ⦄ → Show (Reverse F A)

{-# COMPILE GHC Monad[Reverse[F]]         = \ fℓ f AgdaMonad            -> AgdaMonad         #-}
{-# COMPILE GHC Functor[Reverse[F]]       = \ fℓ f AgdaFunctor          -> AgdaFunctor       #-}
{-# COMPILE GHC MonadFail[Reverse[F]]     = \ fℓ f AgdaMonadFail        -> AgdaMonadFail     #-}
{-# COMPILE GHC Applicative[Reverse[F]]   = \ fℓ f AgdaApplicative      -> AgdaApplicative   #-}
{-# COMPILE GHC Foldable[Reverse[F]]      = \ fℓ f AgdaFoldable         -> AgdaFoldable      #-}
{-# COMPILE GHC Traversable[Reverse[F]]   = \ fℓ f AgdaTraversable      -> AgdaTraversable   #-}
{-# COMPILE GHC Contravariant[Reverse[F]] = \ fℓ f AgdaContravariant    -> AgdaContravariant #-}
{-# COMPILE GHC Eq1[Reverse[F]]           = \ fℓ f AgdaEq1              -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[Reverse[F]]          = \ fℓ f AgdaOrd1             -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[Reverse[F]]         = \ fℓ f AgdaRead1            -> AgdaRead1         #-}
{-# COMPILE GHC Show1[Reverse[F]]         = \ fℓ f AgdaShow1            -> AgdaShow1         #-}
{-# COMPILE GHC Alternative[Reverse[F]]   = \ fℓ f AgdaAlternative      -> AgdaAlternative   #-}
{-# COMPILE GHC MonadPlus[Reverse[F]]     = \ fℓ f AgdaMonadPlus        -> AgdaMonadPlus     #-}
{-# COMPILE GHC Eq[Reverse[F,A]]          = \ fℓ f a AgdaEq1 AgdaEq     -> AgdaEq            #-}
{-# COMPILE GHC Ord[Reverse[F,A]]         = \ fℓ f a AgdaOrd1 AgdaOrd   -> AgdaOrd           #-}
{-# COMPILE GHC Read[Reverse[F,A]]        = \ fℓ f a AgdaRead1 AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Show[Reverse[F,A]]        = \ fℓ f a AgdaShow1 AgdaShow -> AgdaShow          #-}
