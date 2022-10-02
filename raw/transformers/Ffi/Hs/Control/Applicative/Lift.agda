{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Applicative.Lift where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.Applicative   using (Applicative; pure)
open import Ffi.Hs.Data.Either           using (Either; Left; Right; either)
open import Ffi.Hs.Data.Functor.Constant using (Constant; mkConstant)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Applicative.Lift
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ eℓ : Level
        A B E : Set aℓ
        F G : Set aℓ → Set aℓ


data Lift (F : Set aℓ → Set aℓ) (A : Set aℓ) : Set aℓ where
    Pure  : A → Lift F A
    Other : F A → Lift F A

{-# FOREIGN GHC type AgdaLift aℓ = Control.Applicative.Lift.Lift #-}
{-# COMPILE GHC Lift = data(1) AgdaLift
    ( Control.Applicative.Lift.Pure
    | Control.Applicative.Lift.Other
    ) #-}

postulate
    Functor[Lift[F]]     : ⦃ Functor F ⦄ → Functor (Lift F)
    Applicative[Lift[F]] : ⦃ Applicative F ⦄ → Applicative (Lift F)
    Foldable[Lift[F]]    : ⦃ Foldable F ⦄ → Foldable (Lift F)
    Traversable[Lift[F]] : ⦃ Traversable F ⦄ → Traversable (Lift F)
    Eq1[Lift[F]]         : ⦃ Eq1 F ⦄ → Eq1 (Lift F)
    Ord1[Lift[F]]        : ⦃ Ord1 F ⦄ → Ord1 (Lift F)
    Read1[Lift[F]]       : ⦃ Read1 F ⦄ → Read1 (Lift F)
    Show1[Lift[F]]       : ⦃ Show1 F ⦄ → Show1 (Lift F)
    Alternative[Lift[F]] : ⦃ Alternative F ⦄ → Alternative (Lift F)
    Eq[Lift[F,A]]        : ⦃ Eq1 F ⦄ → ⦃ Eq A ⦄ → Eq (Lift F A)
    Ord[Lift[F,A]]       : ⦃ Ord1 F ⦄ → ⦃ Ord A ⦄ → Ord (Lift F A)
    Read[Lift[F,A]]      : ⦃ Read1 F ⦄ → ⦃ Read A ⦄ → Read (Lift F A)
    Show[Lift[F,A]]      : ⦃ Show1 F ⦄ → ⦃ Show A ⦄ → Show (Lift F A)

{-# COMPILE GHC Functor[Lift[F]]     = \ fℓ f AgdaFunctor          -> AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Lift[F]] = \ fℓ f AgdaApplicative      -> AgdaApplicative #-}
{-# COMPILE GHC Foldable[Lift[F]]    = \ fℓ f AgdaFoldable         -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Lift[F]] = \ fℓ f AgdaTraversable      -> AgdaTraversable #-}
{-# COMPILE GHC Eq1[Lift[F]]         = \ fℓ f AgdaEq1              -> AgdaEq1         #-}
{-# COMPILE GHC Ord1[Lift[F]]        = \ fℓ f AgdaOrd1             -> AgdaOrd1        #-}
{-# COMPILE GHC Read1[Lift[F]]       = \ fℓ f AgdaRead1            -> AgdaRead1       #-}
{-# COMPILE GHC Show1[Lift[F]]       = \ fℓ f AgdaShow1            -> AgdaShow1       #-}
{-# COMPILE GHC Alternative[Lift[F]] = \ fℓ f AgdaAlternative      -> AgdaAlternative #-}
{-# COMPILE GHC Eq[Lift[F,A]]        = \ fℓ f a AgdaEq1 AgdaEq     -> AgdaEq          #-}
{-# COMPILE GHC Ord[Lift[F,A]]       = \ fℓ f a AgdaOrd1 AgdaOrd   -> AgdaOrd         #-}
{-# COMPILE GHC Read[Lift[F,A]]      = \ fℓ f a AgdaRead1 AgdaRead -> AgdaRead        #-}
{-# COMPILE GHC Show[Lift[F,A]]      = \ fℓ f a AgdaShow1 AgdaShow -> AgdaShow        #-}


unLift : ⦃ Applicative F ⦄ → Lift F A → F A
unLift (Pure x)  = pure x
unLift (Other e) = e

mapLift : (F A → G A) → Lift F A → Lift G A
mapLift f (Pure x)  = Pure x
mapLift f (Other e) = Other (f e)

elimLift : (A → B) → (F A → B) → Lift F A → B
elimLift f g (Pure x)  = f x
elimLift f g (Other e) = g e


Errors : (E : Set aℓ) → Set aℓ → Set aℓ
Errors E = Lift (Constant E)

runErrors : Errors E A → Either E A
runErrors (Pure x)               = Right x
runErrors (Other (mkConstant e)) = Left e

failure : E → Errors E A
failure e = Other (mkConstant e)

eitherToErrors : Either E A → Errors E A
eitherToErrors = either failure Pure
