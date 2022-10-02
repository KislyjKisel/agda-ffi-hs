{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Compose where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Functor.Compose
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ fℓ gℓ : Level
        A : Set aℓ
        F : Set gℓ → Set fℓ
        G : Set aℓ → Set gℓ

data Compose (F : Set gℓ → Set fℓ) (G : Set aℓ → Set gℓ) (A : Set aℓ) : Set (aℓ ⊔ fℓ ⊔ gℓ) where
    mkCompose : F (G A) → Compose F G A

{-# FOREIGN GHC type AgdaCompose aℓ fℓ gℓ = Data.Functor.Compose.Compose #-}
{-# COMPILE GHC Compose = data(3) AgdaCompose (Data.Functor.Compose.Compose) #-}

postulate
    TestEquality[Compose[F,G]]  : ⦃ TestEquality F ⦄ → TestEquality (Compose F G)
    Foldable[Compose[F,G]]      : ⦃ Foldable F ⦄ → ⦃ Foldable G ⦄ → Foldable (Compose F G)
    Eq1[Compose[F,G]]           : ⦃ Eq1 F ⦄ → ⦃ Eq1 G ⦄ → Eq1 (Compose F G)
    Ord1[Compose[F,G]]          : ⦃ Ord1 F ⦄ → ⦃ Ord1 G ⦄ → Ord1 (Compose F G)
    Read1[Compose[F,G]]         : ⦃ Read1 F ⦄ → ⦃ Read1 G ⦄ → Read1 (Compose F G)
    Show1[Compose[F,G]]         : ⦃ Show1 F ⦄ → ⦃ Show1 G ⦄ → Show1 (Compose F G)
    Contravariant[Compose[F,G]] : ⦃ Functor F ⦄ → ⦃ Contravariant G ⦄ → Contravariant (Compose F G)
    Traversable[Compose[F,G]]   : ⦃ Traversable F ⦄ → ⦃ Traversable G ⦄ → Traversable (Compose F G)
    Alternative[Compose[F,G]]   : ⦃ Alternative F ⦄ → ⦃ Alternative G ⦄ → Alternative (Compose F G)
    Applicative[Compose[F,G]]   : ⦃ Applicative F ⦄ → ⦃ Applicative G ⦄ → Applicative (Compose F G)
    Functor[Compose[F,G]]       : ⦃ Functor F ⦄ → ⦃ Functor G ⦄ → Functor (Compose F G)
    -- todo: Data instance requires Typeable kind
    -- Data[Compose[F,G,A]]        : ⦃ Typeable A ⦄ → ⦃ Typeable F ⦄ → ⦃ Typeable G ⦄ → ⦃ Data (F (G A)) ⦄ → Data (Compose F G A)
    Monoid[Compose[F,G,A]]      : ⦃ Monoid (F (G A)) ⦄ → Monoid (Compose F G A)
    Semigroup[Compose[F,G,A]]   : ⦃ Semigroup (F (G A)) ⦄ → Semigroup (Compose F G A)
    Read[Compose[F,G,A]]        : ⦃ Read1 F ⦄ → ⦃ Read1 G ⦄ → ⦃ Read A ⦄ → Read (Compose F G A)
    Show[Compose[F,G,A]]        : ⦃ Show1 F ⦄ → ⦃ Show1 G ⦄ → ⦃ Show A ⦄ → Show (Compose F G A)
    Eq[Compose[F,G,A]]          : ⦃ Eq1 F ⦄ → ⦃ Eq1 G ⦄ → ⦃ Eq A ⦄ → Eq (Compose F G A)
    Ord[Compose[F,G,A]]         : ⦃ Ord1 F ⦄ → ⦃ Ord1 G ⦄ → ⦃ Ord A ⦄ → Ord (Compose F G A)

{-# COMPILE GHC TestEquality[Compose[F,G]]  = \ gℓ fℓ f aℓ g AgdaTestEquality                 -> AgdaTestEquality  #-}
{-# COMPILE GHC Foldable[Compose[F,G]]      = \ aℓ fℓ f gℓ g AgdaFoldable AgdaFoldable        -> AgdaFoldable      #-}
{-# COMPILE GHC Eq1[Compose[F,G]]           = \ aℓ fℓ f gℓ g AgdaEq1 AgdaEq1                  -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[Compose[F,G]]          = \ aℓ fℓ f gℓ g AgdaOrd1 AgdaOrd1                -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[Compose[F,G]]         = \ aℓ fℓ f gℓ g AgdaRead1 AgdaRead1              -> AgdaRead1         #-}
{-# COMPILE GHC Show1[Compose[F,G]]         = \ aℓ fℓ f gℓ g AgdaShow1 AgdaShow1              -> AgdaShow1         #-}
{-# COMPILE GHC Contravariant[Compose[F,G]] = \ fℓ f g AgdaFunctor AgdaContravariant          -> AgdaContravariant #-}
{-# COMPILE GHC Traversable[Compose[F,G]]   = \ fℓ f g AgdaTraversable AgdaTraversable        -> AgdaTraversable   #-}
{-# COMPILE GHC Alternative[Compose[F,G]]   = \ fℓ f g AgdaAlternative AgdaAlternative        -> AgdaAlternative   #-}
{-# COMPILE GHC Applicative[Compose[F,G]]   = \ fℓ f g AgdaApplicative AgdaApplicative        -> AgdaApplicative   #-}
{-# COMPILE GHC Functor[Compose[F,G]]       = \ fℓ f g AgdaFunctor AgdaFunctor                -> AgdaFunctor       #-}
{-# COMPILE GHC Monoid[Compose[F,G,A]]      = \ aℓ fℓ f gℓ g a AgdaMonoid                     -> AgdaMonoid        #-}
{-# COMPILE GHC Semigroup[Compose[F,G,A]]   = \ aℓ fℓ f gℓ g a AgdaSemigroup                  -> AgdaSemigroup     #-}
{-# COMPILE GHC Read[Compose[F,G,A]]        = \ aℓ fℓ f gℓ g a AgdaRead1 AgdaRead1 AgdaRead   -> AgdaRead          #-}
{-# COMPILE GHC Show[Compose[F,G,A]]        = \ aℓ fℓ f gℓ g a AgdaShow1 AgdaShow1 AgdaShow   -> AgdaShow          #-}
{-# COMPILE GHC Eq[Compose[F,G,A]]          = \ aℓ fℓ f gℓ g a AgdaEq1 AgdaEq1 AgdaEq         -> AgdaEq            #-}
{-# COMPILE GHC Ord[Compose[F,G,A]]         = \ aℓ fℓ f gℓ g a AgdaOrd1 AgdaOrd1 AgdaOrd      -> AgdaOrd           #-}
-- {-# COMPILE GHC Data[Compose[F,G,A]] =
--   \ aℓ a fℓ f gℓ g AgdaTypeable AgdaTypeable AgdaTypeable AgdaData -> AgdaData #-}
