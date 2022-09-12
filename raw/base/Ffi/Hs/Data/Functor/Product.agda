{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Product where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

{-# FOREIGN GHC
import qualified Data.Functor.Product
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ fℓ gℓ : Level
        A : Set aℓ
        F G : Set aℓ → Set aℓ

data Product (F : Set aℓ → Set fℓ) (G : Set aℓ → Set gℓ) (A : Set aℓ) : Set (aℓ ⊔ fℓ ⊔ gℓ) where
    Pair : F A → G A → Product F G A

{-# FOREIGN GHC type AgdaProduct aℓ fℓ gℓ = Data.Functor.Product.Product #-}
{-# COMPILE GHC Product = data(3) AgdaProduct (Data.Functor.Product.Pair) #-}

postulate
    MonadFix[Product[F,G]]      : ⦃ MonadFix F ⦄ → ⦃ MonadFix G ⦄ → MonadFix (Product F G)
    MonadZip[Product[F,G]]      : ⦃ MonadZip F ⦄ → ⦃ MonadZip G ⦄ → MonadZip (Product F G)
    Foldable[Product[F,G]]      : ⦃ Foldable F ⦄ → ⦃ Foldable G ⦄ → Foldable (Product F G)
    Eq1[Product[F,G]]           : ⦃ Eq1 F ⦄ → ⦃ Eq1 G ⦄ → Eq1 (Product F G)
    Ord1[Product[F,G]]          : ⦃ Ord1 F ⦄ → ⦃ Ord1 G ⦄ → Ord1 (Product F G)
    Read1[Product[F,G]]         : ⦃ Read1 F ⦄ → ⦃ Read1 G ⦄ → Read1 (Product F G)
    Show1[Product[F,G]]         : ⦃ Show1 F ⦄ → ⦃ Show1 G ⦄ → Show1 (Product F G)
    Contravariant[Product[F,G]] : ⦃ Contravariant F ⦄ → ⦃ Contravariant G ⦄ → Contravariant (Product F G)
    Traversable[Product[F,G]]   : ⦃ Traversable F ⦄ → ⦃ Traversable G ⦄ → Traversable (Product F G)
    Alternative[Product[F,G]]   : ⦃ Alternative F ⦄ → ⦃ Alternative G ⦄ → Alternative (Product F G)
    Applicative[Product[F,G]]   : ⦃ Applicative F ⦄ → ⦃ Applicative G ⦄ → Applicative (Product F G)
    Functor[Product[F,G]]       : ⦃ Functor F ⦄ → ⦃ Functor G ⦄ → Functor (Product F G)
    Monad[Product[F,G]]         : ⦃ Monad F ⦄ → ⦃ Monad G ⦄ → Monad (Product F G)
    MonadPlus[Product[F,G]]     : ⦃ MonadPlus F ⦄ → ⦃ MonadPlus G ⦄ → MonadPlus (Product F G)
    Data[Product[F,G,A]]        : ⦃ Typeable A ⦄ → ⦃ Typeable F ⦄ → ⦃ Typeable G ⦄ → ⦃ Data (F A) ⦄ → ⦃ Data (G A) ⦄ → Data (Product F G A)
    Monoid[Product[F,G,A]]      : ⦃ Monoid (F A) ⦄ → ⦃ Monoid (G A) ⦄ → Monoid (Product F G A)
    Semigroup[Product[F,G,A]]   : ⦃ Semigroup (F A) ⦄ → ⦃ Semigroup (G A) ⦄ → Semigroup (Product F G A)
    Read[Product[F,G,A]]        : ⦃ Read1 F ⦄ → ⦃ Read1 G ⦄ → ⦃ Read A ⦄ → Read (Product F G A)
    Show[Product[F,G,A]]        : ⦃ Show1 F ⦄ → ⦃ Show1 G ⦄ → ⦃ Show A ⦄ → Show (Product F G A)
    Eq[Product[F,G,A]]          : ⦃ Eq1 F ⦄ → ⦃ Eq1 G ⦄ → ⦃ Eq A ⦄ → Eq (Product F G A)
    Ord[Product[F,G,A]]         : ⦃ Ord1 F ⦄ → ⦃ Ord1 G ⦄ → ⦃ Ord A ⦄ → Ord (Product F G A)

{-# COMPILE GHC MonadFix[Product[F,G]]      = \ fℓ f gℓ g AgdaMonadFix AgdaMonadFix           -> AgdaMonadFix      #-}
{-# COMPILE GHC MonadZip[Product[F,G]]      = \ fℓ f gℓ g AgdaMonadZip AgdaMonadZip           -> AgdaMonadZip      #-}
{-# COMPILE GHC Foldable[Product[F,G]]      = \ aℓ fℓ f gℓ g AgdaFoldable AgdaFoldable        -> AgdaFoldable      #-}
{-# COMPILE GHC Eq1[Product[F,G]]           = \ aℓ fℓ f gℓ g AgdaEq1 AgdaEq1                  -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[Product[F,G]]          = \ aℓ fℓ f gℓ g AgdaOrd1 AgdaOrd1                -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[Product[F,G]]         = \ aℓ fℓ f gℓ g AgdaRead1 AgdaRead1              -> AgdaRead1         #-}
{-# COMPILE GHC Show1[Product[F,G]]         = \ aℓ fℓ f gℓ g AgdaShow1 AgdaShow1              -> AgdaShow1         #-}
{-# COMPILE GHC Contravariant[Product[F,G]] = \ fℓ f gℓ g AgdaContravariant AgdaContravariant -> AgdaContravariant #-}
{-# COMPILE GHC Traversable[Product[F,G]]   = \ aℓ fℓ f gℓ g AgdaTraversable AgdaTraversable  -> AgdaTraversable   #-}
{-# COMPILE GHC Alternative[Product[F,G]]   = \ fℓ f gℓ g AgdaAlternative AgdaAlternative     -> AgdaAlternative   #-}
{-# COMPILE GHC Applicative[Product[F,G]]   = \ fℓ f gℓ g AgdaApplicative AgdaApplicative     -> AgdaApplicative   #-}
{-# COMPILE GHC Functor[Product[F,G]]       = \ fℓ f gℓ g AgdaFunctor AgdaFunctor             -> AgdaFunctor       #-}
{-# COMPILE GHC Monad[Product[F,G]]         = \ fℓ f gℓ g AgdaMonad AgdaMonad                 -> AgdaMonad         #-}
{-# COMPILE GHC MonadPlus[Product[F,G]]     = \ fℓ f gℓ g AgdaMonadPlus AgdaMonadPlus         -> AgdaMonadPlus     #-}
{-# COMPILE GHC Monoid[Product[F,G,A]]      = \ aℓ fℓ f gℓ g a AgdaMonoid AgdaMonoid          -> AgdaMonoid        #-}
{-# COMPILE GHC Semigroup[Product[F,G,A]]   = \ aℓ fℓ f gℓ g a AgdaSemigroup AgdaSemigroup    -> AgdaSemigroup     #-}
{-# COMPILE GHC Read[Product[F,G,A]]        = \ aℓ fℓ f gℓ g a AgdaRead1 AgdaRead1 AgdaRead   -> AgdaRead          #-}
{-# COMPILE GHC Show[Product[F,G,A]]        = \ aℓ fℓ f gℓ g a AgdaShow1 AgdaShow1 AgdaShow   -> AgdaShow          #-}
{-# COMPILE GHC Eq[Product[F,G,A]]          = \ aℓ fℓ f gℓ g a AgdaEq1 AgdaEq1 AgdaEq         -> AgdaEq            #-}
{-# COMPILE GHC Ord[Product[F,G,A]]         = \ aℓ fℓ f gℓ g a AgdaOrd1 AgdaOrd1 AgdaOrd      -> AgdaOrd           #-}
{-# COMPILE GHC Data[Product[F,G,A]] =
    \ aℓ a fℓ f gℓ g AgdaTypeable AgdaTypeable AgdaTypeable AgdaData AgdaData -> AgdaData #-}
