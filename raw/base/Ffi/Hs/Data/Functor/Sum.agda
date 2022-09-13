{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Sum where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

{-# FOREIGN GHC
import qualified Data.Functor.Sum
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ fℓ gℓ : Level
        A : Set aℓ
        F : Set aℓ → Set fℓ
        G : Set aℓ → Set gℓ

data Sum (F : Set aℓ → Set fℓ) (G : Set aℓ → Set gℓ) (A : Set aℓ) : Set (aℓ ⊔ fℓ ⊔ gℓ) where
    InL : F A → Sum F G A
    InR : G A → Sum F G A

{-# FOREIGN GHC type AgdaSum aℓ fℓ gℓ = Data.Functor.Sum.Sum #-}
{-# COMPILE GHC Sum = data(3) AgdaSum (Data.Functor.Sum.InL | Data.Functor.Sum.InR) #-}

postulate
    Foldable[Sum[F,G]]      : ⦃ Foldable F ⦄ → ⦃ Foldable G ⦄ → Foldable (Sum F G)
    Contravariant[Sum[F,G]] : ⦃ Contravariant F ⦄ → ⦃ Contravariant G ⦄ → Contravariant (Sum F G)
    Traversable[Sum[F,G]]   : ⦃ Traversable F ⦄ → ⦃ Traversable G ⦄ → Traversable (Sum F G)
    Functor[Sum[F,G]]       : ⦃ Functor F ⦄ → ⦃ Functor G ⦄ → Functor (Sum F G)
    Read[Sum[F,G,A]]        : ⦃ Read1 F ⦄ → ⦃ Read1 G ⦄ → ⦃ Read A ⦄ → Read (Sum F G A)
    Show[Sum[F,G,A]]        : ⦃ Show1 F ⦄ → ⦃ Show1 G ⦄ → ⦃ Show A ⦄ → Show (Sum F G A)
    Eq[Sum[F,G,A]]          : ⦃ Eq1 F ⦄ → ⦃ Eq1 G ⦄ → ⦃ Eq A ⦄ → Eq (Sum F G A)
    Ord[Sum[F,G,A]]         : ⦃ Ord1 F ⦄ → ⦃ Ord1 G ⦄ → ⦃ Ord A ⦄ → Ord (Sum F G A)
    Eq1[Sum[F,G]]           : ⦃ Eq1 F ⦄ → ⦃ Eq1 G ⦄ → Eq1 (Sum F G)
    Ord1[Sum[F,G]]          : ⦃ Ord1 F ⦄ → ⦃ Ord1 G ⦄ → Ord1 (Sum F G)
    Read1[Sum[F,G]]         : ⦃ Read1 F ⦄ → ⦃ Read1 G ⦄ → Read1 (Sum F G)
    Show1[Sum[F,G]]         : ⦃ Show1 F ⦄ → ⦃ Show1 G ⦄ → Show1 (Sum F G)
    -- todo: Data instance - Typeable kind
    -- Data[Sum[F,G,A]] : ⦃ Typeable A ⦄ → ⦃ Typeable F ⦄ → ⦃ Typeable G ⦄ →
    --                   ⦃ Data (F A) ⦄ → ⦃ Data (G A) ⦄ → Data (Sum F G A)

{-# COMPILE GHC Foldable[Sum[F,G]]      = \ aℓ fℓ f gℓ g AgdaFoldable AgdaFoldable       -> AgdaFoldable      #-}
{-# COMPILE GHC Contravariant[Sum[F,G]] = \ aℓ f g AgdaContravariant AgdaContravariant   -> AgdaContravariant #-}
{-# COMPILE GHC Traversable[Sum[F,G]]   = \ aℓ fℓ f gℓ g AgdaTraversable AgdaTraversable -> AgdaTraversable   #-}
{-# COMPILE GHC Functor[Sum[F,G]]       = \ aℓ f g AgdaFunctor AgdaFunctor               -> AgdaFunctor       #-}
{-# COMPILE GHC Read[Sum[F,G,A]]        = \ aℓ fℓ f gℓ g a AgdaRead1 AgdaRead1 AgdaRead  -> AgdaRead          #-}
{-# COMPILE GHC Show[Sum[F,G,A]]        = \ aℓ fℓ f gℓ g a AgdaShow1 AgdaShow1 AgdaShow  -> AgdaShow          #-}
{-# COMPILE GHC Eq[Sum[F,G,A]]          = \ aℓ fℓ f gℓ g a AgdaEq1 AgdaEq1 AgdaEq        -> AgdaEq            #-}
{-# COMPILE GHC Ord[Sum[F,G,A]]         = \ aℓ fℓ f gℓ g a AgdaOrd1 AgdaOrd1 AgdaOrd     -> AgdaOrd           #-}
{-# COMPILE GHC Eq1[Sum[F,G]]           = \ aℓ fℓ f gℓ g AgdaEq1 AgdaEq1                 -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[Sum[F,G]]          = \ aℓ fℓ f gℓ g AgdaOrd1 AgdaOrd1               -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[Sum[F,G]]         = \ aℓ fℓ f gℓ g AgdaRead1 AgdaRead1             -> AgdaRead1         #-}
{-# COMPILE GHC Show1[Sum[F,G]]         = \ aℓ fℓ f gℓ g AgdaShow1 AgdaShow1             -> AgdaShow1         #-}
-- {-# COMPILE GHC Data[Sum[F,G,A]] = \ aℓ fℓ f gℓ g a AgdaTypeable AgdaTypeable AgdaTypeable AgdaData AgdaData -> AgdaData #-}
