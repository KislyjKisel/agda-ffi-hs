{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Sum where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Functor; Foldable; Traversable; Contravariant)

{-# FOREIGN GHC
import qualified Data.Functor.Sum
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaFoldable, AgdaFunctor, AgdaTraversable, AgdaContravariant
    )
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        F G : Set aℓ → Set aℓ

data Sum (F G : Set aℓ → Set aℓ) (A : Set aℓ) : Set aℓ where
    InL : F A → Sum F G A
    InR : G A → Sum F G A

{-# FOREIGN GHC type AgdaSum aℓ = Data.Functor.Sum.Sum #-}
{-# COMPILE GHC Sum = data(1) AgdaSum (Data.Functor.Sum.InL | Data.Functor.Sum.InR) #-}

postulate
    Foldable[Sum[F,G]]      : ⦃ Foldable F ⦄ → ⦃ Foldable G ⦄ → Foldable (Sum F G)
    Contravariant[Sum[F,G]] : ⦃ Contravariant F ⦄ → ⦃ Contravariant G ⦄ → Contravariant (Sum F G)
    Traversable[Sum[F,G]]   : ⦃ Traversable F ⦄ → ⦃ Traversable G ⦄ → Traversable (Sum F G)
    Functor[Sum[F,G]]       : ⦃ Functor F ⦄ → ⦃ Functor G ⦄ → Functor (Sum F G)
    -- Data[Sum[F,G,A]]        : ? -> Data (Sum F G A)
    -- Read[Sum[F,G,A]]        : ? -> Read (Sum F G A)
    -- Show[Sum[F,G,A]]        : ? -> Show (Sum F G A)
    -- Eq[Sum[F,G,A]]          : ? -> Eq (Sum F G A)
    -- Ord[Sum[F,G,A]]         : ? -> Ord (Sum F G A)

{-# COMPILE GHC Foldable[Sum[F,G]]      = \ aℓ f g AgdaFoldable AgdaFoldable           -> AgdaFoldable      #-}
{-# COMPILE GHC Contravariant[Sum[F,G]] = \ aℓ f g AgdaContravariant AgdaContravariant -> AgdaContravariant #-}
{-# COMPILE GHC Traversable[Sum[F,G]]   = \ aℓ f g AgdaTraversable AgdaTraversable     -> AgdaTraversable   #-}
{-# COMPILE GHC Functor[Sum[F,G]]       = \ aℓ f g AgdaFunctor AgdaFunctor             -> AgdaFunctor       #-}
