{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Bifunctor where

open import Agda.Primitive

open import Ffi.Hs.-base.Class public
    using (Bifunctor)

{-# FOREIGN GHC
import qualified Data.Bifunctor
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaBifunctor(AgdaBifunctor))
#-}

private
    variable
        aℓ bℓ : Level
        A B C D E : Set aℓ
        F : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)

postulate
    bimap  : ⦃ Bifunctor F ⦄ → (A → B) → (C → D) → F A C → F B D
    first  : ⦃ Bifunctor F ⦄ → (A → B) → F A C → F B C
    second : ⦃ Bifunctor F ⦄ → (B → C) → F A B → F A C

{-# COMPILE GHC bimap  = \ f1ℓ f2ℓ f a b c d AgdaBifunctor -> Data.Bifunctor.bimap  #-}
{-# COMPILE GHC first  = \ f1ℓ f2ℓ f a b c   AgdaBifunctor -> Data.Bifunctor.first  #-}
{-# COMPILE GHC second = \ f1ℓ f2ℓ f b c a   AgdaBifunctor -> Data.Bifunctor.second #-}
