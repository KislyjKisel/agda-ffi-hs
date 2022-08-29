{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor where

open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤)

private
    variable
        aℓ bℓ fℓ : Level
        A : Set aℓ
        B : Set bℓ
        F : Set fℓ → Set fℓ

infixl 4 _<$_ _$>_ _<$>_
infixl 1 _<&>_

postulate
    Functor : (Set fℓ → Set fℓ) → Set fℓ
    fmap    : ⦃ Functor F ⦄ → (A → B) → F A → F B
    _<$_    : ⦃ Functor F ⦄ → A → F B → F A
    _$>_    : ⦃ Functor F ⦄ → F A → B → F B
    _<$>_   : ⦃ Functor F ⦄ → (A → B) → F A → F B
    _<&>_   : ⦃ Functor F ⦄ → F A → (A → B) → F B
    void    : ⦃ Functor F ⦄ → F A → F ⊤

{-# FOREIGN GHC import qualified Data.Functor #-}

{-# FOREIGN GHC data AgdaFunctor fℓ f = Data.Functor.Functor f => AgdaFunctor #-}
{-# COMPILE GHC Functor = type(0) AgdaFunctor #-}

{-# COMPILE GHC fmap  = \ ℓ f a b AgdaFunctor -> Data.Functor.fmap  #-}
{-# COMPILE GHC _<$_  = \ ℓ f a b AgdaFunctor -> (Data.Functor.<$)  #-}
{-# COMPILE GHC _$>_  = \ ℓ f a b AgdaFunctor -> (Data.Functor.$>)  #-}
{-# COMPILE GHC _<$>_ = \ ℓ f a b AgdaFunctor -> (Data.Functor.<$>) #-}
{-# COMPILE GHC _<&>_ = \ ℓ f a b AgdaFunctor -> (Data.Functor.<&>) #-}
{-# COMPILE GHC void  = \ ℓ f a b AgdaFunctor -> Data.Functor.void  #-}
