{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor where

open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤; ⊤′)

open import Ffi.Hs.-base.Class public
    using (Functor)

{-# FOREIGN GHC
import qualified Data.Functor
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ fℓ : Level
        A : Set aℓ
        B : Set bℓ
        F : Set fℓ → Set fℓ

infixl 4 _<$_ _$>_ _<$>_
infixl 1 _<&>_

postulate
    fmap    : ⦃ Functor F ⦄ → (A → B) → F A → F B
    _<$_    : ⦃ Functor F ⦄ → A → F B → F A
    _$>_    : ⦃ Functor F ⦄ → F A → B → F B
    _<$>_   : ⦃ Functor F ⦄ → (A → B) → F A → F B
    _<&>_   : ⦃ Functor F ⦄ → F A → (A → B) → F B
    void    : ⦃ Functor F ⦄ → F A → F ⊤′

{-# COMPILE GHC fmap  = \ fℓ f a b AgdaFunctor -> Data.Functor.fmap  #-}
{-# COMPILE GHC _<$_  = \ fℓ f a b AgdaFunctor -> (Data.Functor.<$)  #-}
{-# COMPILE GHC _$>_  = \ fℓ f a b AgdaFunctor -> (Data.Functor.$>)  #-}
{-# COMPILE GHC _<$>_ = \ fℓ f a b AgdaFunctor -> (Data.Functor.<$>) #-}
{-# COMPILE GHC _<&>_ = \ fℓ f a b AgdaFunctor -> (Data.Functor.<&>) #-}
{-# COMPILE GHC void  = \ fℓ f a AgdaFunctor   -> Data.Functor.void  #-}
