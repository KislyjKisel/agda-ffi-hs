{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Function where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Functor; Applicative; Monad)

{-# FOREIGN GHC
import qualified Data.Function
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ fℓ : Level
        A B C : Set aℓ

infixr 9 _∘_
infixl 1 _&_
infixr 0 _$_

id : A → A
id x = x

const : A → B → A
const x _ = x

_∘_ : (B → C) → (A → B) → A → C
_∘_ f g x = f (g x)

flip : (A → B → C) → B → A → C
flip f x y = f y x

_$_ : (A → B) → A → B
_$_ f = f

_&_ : A → (A → B) → B
_&_ x f = f x

{-# NON_TERMINATING #-}
fix : (A → A) → A
fix f = f (fix f)

on : (B → B → C) → (A → B) → A → A → C
on f g x y = f (g x) (g y)

-- todo: (lambda term in compile pragma) Fn instances
-- postulate
--     Functor[A⟶]     : {A : Set fℓ} → Functor     {fℓ} (λ b → (A → b))
--     Applicative[A⟶] : {A : Set fℓ} → Applicative {fℓ} (λ b → (A → b))
--     Monad[A⟶]       : {A : Set fℓ} → Monad       {fℓ} (λ b → (A → b))

-- {-# COMPILE GHC Functor[A⟶]     = \ fℓ a -> AgdaFunctor     #-}
-- {-# COMPILE GHC Applicative[A⟶] = \ fℓ a -> AgdaApplicative #-}
-- {-# COMPILE GHC Monad[A⟶]       = \ fℓ a -> AgdaMonad       #-}
