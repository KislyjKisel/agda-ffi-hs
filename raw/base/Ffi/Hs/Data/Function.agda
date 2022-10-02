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

{-# INLINE id #-}
{-# COMPILE GHC id = \ aℓ a -> Data.Function.id #-}

const : A → B → A
const x _ = x

{-# INLINE const #-}
{-# COMPILE GHC const = \ aℓ a bℓ b -> Data.Function.const #-}

_∘_ : (B → C) → (A → B) → A → C
_∘_ f g x = f (g x)

{-# INLINE _∘_ #-}
{-# COMPILE GHC _∘_ = \ bℓ b cℓ c aℓ a -> (Data.Function..) #-}

flip : (A → B → C) → B → A → C
flip f x y = f y x

{-# INLINE flip #-}
{-# COMPILE GHC flip = \ aℓ a bℓ b cℓ c -> Data.Function.flip #-}

_$_ : (A → B) → A → B
_$_ f = f

{-# INLINE _$_ #-}
{-# COMPILE GHC _$_ = \ aℓ a bℓ b -> (Data.Function.$) #-}

_&_ : A → (A → B) → B
_&_ x f = f x

{-# INLINE _&_ #-}
{-# COMPILE GHC _&_ = \ aℓ a bℓ b -> (Data.Function.&) #-}

{-# NON_TERMINATING #-}
fix : (A → A) → A
fix f = f (fix f)

{-# COMPILE GHC fix = \ aℓ a -> Data.Function.fix #-}

on : (B → B → C) → (A → B) → A → A → C
on f g x y = f (g x) (g y)

{-# INLINE on #-}
{-# COMPILE GHC on = \ bℓ b cℓ c aℓ a -> Data.Function.on #-}

-- todo: (lambda term in compile pragma) Fn instances
-- postulate
-- Functor[A⟶]     : {A : Set fℓ} → Functor     {fℓ} (λ b → (A → b))
-- Applicative[A⟶] : {A : Set fℓ} → Applicative {fℓ} (λ b → (A → b))
-- Monad[A⟶]       : {A : Set fℓ} → Monad       {fℓ} (λ b → (A → b))
-- Category[⟶]    : Category {aℓ} {bℓ} (\ a b → (a → b))
-- Arrow[⟶]       : Arrow {aℓ} {bℓ} (λ a b → (a → b))
-- ArrowChoice[⟶] : ArrowChoice {aℓ} {bℓ} (λ a b → (a → b))
-- ArrowApply[⟶]  : ArrowApply {aℓ} {bℓ} (λ a b → (a → b))
-- ArrowLoop[⟶]   : ArrowLoop {aℓ} {bℓ} (λ a b → (a → b))

-- {-# COMPILE GHC Functor[A⟶]     = \ fℓ a -> AgdaFunctor     #-}
-- {-# COMPILE GHC Applicative[A⟶] = \ fℓ a -> AgdaApplicative #-}
-- {-# COMPILE GHC Monad[A⟶]       = \ fℓ a -> AgdaMonad       #-}
-- {-# COMPILE GHC Category[⟶] = \ aℓ bℓ -> AgdaCategory #-}
-- {-# COMPILE GHC Arrow[⟶]       = \ aℓ bℓ -> AgdaArrow       #-}
-- {-# COMPILE GHC ArrowChoice[⟶] = \ aℓ bℓ -> AgdaArrowChoice #-}
-- {-# COMPILE GHC ArrowApply[⟶]  = \ aℓ bℓ -> AgdaArrowApply  #-}
-- {-# COMPILE GHC ArrowLoop[⟶]   = \ aℓ bℓ -> AgdaArrowLoop   #-}