{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Prelude where

open import Agda.Builtin.Bool  using (Bool; true; false)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive     using (Level)
open import Ffi.Hs.-base.Class using (Enum)

import Agda.Builtin.Strict

{-# FOREIGN GHC
import qualified Prelude
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaEnum(AgdaEnum))
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ


case_return_of_ : ∀{aℓ bℓ} {A : Set aℓ} → (x : A) → (B : A → Set bℓ) → ((x : A) → B x) → B x
case x return B of f = f x
{-# INLINE case_return_of_ #-}

case_of_ : ∀{aℓ bℓ} {A : Set aℓ} {B : Set bℓ} → A → (A → B) → B
case x of f = f x
{-# INLINE case_of_ #-}

_::_ : ∀{aℓ} (A : Set aℓ) → A → A
_ :: x = x
{-# INLINE _::_ #-}

if_then_else_ : Bool → A → A → A
if true  then x else _ = x
if false then _ else y = y
{-# COMPILE GHC if_then_else_ = \ aℓ a b x y -> if b then x else y #-}

asTypeOf : A → A → A
asTypeOf x _ = x
{-# COMPILE GHC asTypeOf = \ aℓ a -> Prelude.asTypeOf #-}

{-# NON_TERMINATING #-}
until : (A → Bool) → (A → A) → A → A
until p f x = if p x then x else until p f (f x)

-- todo: different runtime reps
postulate
    seq : A → B → B

{-# COMPILE GHC seq = \ aℓ a bℓ b -> Prelude.seq #-}

_$!_ : ∀{aℓ bℓ} {A : Set aℓ} {B : A → Set bℓ} → (∀ x → B x) → (x : A) → B x
f $! x = Agda.Builtin.Strict.primForce x f
{-# COMPILE GHC _$!_ = \ aℓ bℓ a b -> (Prelude.$!) #-}

postulate
    [_⋯_] : ⦃ Enum A ⦄ → A → A → List A
    [_,_⋯_] : ⦃ Enum A ⦄ → A → A → A → List A

{-# COMPILE GHC [_⋯_]   = \ aℓ a AgdaEnum x y     -> [x..y]     #-}
{-# COMPILE GHC [_,_⋯_] = \ aℓ a AgdaEnum x1 x2 y -> [x1,x2..y] #-}
