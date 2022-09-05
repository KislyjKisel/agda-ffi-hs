{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Category where

open import Agda.Primitive

open import Ffi.Hs.-base.Class public
    using (Category)

private
    variable
        aℓ bℓ cℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        Cat : Set aℓ → Set bℓ → Set cℓ

infixr 9 _∘_
infixr 1 _<<<_ _>>>_

postulate
    id    : ⦃ Category Cat ⦄ → Cat A A
    _∘_   : ⦃ Category Cat ⦄ → Cat B C → Cat A B → Cat A C

_<<<_ : ⦃ Category Cat ⦄ → Cat B C → Cat A B → Cat A C
_<<<_ = _∘_

_>>>_ : ⦃ Category Cat ⦄ → Cat A B → Cat B C → Cat A C
f >>> g = g ∘ f

postulate
    Category[⟶] : Category {aℓ} {bℓ} (\ a b → (a → b))

{-# FOREIGN GHC import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaCategory) #-}
{-# FOREIGN GHC import qualified Control.Category as AgdaHsConCat #-}
{-# COMPILE GHC id  = \ aℓ a rℓ cat AgdaCategory ->  AgdaHsConCat.id  #-}
{-# COMPILE GHC _∘_ = \ aℓ cat a b c AgdaCategory -> (AgdaHsConCat..) #-}
{-# COMPILE GHC Category[⟶] = \ aℓ bℓ -> AgdaCategory #-}
