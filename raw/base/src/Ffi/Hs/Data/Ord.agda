{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Ord where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Primitive
open import Ffi.Hs.Data.Tuple using (Tuple2)

open import Ffi.Hs.-base.Class public
    using (Ord)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

data Ordering : Set where
    LT EQ GT : Ordering

{-# FOREIGN GHC import qualified Data.Ord #-}
{-# COMPILE GHC Ordering = data Data.Ord.Ordering (LT | EQ | GT) #-}

infix 4 _<_ _<=_ _>_ _>=_

postulate
    _<_  : ⦃ Ord A ⦄ → A → A → Bool
    _<=_ : ⦃ Ord A ⦄ → A → A → Bool
    _>_  : ⦃ Ord A ⦄ → A → A → Bool
    _>=_ : ⦃ Ord A ⦄ → A → A → Bool

    compare : ⦃ Ord A ⦄ → A → A → Ordering
    min     : ⦃ Ord A ⦄ → A → A → A
    max     : ⦃ Ord A ⦄ → A → A → A

    comparing : ⦃ Ord A ⦄ → (B → A) → B → B → Ordering
    clamp     : ⦃ Ord A ⦄ → Tuple2 A A → A → A

{-# COMPILE GHC _<_  = \ aℓ a AgdaOrd -> (<)  #-}
{-# COMPILE GHC _<=_ = \ aℓ a AgdaOrd -> (<=) #-}
{-# COMPILE GHC _>_  = \ aℓ a AgdaOrd -> (>)  #-}
{-# COMPILE GHC _>=_ = \ aℓ a AgdaOrd -> (>=) #-}

{-# COMPILE GHC compare = \ aℓ a AgdaOrd -> compare #-}
{-# COMPILE GHC min     = \ aℓ a AgdaOrd -> min     #-}
{-# COMPILE GHC max     = \ aℓ a AgdaOrd -> max     #-}

{-# COMPILE GHC comparing = \ aℓ a b AgdaOrd -> Data.Ord.comparing #-}
{-# COMPILE GHC clamp     = \ aℓ a   AgdaOrd -> Data.Ord.clamp     #-}

postulate
    Down    : Set aℓ → Set aℓ
    mkDown  : A → Down A
    getDown : Down A → A

    Ord[Down] : ⦃ Ord A ⦄ → Ord (Down A)

{-# FOREIGN GHC type AgdaDown aℓ = Data.Ord.Data              #-}
{-# COMPILE GHC Down             = type(1) AgdaDown           #-}
{-# COMPILE GHC mkDown           = \ aℓ a -> Data.Ord.Down    #-}
{-# COMPILE GHC getDown          = \ aℓ a -> Data.Ord.getDown #-}
