{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Ord where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Primitive
open import Ffi.Hs.-base.Kind  using (IsKind)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

open import Ffi.Hs.-base.Class public
    using (Ord)

{-# FOREIGN GHC
import qualified Data.Ord
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaOrd)
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Kind (AgdaIsKind)
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

data Ordering : Set where
    LT EQ GT : Ordering

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

{-# COMPILE GHC _<_  = \ aℓ a AgdaOrd -> (Data.Ord.<)  #-}
{-# COMPILE GHC _<=_ = \ aℓ a AgdaOrd -> (Data.Ord.<=) #-}
{-# COMPILE GHC _>_  = \ aℓ a AgdaOrd -> (Data.Ord.>)  #-}
{-# COMPILE GHC _>=_ = \ aℓ a AgdaOrd -> (Data.Ord.>=) #-}

{-# COMPILE GHC compare = \ aℓ a AgdaOrd -> Data.Ord.compare #-}
{-# COMPILE GHC min     = \ aℓ a AgdaOrd -> Data.Ord.min     #-}
{-# COMPILE GHC max     = \ aℓ a AgdaOrd -> Data.Ord.max     #-}

{-# COMPILE GHC comparing = \ aℓ a b AgdaOrd -> Data.Ord.comparing #-}
{-# COMPILE GHC clamp     = \ aℓ a   AgdaOrd -> Data.Ord.clamp     #-}

postulate
    `Ordering : Set₁
    `LT `EQ `GT : `Ordering
    IsKind[`Ordering] : IsKind `Ordering

{-# COMPILE GHC `Ordering = type Data.Ord.Ordering #-}
{-# COMPILE GHC `LT = type 'Data.Ord.LT #-}
{-# COMPILE GHC `EQ = type 'Data.Ord.EQ #-}
{-# COMPILE GHC `GT = type 'Data.Ord.GT #-}
{-# COMPILE GHC IsKind[`Ordering] = AgdaIsKind #-}

postulate
    Down    : Set aℓ → Set aℓ
    mkDown  : A → Down A
    getDown : Down A → A

    Ord[Down] : ⦃ Ord A ⦄ → Ord (Down A)

{-# FOREIGN GHC type AgdaDown aℓ = Data.Ord.Data #-}
{-# COMPILE GHC Down = type(1) AgdaDown #-}
{-# COMPILE GHC mkDown    = \ aℓ a         -> Data.Ord.Down    #-}
{-# COMPILE GHC getDown   = \ aℓ a         -> Data.Ord.getDown #-}
{-# COMPILE GHC Ord[Down] = \ aℓ a AgdaOrd -> AgdaOrd          #-}
