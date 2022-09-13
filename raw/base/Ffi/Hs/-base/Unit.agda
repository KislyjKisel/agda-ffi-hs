{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Unit where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

{-# FOREIGN GHC
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level

record ⊤ {ℓ} : Set ℓ where
    instance constructor tt

{-# FOREIGN GHC type AgdaUnit ℓ = ()      #-}
{-# COMPILE GHC ⊤ = data(1) AgdaUnit (()) #-}

cast : ⊤ {aℓ} → ⊤ {bℓ}
cast _ = tt

postulate
    Eq[⊤]        : Eq {aℓ} ⊤
    Ord[⊤]       : Ord {aℓ} ⊤
    Enum[⊤]      : Enum {aℓ} ⊤
    Bounded[⊤]   : Bounded {aℓ} ⊤
    Semigroup[⊤] : Semigroup {aℓ} ⊤
    Monoid[⊤]    : Monoid {aℓ} ⊤
    Show[⊤]      : Show {aℓ} ⊤
    Read[⊤]      : Read {aℓ} ⊤

{-# COMPILE GHC Eq[⊤]        = \ aℓ -> AgdaEq        #-}
{-# COMPILE GHC Ord[⊤]       = \ aℓ -> AgdaOrd       #-}
{-# COMPILE GHC Enum[⊤]      = \ aℓ -> AgdaEnum      #-}
{-# COMPILE GHC Bounded[⊤]   = \ aℓ -> AgdaBounded   #-}
{-# COMPILE GHC Semigroup[⊤] = \ aℓ -> AgdaSemigroup #-}
{-# COMPILE GHC Monoid[⊤]    = \ aℓ -> AgdaMonoid    #-}
{-# COMPILE GHC Show[⊤]      = \ aℓ -> AgdaShow      #-}
{-# COMPILE GHC Read[⊤]      = \ aℓ -> AgdaRead      #-}
