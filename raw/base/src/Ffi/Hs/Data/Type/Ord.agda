{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Type.Ord where

open import Agda.Primitive
open import Ffi.Hs.-base.Class        using (Eq; Show)
open import Ffi.Hs.Data.Bool          using (`Bool)
open import Ffi.Hs.Data.Ord           using (`Ordering; `LT; `EQ; `GT)
open import Ffi.Hs.Data.Type.Equality using (_~_)

{-# FOREIGN GHC
import qualified Data.Type.Ord
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaEq, AgdaShow)
#-}

private
    variable
        aℓ : Level
        K : Set (lsuc aℓ)
        A B : K

postulate
    Compare : K → K → `Ordering

{-# FOREIGN GHC type AgdaCompare aℓ k = Data.Type.Ord.Compare #-}
{-# COMPILE GHC Compare = type(2) AgdaCompare #-}

data OrderingI (A B : K) : Set₁ where
    LTI : ⦃ Compare A B ~ `LT ⦄ → OrderingI A B
    EQI : ⦃ Compare A B ~ `EQ ⦄ → OrderingI A B
    GTI : ⦃ Compare A B ~ `GT ⦄ → OrderingI A B

{-# FOREIGN GHC type AgdaOrderingI aℓ k = Data.Type.Ord.OrderingI #-}
{-# COMPILE GHC OrderingI = data(2) AgdaOrderingI (Data.Type.Ord.LTI | Data.Type.Ord.EQI | Data.Type.Ord.GTI) #-}

infix 4 _<=?_ _>=?_ _>?_ _<?_ -- _<=_ _>=_ _>_ _<_

postulate
    Show[OrderingI] : Show (OrderingI A B)
    Eq[OrderingI]   : Eq (OrderingI A B)

    OrdCond : `Ordering → K → K → K → K
    -- _<=_ : 
    _<=?_ : K → K → `Bool
    -- _>=_ : 
    _>=?_ : K → K → `Bool
    -- _>_ : 
    _>?_ : K → K → `Bool
    -- _<_ : 
    _<?_ : K → K → `Bool
    Max : K → K → K
    Min : K → K → K

{-# COMPILE GHC Show[OrderingI] = \ aℓ k a b -> AgdaShow #-}
{-# COMPILE GHC Eq[OrderingI]   = \ aℓ k a b -> AgdaEq   #-}

{-# FOREIGN GHC type AgdaOrdCond aℓ k = Data.Type.Ord.OrdCond #-}
{-# COMPILE GHC OrdCond = type(2) AgdaOrdCond #-}

{-# FOREIGN GHC type AgdaLessEqQ    aℓ k = (Data.Type.Ord.<=?) #-}
{-# FOREIGN GHC type AgdaGreaterEqQ aℓ k = (Data.Type.Ord.>=?) #-}
{-# FOREIGN GHC type AgdaGreaterQ   aℓ k = (Data.Type.Ord.>?)  #-}
{-# FOREIGN GHC type AgdaLessQ      aℓ k = (Data.Type.Ord.<?)  #-}
{-# COMPILE GHC _<=?_ = type(2) AgdaLessEqQ    #-}
{-# COMPILE GHC _>=?_ = type(2) AgdaGreaterEqQ #-}
{-# COMPILE GHC _>?_  = type(2) AgdaGreaterQ   #-}
{-# COMPILE GHC _<?_  = type(2) AgdaLessQ      #-}
