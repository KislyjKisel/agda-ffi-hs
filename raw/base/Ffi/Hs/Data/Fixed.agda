{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Fixed where

open import Agda.Builtin.Bool using (Bool)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.Int  using () renaming (Int to Integer)
open import Agda.Builtin.List using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Eq    using (Eq)
open import Ffi.Hs.Data.Ord   using (Ord)
open import Ffi.Hs.Data.Tuple using (Tuple2)

{-# FOREIGN GHC
import qualified Data.Fixed
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ fℓ gℓ : Level
        A : Set aℓ
        B : Set bℓ
        F : Set fℓ → Set gℓ

postulate
    div'    : ⦃ Real A ⦄ → ⦃ Integral B ⦄ → A → A → B
    mod'    : ⦃ Real A ⦄ → A → A → A
    divMod' : ⦃ Real A ⦄ → ⦃ Integral B ⦄ → A → A → Tuple2 B A

    HasResolution : Set aℓ → Set aℓ
    resolution : ⦃ HasResolution A ⦄ → F A → Integer

    E0 E1 E2 E3 E6 E9 E12 : Set

    HasResolution[E0]  : HasResolution E0
    HasResolution[E1]  : HasResolution E1
    HasResolution[E2]  : HasResolution E2
    HasResolution[E3]  : HasResolution E3
    HasResolution[E6]  : HasResolution E6
    HasResolution[E9]  : HasResolution E9
    HasResolution[E12] : HasResolution E12

{-# COMPILE GHC div'    = \ aℓ bℓ a b AgdaReal AgdaIntegral -> Data.Fixed.div'    #-}
{-# COMPILE GHC mod'    = \ aℓ    a   AgdaReal              -> Data.Fixed.mod'    #-}
{-# COMPILE GHC divMod' = \ aℓ bℓ a b AgdaReal AgdaIntegral -> Data.Fixed.divMod' #-}

{-# FOREIGN GHC data AgdaHasResolution aℓ a = Data.Fixed.HasResolution a => AgdaHasResolution #-}
{-# COMPILE GHC HasResolution = type(0) AgdaHasResolution #-}
{-# COMPILE GHC resolution = \ aℓ gℓ a f AgdaHasResolution -> Data.Fixed.resolution #-}

{-# COMPILE GHC E0  = type Data.Fixed.E0  #-}
{-# COMPILE GHC E1  = type Data.Fixed.E1  #-}
{-# COMPILE GHC E2  = type Data.Fixed.E2  #-}
{-# COMPILE GHC E3  = type Data.Fixed.E3  #-}
{-# COMPILE GHC E6  = type Data.Fixed.E6  #-}
{-# COMPILE GHC E9  = type Data.Fixed.E9  #-}
{-# COMPILE GHC E12 = type Data.Fixed.E12 #-}

{-# COMPILE GHC HasResolution[E0]  = AgdaHasResolution #-}
{-# COMPILE GHC HasResolution[E1]  = AgdaHasResolution #-}
{-# COMPILE GHC HasResolution[E2]  = AgdaHasResolution #-}
{-# COMPILE GHC HasResolution[E3]  = AgdaHasResolution #-}
{-# COMPILE GHC HasResolution[E6]  = AgdaHasResolution #-}
{-# COMPILE GHC HasResolution[E9]  = AgdaHasResolution #-}
{-# COMPILE GHC HasResolution[E12] = AgdaHasResolution #-}

data Fixed (A : Set aℓ) : Set aℓ where
    MkFixed : Integer → Fixed A

{-# FOREIGN GHC type AgdaFixed aℓ = Data.Fixed.Fixed #-}
{-# COMPILE GHC Fixed = data(1) AgdaFixed (Data.Fixed.MkFixed) #-}

postulate
    showFixed : ⦃ HasResolution A ⦄ → Bool → Fixed A → List Char

{-# COMPILE GHC showFixed = \ aℓ a AgdaHasResolution -> Data.Fixed.showFixed #-}

Uni : Set
Uni = Fixed E0

Deci : Set
Deci = Fixed E1

Centi : Set
Centi = Fixed E2

Milli : Set
Milli = Fixed E3

Micro : Set
Micro = Fixed E6

Nano : Set
Nano = Fixed E9

Pico : Set
Pico = Fixed E12

postulate
    Enum[Fixed[A]]       : Enum (Fixed A)
    Num[Fixed[A]]        : ⦃ HasResolution A ⦄ → Num (Fixed A)
    Read[Fixed[A]]       : ⦃ HasResolution A ⦄ → Read (Fixed A)
    Fractional[Fixed[A]] : ⦃ HasResolution A ⦄ → Fractional (Fixed A)
    Real[Fixed[A]]       : ⦃ HasResolution A ⦄ → Real (Fixed A)
    RealFrac[Fixed[A]]   : ⦃ HasResolution A ⦄ → RealFrac (Fixed A)
    Show[Fixed[A]]       : ⦃ HasResolution A ⦄ → Show (Fixed A)
    Eq[Fixed[A]]         : Eq (Fixed A)
    Ord[Fixed[A]]        : Ord (Fixed A)

{-# COMPILE GHC Enum[Fixed[A]]       = \ aℓ a                   -> AgdaEnum       #-}
{-# COMPILE GHC Num[Fixed[A]]        = \ aℓ a AgdaHasResolution -> AgdaNum        #-}
{-# COMPILE GHC Read[Fixed[A]]       = \ aℓ a AgdaHasResolution -> AgdaRead       #-}
{-# COMPILE GHC Fractional[Fixed[A]] = \ aℓ a AgdaHasResolution -> AgdaFractional #-}
{-# COMPILE GHC Real[Fixed[A]]       = \ aℓ a AgdaHasResolution -> AgdaReal       #-}
{-# COMPILE GHC RealFrac[Fixed[A]]   = \ aℓ a AgdaHasResolution -> AgdaRealFrac   #-}
{-# COMPILE GHC Show[Fixed[A]]       = \ aℓ a AgdaHasResolution -> AgdaShow       #-}
{-# COMPILE GHC Eq[Fixed[A]]         = \ aℓ a                   -> AgdaEq         #-}
{-# COMPILE GHC Ord[Fixed[A]]        = \ aℓ a                   -> AgdaOrd        #-}
