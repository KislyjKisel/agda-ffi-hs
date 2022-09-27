{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Num where

open import Agda.Builtin.Int   using (Int)
open import Agda.Primitive
open import Ffi.Hs.-base.Class

open Ffi.Hs.-base.Class public
    using (Num)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified GHC.Num
import MAlonzo.Code.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

Integer : Set
Integer = Int

postulate
    Data[Integer]     : Data Integer
    Bits[Integer]     : Bits Integer
    Enum[Integer]     : Enum Integer
    Ix[Integer]       : Ix Integer
    Num[Integer]      : Num Integer
    Read[Integer]     : Read Integer
    Integral[Integer] : Integral Integer
    Real[Integer]     : Real Integer
    Show[Integer]     : Show Integer
    Eq[Integer]       : Eq Integer
    Ord[Integer]      : Ord Integer

{-# COMPILE GHC Data[Integer]     = AgdaData     #-}
{-# COMPILE GHC Bits[Integer]     = AgdaBits     #-}
{-# COMPILE GHC Enum[Integer]     = AgdaEnum     #-}
{-# COMPILE GHC Ix[Integer]       = AgdaIx       #-}
{-# COMPILE GHC Num[Integer]      = AgdaNum      #-}
{-# COMPILE GHC Read[Integer]     = AgdaRead     #-}
{-# COMPILE GHC Integral[Integer] = AgdaIntegral #-}
{-# COMPILE GHC Real[Integer]     = AgdaReal     #-}
{-# COMPILE GHC Show[Integer]     = AgdaShow     #-}
{-# COMPILE GHC Eq[Integer]       = AgdaEq       #-}
{-# COMPILE GHC Ord[Integer]      = AgdaOrd      #-}


infixl 7 _*_
infixl 6 _+_ _-_

postulate
    _+_         : ⦃ Num A ⦄ → A → A → A
    _-_         : ⦃ Num A ⦄ → A → A → A
    _*_         : ⦃ Num A ⦄ → A → A → A
    negate      : ⦃ Num A ⦄ → A → A
    abs         : ⦃ Num A ⦄ → A → A
    signum      : ⦃ Num A ⦄ → A → A
    fromInteger : ⦃ Num A ⦄ → Integer → A
    subtract    : ⦃ Num A ⦄ → A → A → A

{-# COMPILE GHC _+_                = \ aℓ a AgdaNum -> (GHC.Num.+)         #-}
{-# COMPILE GHC _-_                = \ aℓ a AgdaNum -> (GHC.Num.-)         #-}
{-# COMPILE GHC _*_                = \ aℓ a AgdaNum -> (GHC.Num.*)         #-}
{-# COMPILE GHC negate             = \ aℓ a AgdaNum -> GHC.Num.negate      #-}
{-# COMPILE GHC abs                = \ aℓ a AgdaNum -> GHC.Num.abs         #-}
{-# COMPILE GHC signum             = \ aℓ a AgdaNum -> GHC.Num.signum      #-}
{-# COMPILE GHC fromInteger        = \ aℓ a AgdaNum -> GHC.Num.fromInteger #-}
{-# COMPILE GHC subtract           = \ aℓ a AgdaNum -> GHC.Num.subtract    #-}
