{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Num where

open import Agda.Builtin.Nat      using (zero; suc)
open import Agda.Builtin.Unit     using (⊤)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Literals

open Ffi.Hs.-base.Class public
    using (Num)

open import Agda.Builtin.Int as ℤ public
    using ()
    renaming (Int to Integer)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified GHC.Num
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


Lit-FromNat[Integer] : Lit-FromNat Integer
Lit-FromNat[Integer] .Lit-ConstrainNat s = ⊤
Lit-FromNat[Integer] .Lit-fromNat      s = ℤ.pos s

Lit-FromNeg[Integer] : Lit-FromNeg Integer
Lit-FromNeg[Integer] .Lit-ConstrainNeg n = ⊤
Lit-FromNeg[Integer] .Lit-fromNeg zero    = ℤ.pos 0
Lit-FromNeg[Integer] .Lit-fromNeg (suc n) = ℤ.negsuc n

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
