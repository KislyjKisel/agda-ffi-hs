{-# OPTIONS --without-K #-}

module Ffi.Hs.Numeric.Half where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.Int as ℤ  using ()
open import Agda.Primitive         using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.DeepSeq using (NFData)
open import Ffi.Hs.Foreign.C.Types using (CUShort; Num[CUShort])
open import Ffi.Hs.GHC.Float       using (Float)
open import Ffi.Hs.GHC.Num         using (fromInteger)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Numeric.Half
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData (AgdaNFData))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


record Half : Set where
    constructor mkHalf
    field
        getHalf : CUShort

open Half public

{-# COMPILE GHC Half = data Numeric.Half.Half (Numeric.Half.Half) #-}

postulate
    Eq[Half]         : Eq Half
    Floating[Half]   : Floating Half
    Fractional[Half] : Fractional Half
    Num[Half]        : Num Half
    Ord[Half]        : Ord Half
    Read[Half]       : Read Half
    Real[Half]       : Real Half
    RealFloat[Half]  : RealFloat Half
    RealFrac[Half]   : RealFrac Half
    Show[Half]       : Show Half
    Storable[Half]   : Storable Half
    NFData[Half]     : NFData Half
    -- todo: Binary instance for Half

{-# COMPILE GHC Eq[Half]         = AgdaEq         #-}
{-# COMPILE GHC Floating[Half]   = AgdaFloating   #-}
{-# COMPILE GHC Fractional[Half] = AgdaFractional #-}
{-# COMPILE GHC Num[Half]        = AgdaNum        #-}
{-# COMPILE GHC Ord[Half]        = AgdaOrd        #-}
{-# COMPILE GHC Read[Half]       = AgdaRead       #-}
{-# COMPILE GHC Real[Half]       = AgdaReal       #-}
{-# COMPILE GHC RealFloat[Half]  = AgdaRealFloat  #-}
{-# COMPILE GHC RealFrac[Half]   = AgdaRealFrac   #-}
{-# COMPILE GHC Show[Half]       = AgdaShow       #-}
{-# COMPILE GHC Storable[Half]   = AgdaStorable   #-}
{-# COMPILE GHC NFData[Half]     = AgdaNFData     #-}

postulate
    isZero   : Half → Bool
    fromHalf : Half → Float
    toHalf   : Float → Half

{-# COMPILE GHC isZero   = Numeric.Half.isZero   #-}
{-# COMPILE GHC fromHalf = Numeric.Half.fromHalf #-}
{-# COMPILE GHC toHalf   = Numeric.Half.toHalf   #-}

POS-INF : Half
POS-INF = mkHalf (fromInteger ⦃ Num[CUShort] ⦄ (ℤ.pos 0x7c00))
{-# COMPILE GHC POS-INF = Numeric.Half.POS_INF #-}

NEG-INF : Half
NEG-INF = mkHalf (fromInteger ⦃ Num[CUShort] ⦄ (ℤ.pos 0xfc00))
{-# COMPILE GHC NEG-INF = Numeric.Half.NEG_INF #-}

QNaN : Half
QNaN = mkHalf (fromInteger ⦃ Num[CUShort] ⦄ (ℤ.pos 0x7fff))
{-# COMPILE GHC QNaN = Numeric.Half.QNaN #-}

SNaN : Half
SNaN = mkHalf (fromInteger ⦃ Num[CUShort] ⦄ (ℤ.pos 0x7c00))
{-# COMPILE GHC SNaN = Numeric.Half.SNaN #-}

HALF-MIN : Half
HALF-MIN = mkHalf (fromInteger ⦃ Num[CUShort] ⦄ (ℤ.pos 0x0001))
{-# COMPILE GHC HALF-MIN = Numeric.Half.HALF_MIN #-}

HALF-NRM-MIN : Half
HALF-NRM-MIN = mkHalf (fromInteger ⦃ Num[CUShort] ⦄ (ℤ.pos 0x0400))
{-# COMPILE GHC HALF-NRM-MIN = Numeric.Half.HALF_NRM_MIN #-}

HALF-MAX : Half
HALF-MAX = mkHalf (fromInteger ⦃ Num[CUShort] ⦄ (ℤ.pos 0x7bff))
{-# COMPILE GHC HALF-MAX = Numeric.Half.HALF_MAX #-}

HALF-EPSILON : Half
HALF-EPSILON = mkHalf (fromInteger ⦃ Num[CUShort] ⦄ (ℤ.pos 0x1400))
{-# COMPILE GHC HALF-EPSILON = Numeric.Half.HALF_EPSILON #-}

HALF-DIG : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
HALF-DIG = fromInteger (ℤ.pos 2)
{-# COMPILE GHC HALF-DIG = \ aℓ a AgdaEq AgdaNum -> Numeric.Half.HALF_DIG #-}

HALF-MIN-10-EXP : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
HALF-MIN-10-EXP = fromInteger (ℤ.negsuc 3)
{-# COMPILE GHC HALF-MIN-10-EXP = \ aℓ a AgdaEq AgdaNum -> Numeric.Half.HALF_MIN_10_EXP #-}

HALF-MAX-10-EXP : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
HALF-MAX-10-EXP = fromInteger (ℤ.pos 4)
{-# COMPILE GHC HALF-MAX-10-EXP = \ aℓ a AgdaEq AgdaNum -> Numeric.Half.HALF_MAX_10_EXP #-}
