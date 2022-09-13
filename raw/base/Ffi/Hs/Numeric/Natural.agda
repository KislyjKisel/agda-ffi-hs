{-# OPTIONS --without-K #-}

module Ffi.Hs.Numeric.Natural where

open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Numeric.Natural
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

postulate
    Natural : Set

    Data[Natural]     : Data Natural
    Bits[Natural]     : Bits Natural
    Enum[Natural]     : Enum Natural
    Ix[Natural]       : Ix Natural
    Num[Natural]      : Num Natural
    Read[Natural]     : Read Natural
    Integral[Natural] : Integral Natural
    Real[Natural]     : Real Natural
    Show[Natural]     : Show Natural
    Eq[Natural]       : Eq Natural
    Ord[Natural]      : Ord Natural

{-# COMPILE GHC Natural = type Numeric.Natural.Natural #-}

{-# COMPILE GHC Data[Natural]     = AgdaData     #-}
{-# COMPILE GHC Bits[Natural]     = AgdaBits     #-}
{-# COMPILE GHC Enum[Natural]     = AgdaEnum     #-}
{-# COMPILE GHC Ix[Natural]       = AgdaIx       #-}
{-# COMPILE GHC Num[Natural]      = AgdaNum      #-}
{-# COMPILE GHC Read[Natural]     = AgdaRead     #-}
{-# COMPILE GHC Integral[Natural] = AgdaIntegral #-}
{-# COMPILE GHC Real[Natural]     = AgdaReal     #-}
{-# COMPILE GHC Show[Natural]     = AgdaShow     #-}
{-# COMPILE GHC Eq[Natural]       = AgdaEq       #-}
{-# COMPILE GHC Ord[Natural]      = AgdaOrd      #-}
