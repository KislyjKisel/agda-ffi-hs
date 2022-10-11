{-# OPTIONS --without-K #-}

module Ffi.Hs.Numeric.Fixed where

open import Agda.Builtin.Int as ℤ  using ()
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Foreign.C.Types using (CInt; Integral[CInt])
open import Ffi.Hs.GHC.Float       using (Double; Num[Double]; Fractional[Double]; RealFrac[Double])
open import Ffi.Hs.GHC.Num         using (fromInteger; _+_; _*_)
open import Ffi.Hs.GHC.Real        using (fromIntegral; floor; _/_)

{-# FOREIGN GHC
import qualified Numeric.Fixed
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}


record Fixed : Set where
    constructor mkFixed
    field
        getFixed : CInt

open Fixed public

{-# COMPILE GHC Fixed = data Numeric.Fixed.Fixed (Numeric.Fixed.Fixed) #-}

postulate
    Bounded[Fixed]    : Bounded Fixed
    Enum[Fixed]       : Enum Fixed
    Eq[Fixed]         : Eq Fixed
    Floating[Fixed]   : Floating Fixed
    Fractional[Fixed] : Fractional Fixed
    Num[Fixed]        : Num Fixed
    Ord[Fixed]        : Ord Fixed
    Real[Fixed]       : Real Fixed
    RealFloat[Fixed]  : RealFloat Fixed
    RealFrac[Fixed]   : RealFrac Fixed
    Show[Fixed]       : Show Fixed
    Storable[Fixed]   : Storable Fixed

{-# COMPILE GHC Bounded[Fixed]    = AgdaBounded    #-}
{-# COMPILE GHC Enum[Fixed]       = AgdaEnum       #-}
{-# COMPILE GHC Eq[Fixed]         = AgdaEq         #-}
{-# COMPILE GHC Floating[Fixed]   = AgdaFloating   #-}
{-# COMPILE GHC Fractional[Fixed] = AgdaFractional #-}
{-# COMPILE GHC Num[Fixed]        = AgdaNum        #-}
{-# COMPILE GHC Ord[Fixed]        = AgdaOrd        #-}
{-# COMPILE GHC Real[Fixed]       = AgdaReal       #-}
{-# COMPILE GHC RealFloat[Fixed]  = AgdaRealFloat  #-}
{-# COMPILE GHC RealFrac[Fixed]   = AgdaRealFrac   #-}
{-# COMPILE GHC Show[Fixed]       = AgdaShow       #-}
{-# COMPILE GHC Storable[Fixed]   = AgdaStorable   #-}


fromFixed : Fixed → Double
fromFixed (mkFixed x) = (fromIntegral x) / (fromInteger (ℤ.pos 65536))
    where
    instance
        _ = Fractional[Double]
        _ = Integral[CInt]
        _ = Num[Double]

toFixed : Double → Fixed
toFixed x = mkFixed (floor (x * (fromInteger (ℤ.pos 65536)) + 0.5))
    where
    instance
        _ = Integral[CInt]
        _ = Num[Double]
        _ = RealFrac[Double]

{-# COMPILE GHC fromFixed = Ffi.Hs.Numeric.Fixed.fromFixed #-}
{-# COMPILE GHC toFixed   = Ffi.Hs.Numeric.Fixed.toFixed   #-}
