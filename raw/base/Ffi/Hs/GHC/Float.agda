{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Float where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.Int       using () renaming (Int to Integer)
open import Agda.Builtin.List      using (List)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Int        using (Int)
open import Ffi.Hs.Data.Tuple      using (Tuple2)
open import Ffi.Hs.Data.Word       using (Word; Word32; Word64)
open import Ffi.Hs.GHC.Exts        using (Int#; Word32#; Word64#)
open import Ffi.Hs.GHC.Real        using (Rational)
open import Ffi.Hs.Numeric.Natural using (Natural)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified GHC.Float
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

open Ffi.Hs.-base.Class public
    using (Floating; RealFloat)

open import Agda.Builtin.Float public
    using () renaming (Float to Double)

open Ffi.Hs.GHC.Exts public
    using (Float; F#; D#; unD#; Float#; Double#)

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    pi          : ⦃ Floating A ⦄ → A
    exp         : ⦃ Floating A ⦄ → A
    log         : ⦃ Floating A ⦄ → A
    sqrt        : ⦃ Floating A ⦄ → A → A
    sin         : ⦃ Floating A ⦄ → A → A
    cos         : ⦃ Floating A ⦄ → A → A
    tan         : ⦃ Floating A ⦄ → A → A
    asin        : ⦃ Floating A ⦄ → A → A
    acos        : ⦃ Floating A ⦄ → A → A
    atan        : ⦃ Floating A ⦄ → A → A
    sinh        : ⦃ Floating A ⦄ → A → A
    cosh        : ⦃ Floating A ⦄ → A → A
    tanh        : ⦃ Floating A ⦄ → A → A
    asinh       : ⦃ Floating A ⦄ → A → A
    acosh       : ⦃ Floating A ⦄ → A → A
    atanh       : ⦃ Floating A ⦄ → A → A
    _**_        : ⦃ Floating A ⦄ → A → A → A
    logBase     : ⦃ Floating A ⦄ → A → A → A
    log1p       : ⦃ Floating A ⦄ → A → A
    expm1       : ⦃ Floating A ⦄ → A → A
    log1pexp    : ⦃ Floating A ⦄ → A → A
    log1mexp    : ⦃ Floating A ⦄ → A → A

    floatRadix     : ⦃ RealFloat A ⦄ → A → Integer
    floatDigits    : ⦃ RealFloat A ⦄ → A → Int
    floatRange     : ⦃ RealFloat A ⦄ → A → Tuple2 Int Int
    decodeFloat    : ⦃ RealFloat A ⦄ → A → Tuple2 Integer Int
    encodeFloat    : ⦃ RealFloat A ⦄ → Integer → Int → A
    exponent       : ⦃ RealFloat A ⦄ → A → Int
    significand    : ⦃ RealFloat A ⦄ → A → A
    scaleFloat     : ⦃ RealFloat A ⦄ → Int → A → A
    isNaN          : ⦃ RealFloat A ⦄ → A → Bool
    isInfinite     : ⦃ RealFloat A ⦄ → A → Bool
    isDenormalized : ⦃ RealFloat A ⦄ → A → Bool
    isNegativeZero : ⦃ RealFloat A ⦄ → A → Bool
    isIEEE         : ⦃ RealFloat A ⦄ → A → Bool
    atan2          : ⦃ RealFloat A ⦄ → A → A → A

{-# COMPILE GHC pi          = \ aℓ a AgdaFloating -> GHC.Float.pi          #-}
{-# COMPILE GHC exp         = \ aℓ a AgdaFloating -> GHC.Float.exp         #-}
{-# COMPILE GHC log         = \ aℓ a AgdaFloating -> GHC.Float.log         #-}
{-# COMPILE GHC sqrt        = \ aℓ a AgdaFloating -> GHC.Float.sqrt        #-}
{-# COMPILE GHC sin         = \ aℓ a AgdaFloating -> GHC.Float.sin         #-}
{-# COMPILE GHC cos         = \ aℓ a AgdaFloating -> GHC.Float.cos         #-}
{-# COMPILE GHC tan         = \ aℓ a AgdaFloating -> GHC.Float.tan         #-}
{-# COMPILE GHC asin        = \ aℓ a AgdaFloating -> GHC.Float.asin        #-}
{-# COMPILE GHC acos        = \ aℓ a AgdaFloating -> GHC.Float.acos        #-}
{-# COMPILE GHC atan        = \ aℓ a AgdaFloating -> GHC.Float.atan        #-}
{-# COMPILE GHC sinh        = \ aℓ a AgdaFloating -> GHC.Float.sinh        #-}
{-# COMPILE GHC cosh        = \ aℓ a AgdaFloating -> GHC.Float.cosh        #-}
{-# COMPILE GHC tanh        = \ aℓ a AgdaFloating -> GHC.Float.tanh        #-}
{-# COMPILE GHC asinh       = \ aℓ a AgdaFloating -> GHC.Float.asinh       #-}
{-# COMPILE GHC acosh       = \ aℓ a AgdaFloating -> GHC.Float.acosh       #-}
{-# COMPILE GHC atanh       = \ aℓ a AgdaFloating -> GHC.Float.atanh       #-}
{-# COMPILE GHC _**_        = \ aℓ a AgdaFloating -> (GHC.Float.**)        #-}
{-# COMPILE GHC logBase     = \ aℓ a AgdaFloating -> GHC.Float.logBase     #-}
{-# COMPILE GHC log1p       = \ aℓ a AgdaFloating -> GHC.Float.log1p       #-}
{-# COMPILE GHC expm1       = \ aℓ a AgdaFloating -> GHC.Float.expm1       #-}
{-# COMPILE GHC log1pexp    = \ aℓ a AgdaFloating -> GHC.Float.log1pexp    #-}
{-# COMPILE GHC log1mexp    = \ aℓ a AgdaFloating -> GHC.Float.log1mexp    #-}

{-# COMPILE GHC floatRadix     = \ aℓ a AgdaRealFloat -> GHC.Float.floatRadix     #-}
{-# COMPILE GHC floatDigits    = \ aℓ a AgdaRealFloat -> GHC.Float.floatDigits    #-}
{-# COMPILE GHC floatRange     = \ aℓ a AgdaRealFloat -> GHC.Float.floatRange     #-}
{-# COMPILE GHC decodeFloat    = \ aℓ a AgdaRealFloat -> GHC.Float.decodeFloat    #-}
{-# COMPILE GHC encodeFloat    = \ aℓ a AgdaRealFloat -> GHC.Float.encodeFloat    #-}
{-# COMPILE GHC exponent       = \ aℓ a AgdaRealFloat -> GHC.Float.exponent       #-}
{-# COMPILE GHC significand    = \ aℓ a AgdaRealFloat -> GHC.Float.significand    #-}
{-# COMPILE GHC scaleFloat     = \ aℓ a AgdaRealFloat -> GHC.Float.scaleFloat     #-}
{-# COMPILE GHC isNaN          = \ aℓ a AgdaRealFloat -> GHC.Float.isNaN          #-}
{-# COMPILE GHC isInfinite     = \ aℓ a AgdaRealFloat -> GHC.Float.isInfinite     #-}
{-# COMPILE GHC isDenormalized = \ aℓ a AgdaRealFloat -> GHC.Float.isDenormalized #-}
{-# COMPILE GHC isNegativeZero = \ aℓ a AgdaRealFloat -> GHC.Float.isNegativeZero #-}
{-# COMPILE GHC isIEEE         = \ aℓ a AgdaRealFloat -> GHC.Float.isIEEE         #-}
{-# COMPILE GHC atan2          = \ aℓ a AgdaRealFloat -> GHC.Float.atan2          #-}


data FFFormat : Set where
    FFExponent : FFFormat
    FFFixed    : FFFormat
    FFGeneric  : FFFormat

{-# COMPILE GHC FFFormat = data GHC.Float.FFFormat
    ( GHC.Float.FFExponent
    | GHC.Float.FFFixed
    | GHC.Float.FFGeneric
    ) #-}


postulate
    clamp                 : Int → Int → Int
    showFloat             : ⦃ RealFloat A ⦄ → A → List Char → List Char
    floatToDigits         : ⦃ RealFloat A ⦄ → Integer → A → Tuple2 (List Int) Int
    fromRat               : ⦃ RealFloat A ⦄ → Rational → A
    formatRealFloat       : ⦃ RealFloat A ⦄ → FFFormat → Maybe Int → A → List Char
    log1mexpOrd           : ⦃ Ord A ⦄ → ⦃ Floating A ⦄ →  A → A
    plusFloat             : Float → Float → Float
    minusFloat            : Float → Float → Float
    negateFloat           : Float → Float
    timesFloat            : Float → Float → Float
    fabsFloat             : Float → Float
    integerToFloat#       : Integer → Float#
    integerToBinaryFloat' : ⦃ RealFloat A ⦄ → Integer → A
    naturalToFloat#       : Natural → Float#
    divideFloat           : Float → Float → Float
    rationalToFloat       : Integer → Integer → Float
    fromRat''             : ⦃ RealFloat A ⦄ → Int → Int → Integer → Integer → A
    properFractionFloat   : ⦃ Integral A ⦄ → Float → Tuple2 A Float
    truncateFloat         : ⦃ Integral A ⦄ → Float → A
    roundFloat            : ⦃ Integral A ⦄ → Float → A
    floorFloat            : ⦃ Integral A ⦄ → Float → A
    ceilingFloat          : ⦃ Integral A ⦄ → Float → A
    expFloat              : Float → Float
    logFloat              : Float → Float
    sqrtFloat             : Float → Float
    sinFloat              : Float → Float
    cosFloat              : Float → Float
    tanFloat              : Float → Float
    asinFloat             : Float → Float
    acosFloat             : Float → Float
    atanFloat             : Float → Float
    sinhFloat             : Float → Float
    coshFloat             : Float → Float
    tanhFloat             : Float → Float
    powerFloat            : Float → Float → Float
    asinhFloat            : Float → Float
    acoshFloat            : Float → Float
    atanhFloat            : Float → Float
    log1pFloat            : Float → Float
    expm1Float            : Float → Float
    isFloatFinite         : Float → Int
    isFloatNaN            : Float → Int
    isFloatInfinite       : Float → Int
    isFloatDenormalized   : Float → Int
    isFloatNegativeZero   : Float → Int
    showSignedFloat       : ⦃ RealFloat A ⦄ → (A → List Char) → Int → A → List Char → List Char
    plusDouble            : Double → Double → Double
    minusDouble           : Double → Double → Double
    negateDouble          : Double → Double
    timesDouble           : Double → Double → Double
    fabsDouble            : Double → Double
    integerToDouble#      : Integer → Double#
    naturalToDouble#      : Natural → Double#
    divideDouble          : Double → Double → Double
    rationalToDouble      : Integer → Integer → Double
    expDouble             : Double → Double
    logDouble             : Double → Double
    sqrtDouble            : Double → Double
    sinDouble             : Double → Double
    cosDouble             : Double → Double
    tanDouble             : Double → Double
    asinDouble            : Double → Double
    acosDouble            : Double → Double
    atanDouble            : Double → Double
    sinhDouble            : Double → Double
    coshDouble            : Double → Double
    tanhDouble            : Double → Double
    powerDouble           : Double → Double → Double
    asinhDouble           : Double → Double
    acoshDouble           : Double → Double
    atanhDouble           : Double → Double
    log1pDouble           : Double → Double
    expm1Double           : Double → Double
    properFractionDouble  : ⦃ Integral A ⦄ → Double → Tuple2 A Double
    truncateDouble        : ⦃ Integral A ⦄ → Double → A
    roundDouble           : ⦃ Integral A ⦄ → Double → A
    ceilingDouble         : ⦃ Integral A ⦄ → Double → A
    floorDouble           : ⦃ Integral A ⦄ → Double → A
    isDoubleFinite        : Double → Int
    isDoubleNaN           : Double → Int
    isDoubleInfinite      : Double → Int
    isDoubleDenormalized  : Double → Int
    isDoubleNegativeZero  : Double → Int
    formatRealFloatAlt    : ⦃ RealFloat A ⦄ → FFFormat → Maybe Int → Bool → A → List Char
    roundTo               : Int → Int → List Int → Tuple2 Int (List Int)
    expt                  : Integer → Int → Integer
    roundingMode#         : Integer → Int# → Int#
    fromRat'              : ⦃ RealFloat A ⦄ → Rational → A
    minExpt               : Int
    maxExpt               : Int
    -- todo: expts                 : Array Int Integer
    maxExpt10             : Int
    -- todo: expts10               : Array Int Integer
    gtFloat               : Float → Float → Bool
    geFloat               : Float → Float → Bool
    ltFloat               : Float → Float → Bool
    leFloat               : Float → Float → Bool
    gtDouble              : Double → Double → Bool
    geDouble              : Double → Double → Bool
    leDouble              : Double → Double → Bool
    ltDouble              : Double → Double → Bool
    double2Float          : Double → Float
    float2Double          : Float → Double
    word2Double           : Word → Double
    word2Float            : Word → Float
    castWord32ToFloat     : Word32 → Float
    stgWord32ToFloat      : Word32# → Float#
    castFloatToWord32     : Float → Word32
    stgFloatToWord32      : Float# → Word32#
    castWord64ToDouble    : Word64 → Double
    stgWord64ToDouble     : Word64# → Double#
    castDoubleToWord64    : Double → Word64
    stgDoubleToWord64     : Double# → Word64#
    double2Int            : Double → Int
    int2Double            : Int → Double
    float2Int             : Float → Int
    int2Float             : Int → Float
    eqFloat               : Float → Float → Bool
    eqDouble              : Double → Double → Bool

{-# COMPILE GHC clamp                 =                                GHC.Float.clamp                 #-}
{-# COMPILE GHC showFloat             = \ aℓ a AgdaRealFloat        -> GHC.Float.showFloat             #-}
{-# COMPILE GHC floatToDigits         = \ aℓ a AgdaRealFloat        -> GHC.Float.floatToDigits         #-}
{-# COMPILE GHC fromRat               = \ aℓ a AgdaRealFloat        -> GHC.Float.fromRat               #-}
{-# COMPILE GHC formatRealFloat       = \ aℓ a AgdaRealFloat        -> GHC.Float.formatRealFloat       #-}
{-# COMPILE GHC log1mexpOrd           = \ aℓ a AgdaOrd AgdaFloating -> GHC.Float.log1mexpOrd           #-}
{-# COMPILE GHC plusFloat             =                                GHC.Float.plusFloat             #-}
{-# COMPILE GHC minusFloat            =                                GHC.Float.minusFloat            #-}
{-# COMPILE GHC negateFloat           =                                GHC.Float.negateFloat           #-}
{-# COMPILE GHC timesFloat            =                                GHC.Float.timesFloat            #-}
{-# COMPILE GHC fabsFloat             =                                GHC.Float.fabsFloat             #-}
{-# COMPILE GHC integerToFloat#       =                                GHC.Float.integerToFloat#       #-}
{-# COMPILE GHC integerToBinaryFloat' = \ aℓ a AgdaRealFloat        -> GHC.Float.integerToBinaryFloat' #-}
{-# COMPILE GHC naturalToFloat#       =                                GHC.Float.naturalToFloat#       #-}
{-# COMPILE GHC divideFloat           =                                GHC.Float.divideFloat           #-}
{-# COMPILE GHC rationalToFloat       =                                GHC.Float.rationalToFloat       #-}
{-# COMPILE GHC fromRat''             = \ aℓ a AgdaRealFloat        -> GHC.Float.fromRat''             #-}
{-# COMPILE GHC properFractionFloat   = \ aℓ a AgdaIntegral         -> GHC.Float.properFractionFloat   #-}
{-# COMPILE GHC truncateFloat         = \ aℓ a AgdaIntegral         -> GHC.Float.truncateFloat         #-}
{-# COMPILE GHC roundFloat            = \ aℓ a AgdaIntegral         -> GHC.Float.roundFloat            #-}
{-# COMPILE GHC floorFloat            = \ aℓ a AgdaIntegral         -> GHC.Float.floorFloat            #-}
{-# COMPILE GHC ceilingFloat          = \ aℓ a AgdaIntegral         -> GHC.Float.ceilingFloat          #-}
{-# COMPILE GHC expFloat              =                                GHC.Float.expFloat              #-}
{-# COMPILE GHC logFloat              =                                GHC.Float.logFloat              #-}
{-# COMPILE GHC sqrtFloat             =                                GHC.Float.sqrtFloat             #-}
{-# COMPILE GHC sinFloat              =                                GHC.Float.sinFloat              #-}
{-# COMPILE GHC cosFloat              =                                GHC.Float.cosFloat              #-}
{-# COMPILE GHC tanFloat              =                                GHC.Float.tanFloat              #-}
{-# COMPILE GHC asinFloat             =                                GHC.Float.asinFloat             #-}
{-# COMPILE GHC acosFloat             =                                GHC.Float.acosFloat             #-}
{-# COMPILE GHC atanFloat             =                                GHC.Float.atanFloat             #-}
{-# COMPILE GHC sinhFloat             =                                GHC.Float.sinhFloat             #-}
{-# COMPILE GHC coshFloat             =                                GHC.Float.coshFloat             #-}
{-# COMPILE GHC tanhFloat             =                                GHC.Float.tanhFloat             #-}
{-# COMPILE GHC powerFloat            =                                GHC.Float.powerFloat            #-}
{-# COMPILE GHC asinhFloat            =                                GHC.Float.asinhFloat            #-}
{-# COMPILE GHC acoshFloat            =                                GHC.Float.acoshFloat            #-}
{-# COMPILE GHC atanhFloat            =                                GHC.Float.atanhFloat            #-}
{-# COMPILE GHC log1pFloat            =                                GHC.Float.log1pFloat            #-}
{-# COMPILE GHC expm1Float            =                                GHC.Float.expm1Float            #-}
{-# COMPILE GHC isFloatFinite         =                                GHC.Float.isFloatFinite         #-}
{-# COMPILE GHC isFloatNaN            =                                GHC.Float.isFloatNaN            #-}
{-# COMPILE GHC isFloatInfinite       =                                GHC.Float.isFloatInfinite       #-}
{-# COMPILE GHC isFloatDenormalized   =                                GHC.Float.isFloatDenormalized   #-}
{-# COMPILE GHC isFloatNegativeZero   =                                GHC.Float.isFloatNegativeZero   #-}
{-# COMPILE GHC showSignedFloat       = \ aℓ a AgdaRealFloat        -> GHC.Float.showSignedFloat       #-}
{-# COMPILE GHC plusDouble            =                                GHC.Float.plusDouble            #-}
{-# COMPILE GHC minusDouble           =                                GHC.Float.minusDouble           #-}
{-# COMPILE GHC negateDouble          =                                GHC.Float.negateDouble          #-}
{-# COMPILE GHC timesDouble           =                                GHC.Float.timesDouble           #-}
{-# COMPILE GHC fabsDouble            =                                GHC.Float.fabsDouble            #-}
{-# COMPILE GHC integerToDouble#      =                                GHC.Float.integerToDouble#      #-}
{-# COMPILE GHC naturalToDouble#      =                                GHC.Float.naturalToDouble#      #-}
{-# COMPILE GHC divideDouble          =                                GHC.Float.divideDouble          #-}
{-# COMPILE GHC rationalToDouble      =                                GHC.Float.rationalToDouble      #-}
{-# COMPILE GHC expDouble             =                                GHC.Float.expDouble             #-}
{-# COMPILE GHC logDouble             =                                GHC.Float.logDouble             #-}
{-# COMPILE GHC sqrtDouble            =                                GHC.Float.sqrtDouble            #-}
{-# COMPILE GHC sinDouble             =                                GHC.Float.sinDouble             #-}
{-# COMPILE GHC cosDouble             =                                GHC.Float.cosDouble             #-}
{-# COMPILE GHC tanDouble             =                                GHC.Float.tanDouble             #-}
{-# COMPILE GHC asinDouble            =                                GHC.Float.asinDouble            #-}
{-# COMPILE GHC acosDouble            =                                GHC.Float.acosDouble            #-}
{-# COMPILE GHC atanDouble            =                                GHC.Float.atanDouble            #-}
{-# COMPILE GHC sinhDouble            =                                GHC.Float.sinhDouble            #-}
{-# COMPILE GHC coshDouble            =                                GHC.Float.coshDouble            #-}
{-# COMPILE GHC tanhDouble            =                                GHC.Float.tanhDouble            #-}
{-# COMPILE GHC powerDouble           =                                GHC.Float.powerDouble           #-}
{-# COMPILE GHC asinhDouble           =                                GHC.Float.asinhDouble           #-}
{-# COMPILE GHC acoshDouble           =                                GHC.Float.acoshDouble           #-}
{-# COMPILE GHC atanhDouble           =                                GHC.Float.atanhDouble           #-}
{-# COMPILE GHC log1pDouble           =                                GHC.Float.log1pDouble           #-}
{-# COMPILE GHC expm1Double           =                                GHC.Float.expm1Double           #-}
{-# COMPILE GHC properFractionDouble  = \ aℓ a AgdaIntegral         -> GHC.Float.properFractionDouble  #-}
{-# COMPILE GHC truncateDouble        = \ aℓ a AgdaIntegral         -> GHC.Float.truncateDouble        #-}
{-# COMPILE GHC roundDouble           = \ aℓ a AgdaIntegral         -> GHC.Float.roundDouble           #-}
{-# COMPILE GHC ceilingDouble         = \ aℓ a AgdaIntegral         -> GHC.Float.ceilingDouble         #-}
{-# COMPILE GHC floorDouble           = \ aℓ a AgdaIntegral         -> GHC.Float.floorDouble           #-}
{-# COMPILE GHC isDoubleFinite        =                                GHC.Float.isDoubleFinite        #-}
{-# COMPILE GHC isDoubleNaN           =                                GHC.Float.isDoubleNaN           #-}
{-# COMPILE GHC isDoubleInfinite      =                                GHC.Float.isDoubleInfinite      #-}
{-# COMPILE GHC isDoubleDenormalized  =                                GHC.Float.isDoubleDenormalized  #-}
{-# COMPILE GHC isDoubleNegativeZero  =                                GHC.Float.isDoubleNegativeZero  #-}
{-# COMPILE GHC formatRealFloatAlt    = \ aℓ a AgdaRealFloat        -> GHC.Float.formatRealFloatAlt    #-}
{-# COMPILE GHC roundTo               =                                GHC.Float.roundTo               #-}
{-# COMPILE GHC expt                  =                                GHC.Float.expt                  #-}
{-# COMPILE GHC roundingMode#         =                                GHC.Float.roundingMode#         #-}
{-# COMPILE GHC fromRat'              = \ aℓ a AgdaRealFloat        -> GHC.Float.fromRat'              #-}
{-# COMPILE GHC minExpt               =                                GHC.Float.minExpt               #-}
{-# COMPILE GHC maxExpt               =                                GHC.Float.maxExpt               #-}
-- {-# COMPILE GHC expts = GHC.Float.expts #-}
{-# COMPILE GHC maxExpt10             =                                GHC.Float.maxExpt10             #-}
-- {-# COMPILE GHC expts10 = GHC.Float.expts10 #-}
{-# COMPILE GHC gtFloat               =                                GHC.Float.gtFloat               #-}
{-# COMPILE GHC geFloat               =                                GHC.Float.geFloat               #-}
{-# COMPILE GHC ltFloat               =                                GHC.Float.ltFloat               #-}
{-# COMPILE GHC leFloat               =                                GHC.Float.leFloat               #-}
{-# COMPILE GHC gtDouble              =                                GHC.Float.gtDouble              #-}
{-# COMPILE GHC geDouble              =                                GHC.Float.geDouble              #-}
{-# COMPILE GHC leDouble              =                                GHC.Float.leDouble              #-}
{-# COMPILE GHC ltDouble              =                                GHC.Float.ltDouble              #-}
{-# COMPILE GHC double2Float          =                                GHC.Float.double2Float          #-}
{-# COMPILE GHC float2Double          =                                GHC.Float.float2Double          #-}
{-# COMPILE GHC word2Double           =                                GHC.Float.word2Double           #-}
{-# COMPILE GHC word2Float            =                                GHC.Float.word2Float            #-}
{-# COMPILE GHC castWord32ToFloat     =                                GHC.Float.castWord32ToFloat     #-}
{-# COMPILE GHC stgWord32ToFloat      =                                GHC.Float.stgWord32ToFloat      #-}
{-# COMPILE GHC castFloatToWord32     =                                GHC.Float.castFloatToWord32     #-}
{-# COMPILE GHC stgFloatToWord32      =                                GHC.Float.stgFloatToWord32      #-}
{-# COMPILE GHC castWord64ToDouble    =                                GHC.Float.castWord64ToDouble    #-}
{-# COMPILE GHC stgWord64ToDouble     =                                GHC.Float.stgWord64ToDouble     #-}
{-# COMPILE GHC castDoubleToWord64    =                                GHC.Float.castDoubleToWord64    #-}
{-# COMPILE GHC stgDoubleToWord64     =                                GHC.Float.stgDoubleToWord64     #-}
{-# COMPILE GHC double2Int            =                                GHC.Float.double2Int            #-}
{-# COMPILE GHC int2Double            =                                GHC.Float.int2Double            #-}
{-# COMPILE GHC float2Int             =                                GHC.Float.float2Int             #-}
{-# COMPILE GHC int2Float             =                                GHC.Float.int2Float             #-}
{-# COMPILE GHC eqFloat               =                                GHC.Float.eqFloat               #-}
{-# COMPILE GHC eqDouble              =                                GHC.Float.eqDouble              #-}

postulate
    Data[Float]       : Data Float
    Storable[Float]   : Storable Float
    Enum[Float]       : Enum Float
    Floating[Float]   : Floating Float
    RealFloat[Float]  : RealFloat Float
    Num[Float]        : Num Float
    Read[Float]       : Read Float
    Fractional[Float] : Fractional Float
    Real[Float]       : Real Float
    RealFrac[Float]   : RealFrac Float
    Show[Float]       : Show Float
    Eq[Float]         : Eq Float
    Ord[Float]        : Ord Float

    Data[Double]       : Data Double
    Storable[Double]   : Storable Double
    Enum[Double]       : Enum Double
    Floating[Double]   : Floating Double
    RealFloat[Double]  : RealFloat Double
    Num[Double]        : Num Double
    Read[Double]       : Read Double
    Fractional[Double] : Fractional Double
    Real[Double]       : Real Double
    RealFrac[Double]   : RealFrac Double
    Show[Double]       : Show Double
    Eq[Double]         : Eq Double
    Ord[Double]        : Ord Double

{-# COMPILE GHC Data[Float]       = AgdaData       #-}
{-# COMPILE GHC Storable[Float]   = AgdaStorable   #-}
{-# COMPILE GHC Enum[Float]       = AgdaEnum       #-}
{-# COMPILE GHC Floating[Float]   = AgdaFloating   #-}
{-# COMPILE GHC RealFloat[Float]  = AgdaRealFloat  #-}
{-# COMPILE GHC Num[Float]        = AgdaNum        #-}
{-# COMPILE GHC Read[Float]       = AgdaRead       #-}
{-# COMPILE GHC Fractional[Float] = AgdaFractional #-}
{-# COMPILE GHC Real[Float]       = AgdaReal       #-}
{-# COMPILE GHC RealFrac[Float]   = AgdaRealFrac   #-}
{-# COMPILE GHC Show[Float]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Float]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Float]        = AgdaOrd        #-}

{-# COMPILE GHC Data[Double]       = AgdaData       #-}
{-# COMPILE GHC Storable[Double]   = AgdaStorable   #-}
{-# COMPILE GHC Enum[Double]       = AgdaEnum       #-}
{-# COMPILE GHC Floating[Double]   = AgdaFloating   #-}
{-# COMPILE GHC RealFloat[Double]  = AgdaRealFloat  #-}
{-# COMPILE GHC Num[Double]        = AgdaNum        #-}
{-# COMPILE GHC Read[Double]       = AgdaRead       #-}
{-# COMPILE GHC Fractional[Double] = AgdaFractional #-}
{-# COMPILE GHC Real[Double]       = AgdaReal       #-}
{-# COMPILE GHC RealFrac[Double]   = AgdaRealFrac   #-}
{-# COMPILE GHC Show[Double]       = AgdaShow       #-}
{-# COMPILE GHC Eq[Double]         = AgdaEq         #-}
{-# COMPILE GHC Ord[Double]        = AgdaOrd        #-}
