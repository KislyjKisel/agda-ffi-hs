{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Real where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Int   using () renaming (Int to Integer)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Num; Ord; Real; Integral; Fractional; RealFrac; Enum; Bounded)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Tuple  using (Tuple2)
open import Ffi.Hs.Text.Show   using (Show; ShowS)

{-# FOREIGN GHC
import qualified GHC.Real
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaNum, AgdaOrd, AgdaReal, AgdaIntegral
    , AgdaFractional, AgdaRealFrac, AgdaEnum
    , AgdaBounded, AgdaShow
    )
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

data Ratio (A : Set aℓ) : Set aℓ where
    _:%_ : A → A → Ratio A

Rational : Set
Rational = Ratio Integer

data FractionalExponentBase : Set where
    Base2 Base10 : FractionalExponentBase

infixl 7 _%_ _/_
infixr 8 _^_ _^^_

postulate
    divZeroError              : A
    ratioZeroDenominatorError : A
    overflowError             : A
    underflowError            : A

    numerator   : Ratio A → A
    denominator : Ratio A → A
    
    infinity   : Rational
    notANumber : Rational

    ratioPrec  : Int
    ratioPrec1 : Int

    toRational : ⦃ Real A ⦄ → A → Rational

    quot      : ⦃ Integral A ⦄ → A → A → A
    rem       : ⦃ Integral A ⦄ → A → A → A
    div       : ⦃ Integral A ⦄ → A → A → A
    mod       : ⦃ Integral A ⦄ → A → A → A
    quotRem   : ⦃ Integral A ⦄ → A → A → Tuple2 A A
    divMod    : ⦃ Integral A ⦄ → A → A → Tuple2 A A
    toInteger : ⦃ Integral A ⦄ → A → Integer

    _%_    : ⦃ Integral A ⦄ → A → A → Ratio A
    reduce : ⦃ Integral A ⦄ → A → A → Ratio A

    _/_          : ⦃ Fractional A ⦄ → A → A → A
    recip        : ⦃ Fractional A ⦄ → A → A
    fromRational : ⦃ Fractional A ⦄ → Rational → A

    properFraction : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → Tuple2 B A
    truncate       : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → B
    round          : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → B
    ceiling        : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → B
    floor          : ⦃ RealFrac A ⦄ → ⦃ Integral B ⦄ → A → B

    numericEnumFrom       : ⦃ Fractional A ⦄ → A → List A
    numericEnumFromThen   : ⦃ Fractional A ⦄ → A → A → List A
    numericEnumFromTo     : ⦃ Ord A ⦄ → ⦃ Fractional A ⦄ → A → A → List A
    numericEnumFromThenTo : ⦃ Ord A ⦄ → ⦃ Fractional A ⦄ → A → A → A → List A

    fromIntegral : ⦃ Integral A ⦄ → ⦃ Num B ⦄ → A → B
    realToFrac   : ⦃ Real A ⦄ → ⦃ Fractional B ⦄ → A → B

    showSigned : ⦃ Real A ⦄ → (A → ShowS) → Int → A → ShowS
    even       : ⦃ Integral A ⦄ → A → Bool
    odd        : ⦃ Integral A ⦄ → A → Bool

    _^_     : ⦃ Num A ⦄ → ⦃ Integral B ⦄ → A → B → A
    _^^_    : ⦃ Fractional A ⦄ → ⦃ Integral B ⦄ → A → B → A
    _^%^_   : ⦃ Integral A ⦄ → Rational → A → Rational
    _^^%^^_ : ⦃ Integral A ⦄ → Rational → A → Rational

    gcd : ⦃ Integral A ⦄ → A → A → A
    lcm : ⦃ Integral A ⦄ → A → A → A
    
    integralEnumFrom       : ⦃ Integral A ⦄ → ⦃ Bounded A ⦄ → A → List A
    integralEnumFromThen   : ⦃ Integral A ⦄ → ⦃ Bounded A ⦄ → A → A → List A
    integralEnumFromTo     : ⦃ Integral A ⦄ → A → A → List A
    integralEnumFromThenTo : ⦃ Integral A ⦄ → A → A → A → List A

    Show[FractionalExponentBase] : Show FractionalExponentBase

    mkRationalBase2            : Rational → Integer → Rational
    mkRationalBase10           : Rational → Integer → Rational
    mkRationalWithExponentBase : Rational → Integer → FractionalExponentBase → Rational

{-# COMPILE GHC divZeroError              = \ aℓ a -> GHC.Real.divZeroError              #-}
{-# COMPILE GHC ratioZeroDenominatorError = \ aℓ a -> GHC.Real.ratioZeroDenominatorError #-}
{-# COMPILE GHC overflowError             = \ aℓ a -> GHC.Real.overflowError             #-}
{-# COMPILE GHC underflowError            = \ aℓ a -> GHC.Real.underflowError            #-}

{-# COMPILE GHC numerator   = \ aℓ a -> GHC.Real.numerator   #-}
{-# COMPILE GHC denominator = \ aℓ a -> GHC.Real.denominator #-}

{-# COMPILE GHC infinity   = GHC.Real.infinity   #-}
{-# COMPILE GHC notANumber = GHC.Real.notANumber #-}

{-# COMPILE GHC ratioPrec  = GHC.Real.ratioPrec  #-}
{-# COMPILE GHC ratioPrec1 = GHC.Real.ratioPrec1 #-}

{-# COMPILE GHC toRational = \ aℓ a AgdaReal -> GHC.Real.toRational #-}

{-# COMPILE GHC quot      = \ aℓ a AgdaIntegral -> GHC.Real.quot      #-}
{-# COMPILE GHC rem       = \ aℓ a AgdaIntegral -> GHC.Real.rem       #-}
{-# COMPILE GHC div       = \ aℓ a AgdaIntegral -> GHC.Real.div       #-}
{-# COMPILE GHC mod       = \ aℓ a AgdaIntegral -> GHC.Real.mod       #-}
{-# COMPILE GHC quotRem   = \ aℓ a AgdaIntegral -> GHC.Real.quotRem   #-}
{-# COMPILE GHC divMod    = \ aℓ a AgdaIntegral -> GHC.Real.divMod    #-}
{-# COMPILE GHC toInteger = \ aℓ a AgdaIntegral -> GHC.Real.toInteger #-}

{-# COMPILE GHC _%_    = \ aℓ a AgdaIntegral -> (GHC.Real.%)    #-}
{-# COMPILE GHC reduce = \ aℓ a AgdaIntegral -> GHC.Real.reduce #-}

{-# COMPILE GHC _/_          = \ aℓ a AgdaFractional -> (GHC.Real./)          #-}
{-# COMPILE GHC recip        = \ aℓ a AgdaFractional -> GHC.Real.recip        #-}
{-# COMPILE GHC fromRational = \ aℓ a AgdaFractional -> GHC.Real.fromRational #-}

{-# COMPILE GHC properFraction = \ aℓ a bℓ b AgdaReal AgdaIntegral -> GHC.Real.properFraction #-}
{-# COMPILE GHC truncate       = \ aℓ a bℓ b AgdaReal AgdaIntegral -> GHC.Real.truncate       #-}
{-# COMPILE GHC round          = \ aℓ a bℓ b AgdaReal AgdaIntegral -> GHC.Real.round          #-}
{-# COMPILE GHC ceiling        = \ aℓ a bℓ b AgdaReal AgdaIntegral -> GHC.Real.ceiling        #-}
{-# COMPILE GHC floor          = \ aℓ a bℓ b AgdaReal AgdaIntegral -> GHC.Real.floor          #-}

{-# COMPILE GHC numericEnumFrom       = \ aℓ a AgdaFractional         -> GHC.Real.numericEnumFrom       #-}
{-# COMPILE GHC numericEnumFromThen   = \ aℓ a AgdaFractional         -> GHC.Real.numericEnumFromThen   #-}
{-# COMPILE GHC numericEnumFromTo     = \ aℓ a AgdaOrd AgdaFractional -> GHC.Real.numericEnumFromTo     #-}
{-# COMPILE GHC numericEnumFromThenTo = \ aℓ a AgdaOrd AgdaFractional -> GHC.Real.numericEnumFromThenTo #-}

{-# COMPILE GHC fromIntegral = \ aℓ a bℓ b AgdaIntegral AgdaNum    -> GHC.Real.fromIntegral #-}
{-# COMPILE GHC realToFrac   = \ aℓ a bℓ b AgdaReal AgdaFractional -> GHC.Real.realToFrac #-}

{-# COMPILE GHC showSigned = \ aℓ a AgdaReal     -> GHC.Real.showSigned #-}
{-# COMPILE GHC even       = \ aℓ a AgdaIntegral -> GHC.Real.showSigned #-}
{-# COMPILE GHC odd        = \ aℓ a AgdaIntegral -> GHC.Real.showSigned #-}

{-# COMPILE GHC _^_     = \ aℓ a bℓ b AgdaNum AgdaIntegral        -> (GHC.Real.^)     #-}
{-# COMPILE GHC _^^_    = \ aℓ a bℓ b AgdaFractional AgdaIntegral -> (GHC.Real.^)     #-}
{-# COMPILE GHC _^%^_   = \ aℓ a      AgdaIntegral                -> (GHC.Real.^%^)   #-}
{-# COMPILE GHC _^^%^^_ = \ aℓ a      AgdaIntegral                -> (GHC.Real.^^%^^) #-}

{-# COMPILE GHC gcd = \ aℓ a AgdaIntegral -> GHC.Real.gcd #-}
{-# COMPILE GHC lcm = \ aℓ a AgdaIntegral -> GHC.Real.lcm #-}

{-# COMPILE GHC integralEnumFrom       = \ aℓ a AgdaIntegral AgdaBounded -> GHC.Real.integralEnumFrom       #-}
{-# COMPILE GHC integralEnumFromThen   = \ aℓ a AgdaIntegral AgdaBounded -> GHC.Real.integralEnumFromThen   #-}
{-# COMPILE GHC integralEnumFromTo     = \ aℓ a AgdaIntegral             -> GHC.Real.integralEnumFromTo     #-}
{-# COMPILE GHC integralEnumFromThenTo = \ aℓ a AgdaIntegral             -> GHC.Real.integralEnumFromThenTo #-}

{-# COMPILE GHC Show[FractionalExponentBase] = AgdaShow #-}

{-# COMPILE GHC mkRationalBase2            = GHC.Real.mkRationalBase2            #-}
{-# COMPILE GHC mkRationalBase10           = GHC.Real.mkRationalBase10           #-}
{-# COMPILE GHC mkRationalWithExponentBase = GHC.Real.mkRationalWithExponentBase #-}
