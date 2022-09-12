{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Scientific where

postulate
    Scientific : Set

    Eq[Scientific]         : Eq Scientific
    Fractional[Scientific] : Fractional Scientific
    Data[Scientific]       : Data Scientific
    Num[Scientific]        : Num Scientific
    Ord[Scientific]        : Ord Scientific
    Read[Scientific]       : Read Scientific
    Real[Scientific]       : Real Scientific
    RealFrac[Scientific]   : RealFrac Scientific
    Show[Scientific]       : Show Scientific
    NFData[Scientific]     : NFData Scientific
    -- todo: binary -> Binary
    -- todo: hashable -> Hashable
    -- todo: template-haskell -> Lift

data FPFormat : Set where
    Exponent : FPFormat
    Fixed    : FPFormat
    Generic  : FPFormat

postulate
    Enum[FPFormat] : Enum FPFormat
    Read[FPFormat] : Read FPFormat
    Show[FPFormat] : Show FPFormat

postulate
    scientific                    : Integer → Int → Scientific
    coefficient                   : Scientific → Integer
    base10Exponent                : Scientific → Int
    isFloating                    : Scientific → Bool
    isInteger                     : Scientific → Bool
    unsafeFromRational            : Rational → Scientific
    fromRationalRepetend          : Maybe Int → Rational → Either (Tuple2 Scientific Rational) (Tuple2 Scientific (Maybe Int))
    fromRationalRepetendLimited   : Int → Rational → Either (Tuple2 Scientific Rational) (Tuple2 Scientific (Maybe Int))
    fromRationalRepetendUnlimited : Rational → Tuple2 Scientific (Maybe Int)
    toRationalRepetend            : Scientific → Int → Rational
    floatingOrInteger             : ⦃ RealFloat R ⦄ → ⦃ Integral I ⦄ → Scientific → Either R I
    toRealFloat                   : ⦃ RealFloat A ⦄ → Scientific → A
    toBoundedRealFloat            : ⦃ RealFloat A ⦄ → Scientific → Either A A
    toBoundedInteger              : ⦃ Integral I ⦄ → ⦃ Bounded I ⦄ → Scientific → Maybe I
    fromFloatDigits               : ⦃ RealFloat A ⦄ → A → Scientific
    scientificP                   : ReadP Scientific
    formatScientific              : FPFormat → Maybe Int → Scientific → List Char
    toDecimalDigits               : Scientific → Tuple2 (List Int) Int
    normalize                     : Scientific → Scientific

-- todo: compile, imports