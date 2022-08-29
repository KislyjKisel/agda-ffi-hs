{-# OPTIONS --without-K #-}

module Ffi.Hs.Numeric where

open import Agda.Builtin.Bool                   using (Bool)
open import Agda.Builtin.Char                   using (Char)
open import Agda.Builtin.Int                    using () renaming (Int to Integer)
open import Agda.Builtin.List                   using (List)
open import Agda.Builtin.Maybe                  using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Float                  using (RealFloat)
open import Ffi.Hs.-base.Num                    using (Num)
open import Ffi.Hs.-base.Real                   using (Real; RealFrac; Rational; Integral)
open import Ffi.Hs.Data.Tuple                  using (Tuple2)
open import Ffi.Hs.Data.Eq                      using (Eq)
open import Ffi.Hs.Data.Int                     using (Int)
open import Ffi.Hs.Text.ParserCombinators.ReadP using (ReadS)
open import Ffi.Hs.Text.Show                    using (ShowS)

open Ffi.Hs.-base.Real public
    using (showSigned)

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    showIntAtBase : ⦃ Integral A ⦄ → A → (Int → Char) → A → ShowS
    showInt       : ⦃ Integral A ⦄ → A → ShowS
    showBin       : ⦃ Integral A ⦄ → A → ShowS
    showHex       : ⦃ Integral A ⦄ → A → ShowS
    showOct       : ⦃ Integral A ⦄ → A → ShowS
    showEFloat    : ⦃ RealFloat A ⦄ → Maybe Int → A → ShowS
    showFFloat    : ⦃ RealFloat A ⦄ → Maybe Int → A → ShowS
    showGFloat    : ⦃ RealFloat A ⦄ → Maybe Int → A → ShowS
    showFFloatAlt : ⦃ RealFloat A ⦄ → Maybe Int → A → ShowS
    showGFloatAlt : ⦃ RealFloat A ⦄ → Maybe Int → A → ShowS
    showFloat     : ⦃ RealFloat A ⦄ → A → ShowS
    showHFloat    : ⦃ RealFloat A ⦄ → A → ShowS
    floatToDigits : ⦃ RealFloat A ⦄ → Integer → A → Tuple2 (List Int) (Int)

    readSigned : ⦃ Real A ⦄ → ReadS A → ReadS A
    readInt    : ⦃ Num A ⦄ → A → (Char → Bool) → (Char → Int) → ReadS A
    readBin    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → ReadS A
    readDec    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → ReadS A
    readOct    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → ReadS A
    readHex    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → ReadS A
    readFloat  : ⦃ RealFrac A ⦄ → ReadS A
    lexDigits  : ReadS (List Char)

    fromRat : ⦃ RealFloat A ⦄ → Rational → A

{-# FOREIGN GHC import qualified Numeric #-}
{-# COMPILE GHC showIntAtBase = \ aℓ a AgdaIntegral  -> Numeric.showIntAtBase #-}
{-# COMPILE GHC showInt       = \ aℓ a AgdaIntegral  -> Numeric.showInt       #-}
{-# COMPILE GHC showBin       = \ aℓ a AgdaIntegral  -> Numeric.showBin       #-}
{-# COMPILE GHC showHex       = \ aℓ a AgdaIntegral  -> Numeric.showHex       #-}
{-# COMPILE GHC showOct       = \ aℓ a AgdaIntegral  -> Numeric.showOct       #-}
{-# COMPILE GHC showEFloat    = \ aℓ a AgdaRealFloat -> Numeric.showEFloat    #-}
{-# COMPILE GHC showFFloat    = \ aℓ a AgdaRealFloat -> Numeric.showFFloat    #-}
{-# COMPILE GHC showGFloat    = \ aℓ a AgdaRealFloat -> Numeric.showGFloat    #-}
{-# COMPILE GHC showFFloatAlt = \ aℓ a AgdaRealFloat -> Numeric.showFFloatAlt #-}
{-# COMPILE GHC showGFloatAlt = \ aℓ a AgdaRealFloat -> Numeric.showGFloatAlt #-}
{-# COMPILE GHC showFloat     = \ aℓ a AgdaRealFloat -> Numeric.showFloat     #-}
{-# COMPILE GHC showHFloat    = \ aℓ a AgdaRealFloat -> Numeric.showHFloat    #-}
{-# COMPILE GHC floatToDigits = \ aℓ a AgdaRealFloat -> Numeric.floatToDigits #-}

{-# COMPILE GHC readSigned = \ aℓ a AgdaReal       -> Numeric.readSigned #-}
{-# COMPILE GHC readInt    = \ aℓ a AgdaNum        -> Numeric.readInt    #-}
{-# COMPILE GHC readBin    = \ aℓ a AgdaEq AgdaNum -> Numeric.readBin    #-}
{-# COMPILE GHC readDec    = \ aℓ a AgdaEq AgdaNum -> Numeric.readDec    #-}
{-# COMPILE GHC readOct    = \ aℓ a AgdaEq AgdaNum -> Numeric.readOct    #-}
{-# COMPILE GHC readHex    = \ aℓ a AgdaEq AgdaNum -> Numeric.readHex    #-}
{-# COMPILE GHC readFloat  = \ aℓ a AgdaRealFrac   -> Numeric.readFloat  #-}
{-# COMPILE GHC lexDigits  = Numeric.lexDigits                           #-}

{-# COMPILE GHC fromRat = \ aℓ a AgdaRealFloat -> Numeric.fromRat #-}
