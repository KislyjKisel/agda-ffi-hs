{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Num where

open import Agda.Builtin.Int using (Int)
open import Agda.Primitive

private
    variable
        aℓ : Level
        A : Set aℓ

Integer : Set
Integer = Int

infixl 6 _+_ _-_
infixl 7 _*_

postulate
    Num : Set aℓ → Set aℓ
    _+_         : ⦃ Num A ⦄ → A → A → A
    _-_         : ⦃ Num A ⦄ → A → A → A
    _*_         : ⦃ Num A ⦄ → A → A → A
    negate      : ⦃ Num A ⦄ → A → A
    abs         : ⦃ Num A ⦄ → A → A
    signum      : ⦃ Num A ⦄ → A → A
    fromInteger : ⦃ Num A ⦄ → Integer → A
    subtract    : ⦃ Num A ⦄ → A → A → A

{-# FOREIGN GHC import qualified GHC.Num #-}

{-# FOREIGN GHC data AgdaNum aℓ a = Num a => AgdaNum #-}
{-# COMPILE GHC Num = type(0) AgdaNum #-}

{-# COMPILE GHC _+_                = \ aℓ a AgdaNum -> (GHC.Num.+)         #-}
{-# COMPILE GHC _-_                = \ aℓ a AgdaNum -> (GHC.Num.-)         #-}
{-# COMPILE GHC _*_                = \ aℓ a AgdaNum -> (GHC.Num.*)         #-}
{-# COMPILE GHC negate             = \ aℓ a AgdaNum -> GHC.Num.negate      #-}
{-# COMPILE GHC abs                = \ aℓ a AgdaNum -> GHC.Num.abs         #-}
{-# COMPILE GHC signum             = \ aℓ a AgdaNum -> GHC.Num.signum      #-}
{-# COMPILE GHC fromInteger        = \ aℓ a AgdaNum -> GHC.Num.fromInteger #-}
{-# COMPILE GHC subtract           = \ aℓ a AgdaNum -> GHC.Num.subtract    #-}
