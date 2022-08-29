{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Bits where

open import Agda.Builtin.Bool     using (Bool)
open import Agda.Builtin.Maybe    using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-I.Num  using (Num)
open import Ffi.Hs.-I.Real using (Integral)
open import Ffi.Hs.Data.Int       using (Int)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

infixl 8 _∙>>∙_ _∙<<∙_ _!>>∙_ _!<<∙_
infixl 7 _∙&∙_
infixl 6 _∙^∙_
infixl 5 _∙|∙_

postulate
    Bits          : Set aℓ → Set aℓ
    _∙&∙_         : ⦃ Bits A ⦄ → A → A → A
    _∙|∙_         : ⦃ Bits A ⦄ → A → A → A
    xor           : ⦃ Bits A ⦄ → A → A → A
    complement    : ⦃ Bits A ⦄ → A → A
    shift         : ⦃ Bits A ⦄ → A → Int → A
    rotate        : ⦃ Bits A ⦄ → A → Int → A
    zeroBits      : ⦃ Bits A ⦄ → A
    bit           : ⦃ Bits A ⦄ → Int → A
    setBit        : ⦃ Bits A ⦄ → A → Int → A
    clearBit      : ⦃ Bits A ⦄ → A → Int → A
    complementBit : ⦃ Bits A ⦄ → A → Int → A
    testBit       : ⦃ Bits A ⦄ → A → Int → Bool
    bitSizeMaybe  : ⦃ Bits A ⦄ → A → Maybe Int
    isSigned      : ⦃ Bits A ⦄ → A → Bool
    shiftL        : ⦃ Bits A ⦄ → A → Int → A
    unsafeShiftL  : ⦃ Bits A ⦄ → A → Int → A
    shiftR        : ⦃ Bits A ⦄ → A → Int → A
    unsafeShiftR  : ⦃ Bits A ⦄ → A → Int → A
    rotateL       : ⦃ Bits A ⦄ → A → Int → A
    rotateR       : ⦃ Bits A ⦄ → A → Int → A
    popCount      : ⦃ Bits A ⦄ → A → Int

    FiniteBits         : Set aℓ → Set aℓ
    finiteBitSize      : ⦃ FiniteBits A ⦄ → A → Int
    countLeadingZeros  : ⦃ FiniteBits A ⦄ → A → Int
    countTrailingZeros : ⦃ FiniteBits A ⦄ → A → Int

    bitDefault      : ⦃ Bits A ⦄ → ⦃ Num A ⦄ → Int → A
    testBitDefault  : ⦃ Bits A ⦄ → ⦃ Num A ⦄ → A → Int → Bool
    popCountDefault : ⦃ Bits A ⦄ → ⦃ Num A ⦄ → A → Int
    toIntegralSized : ⦃ Integral A ⦄ → ⦃ Integral B ⦄ → ⦃ Bits A ⦄ → ⦃ Bits B ⦄ → A → Maybe B
    oneBits         : ⦃ FiniteBits A ⦄ → A

    _∙^∙_  : ⦃ Bits A ⦄ → A → A → A
    _∙>>∙_ : ⦃ Bits A ⦄ → A → Int → A
    _∙<<∙_ : ⦃ Bits A ⦄ → A → Int → A
    _!>>∙_ : ⦃ Bits A ⦄ → A → Int → A
    _!<<∙_ : ⦃ Bits A ⦄ → A → Int → A

    And : Set aℓ → Set aℓ
    And_ : A → (And) A
    getAnd : (And) A → A
    
    Ior : Set aℓ → Set aℓ
    Ior_ : A → (Ior) A
    getIor : (Ior) A → A

    Xor : Set aℓ → Set aℓ
    Xor_ : A → (Xor) A
    getXor : (Xor) A → A

    Iff : Set aℓ → Set aℓ
    Iff_ : A → (Iff) A
    getIff : (Iff) A → A
