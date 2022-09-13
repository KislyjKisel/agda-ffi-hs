{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Bits where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Int    using (Int)

open Ffi.Hs.-base.Class public
    using (Bits; FiniteBits)

{-# FOREIGN GHC
import qualified Data.Bits
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

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

{-# COMPILE GHC _∙&∙_         = \ aℓ a AgdaBits -> (Data.Bits..&.)         #-}
{-# COMPILE GHC _∙|∙_         = \ aℓ a AgdaBits -> (Data.Bits..|.)         #-}
{-# COMPILE GHC xor           = \ aℓ a AgdaBits -> Data.Bits.xor           #-}
{-# COMPILE GHC complement    = \ aℓ a AgdaBits -> Data.Bits.complement    #-}
{-# COMPILE GHC shift         = \ aℓ a AgdaBits -> Data.Bits.shift         #-}
{-# COMPILE GHC rotate        = \ aℓ a AgdaBits -> Data.Bits.rotate        #-}
{-# COMPILE GHC zeroBits      = \ aℓ a AgdaBits -> Data.Bits.zeroBits      #-}
{-# COMPILE GHC bit           = \ aℓ a AgdaBits -> Data.Bits.bit           #-}
{-# COMPILE GHC setBit        = \ aℓ a AgdaBits -> Data.Bits.setBit        #-}
{-# COMPILE GHC clearBit      = \ aℓ a AgdaBits -> Data.Bits.clearBit      #-}
{-# COMPILE GHC complementBit = \ aℓ a AgdaBits -> Data.Bits.complementBit #-}
{-# COMPILE GHC testBit       = \ aℓ a AgdaBits -> Data.Bits.testBit       #-}
{-# COMPILE GHC bitSizeMaybe  = \ aℓ a AgdaBits -> Data.Bits.bitSizeMaybe  #-}
{-# COMPILE GHC isSigned      = \ aℓ a AgdaBits -> Data.Bits.isSigned      #-}
{-# COMPILE GHC shiftL        = \ aℓ a AgdaBits -> Data.Bits.shiftL        #-}
{-# COMPILE GHC unsafeShiftL  = \ aℓ a AgdaBits -> Data.Bits.unsafeShiftL  #-}
{-# COMPILE GHC shiftR        = \ aℓ a AgdaBits -> Data.Bits.shiftR        #-}
{-# COMPILE GHC unsafeShiftR  = \ aℓ a AgdaBits -> Data.Bits.unsafeShiftR  #-}
{-# COMPILE GHC rotateL       = \ aℓ a AgdaBits -> Data.Bits.rotateL       #-}
{-# COMPILE GHC rotateR       = \ aℓ a AgdaBits -> Data.Bits.rotateR       #-}
{-# COMPILE GHC popCount      = \ aℓ a AgdaBits -> Data.Bits.popCount      #-}

{-# COMPILE GHC finiteBitSize = \ aℓ a AgdaFiniteBits -> Data.Bits.finiteBitSize #-}
{-# COMPILE GHC countLeadingZeros = \ aℓ a AgdaFiniteBits -> Data.Bits.countLeadingZeros #-}
{-# COMPILE GHC countTrailingZeros = \ aℓ a AgdaFiniteBits -> Data.Bits.countTrailingZeros #-}

{-# COMPILE GHC bitDefault      = \ aℓ a AgdaBits AgdaNum -> Data.Bits.bitDefault      #-}
{-# COMPILE GHC testBitDefault  = \ aℓ a AgdaBits AgdaNum -> Data.Bits.testBitDefault  #-}
{-# COMPILE GHC popCountDefault = \ aℓ a AgdaBits AgdaNum -> Data.Bits.popCountDefault #-}
{-# COMPILE GHC oneBits         = \ aℓ a AgdaFiniteBits   -> Data.Bits.oneBits         #-}
{-# COMPILE GHC toIntegralSized =
    \ aℓ a bℓ b AgdaIntegral AgdaIntegral AgdaBits AgdaBits -> Data.Bits.toIntegralSized
#-}

{-# COMPILE GHC _∙^∙_  = \ aℓ a AgdaBits -> (Data.Bits..^.) #-}
{-# COMPILE GHC _∙>>∙_ = \ aℓ a AgdaBits -> (Data.Bits..>>.) #-}
{-# COMPILE GHC _∙<<∙_ = \ aℓ a AgdaBits -> (Data.Bits..<<.) #-}
{-# COMPILE GHC _!>>∙_ = \ aℓ a AgdaBits -> (Data.Bits.!>>.) #-}
{-# COMPILE GHC _!<<∙_ = \ aℓ a AgdaBits -> (Data.Bits.!<<.) #-}

postulate
    And : Set aℓ → Set aℓ
    mkAnd : A → And A
    getAnd : And A → A
    
    Ior : Set aℓ → Set aℓ
    mkIor : A → Ior A
    getIor : Ior A → A

    Xor : Set aℓ → Set aℓ
    mkXor : A → Xor A
    getXor : Xor A → A

    Iff : Set aℓ → Set aℓ
    mkIff : A → Iff A
    getIff : Iff A → A

{-# FOREIGN GHC type AgdaAnd aℓ = Data.Bits.And #-}
{-# COMPILE GHC And = type(1) AgdaAnd #-}
{-# COMPILE GHC mkAnd  = \ aℓ a -> Data.Bits.mkAnd  #-}
{-# COMPILE GHC getAnd = \ aℓ a -> Data.Bits.getAnd #-}
    
{-# FOREIGN GHC type AgdaIor aℓ = Data.Bits.Ior #-}
{-# COMPILE GHC Ior = type(1) AgdaIor #-}
{-# COMPILE GHC mkIor  = \ aℓ a -> Data.Bits.mkIor  #-}
{-# COMPILE GHC getIor = \ aℓ a -> Data.Bits.getIor #-}

{-# FOREIGN GHC type AgdaXor aℓ = Data.Bits.Xor #-}
{-# COMPILE GHC Xor = type(1) AgdaXor #-}
{-# COMPILE GHC mkXor  = \ aℓ a -> Data.Bits.mkXor  #-}
{-# COMPILE GHC getXor = \ aℓ a -> Data.Bits.getXor #-}

{-# FOREIGN GHC type AgdaIff aℓ = Data.Bits.Iff #-}
{-# COMPILE GHC Iff = type(1) AgdaIff #-}
{-# COMPILE GHC mkIff  = \ aℓ a -> Data.Bits.mkIff  #-}
{-# COMPILE GHC getIff = \ aℓ a -> Data.Bits.getIff #-}

postulate
    Bits[A]⇒Eq[A]         : ⦃ Bits A ⦄ → Eq A
    FiniteBits[A]⇒Bits[A] : ⦃ FiniteBits A ⦄ → Bits A

    Monoid[And[A]]     : ⦃ FiniteBits A ⦄ → Monoid (And A)
    Semigroup[And[A]]  : ⦃ Bits A ⦄ → Semigroup (And A)
    Bits[And[A]]       : ⦃ Bits A ⦄ → Bits (And A)
    FiniteBits[And[A]] : ⦃ FiniteBits A ⦄ → FiniteBits (And A)
    Bounded[And[A]]    : ⦃ Bounded A ⦄ → Bounded (And A)
    Enum[And[A]]       : ⦃ Enum A ⦄ → Enum (And A)
    Read[And[A]]       : ⦃ Read A ⦄ → Read (And A)
    Show[And[A]]       : ⦃ Show A ⦄ → Show (And A)
    Eq[And[A]]         : ⦃ Eq A ⦄ → Eq (And A)

    Monoid[Ior[A]]     : ⦃ Bits A ⦄ → Monoid (Ior A)
    Semigroup[Ior[A]]  : ⦃ Bits A ⦄ → Semigroup (Ior A)
    Bits[Ior[A]]       : ⦃ Bits A ⦄ → Bits (Ior A)
    FiniteBits[Ior[A]] : ⦃ FiniteBits A ⦄ → FiniteBits (Ior A)
    Bounded[Ior[A]]    : ⦃ Bounded A ⦄ → Bounded (Ior A)
    Enum[Ior[A]]       : ⦃ Enum A ⦄ → Enum (Ior A)
    Read[Ior[A]]       : ⦃ Read A ⦄ → Read (Ior A)
    Show[Ior[A]]       : ⦃ Show A ⦄ → Show (Ior A)
    Eq[Ior[A]]         : ⦃ Eq A ⦄ → Eq (Ior A)

    Monoid[Xor[A]]     : ⦃ Bits A ⦄ → Monoid (Xor A)
    Semigroup[Xor[A]]  : ⦃ Bits A ⦄ → Semigroup (Xor A)
    Bits[Xor[A]]       : ⦃ Bits A ⦄ → Bits (Xor A)
    FiniteBits[Xor[A]] : ⦃ FiniteBits A ⦄ → FiniteBits (Xor A)
    Bounded[Xor[A]]    : ⦃ Bounded A ⦄ → Bounded (Xor A)
    Enum[Xor[A]]       : ⦃ Enum A ⦄ → Enum (Xor A)
    Read[Xor[A]]       : ⦃ Read A ⦄ → Read (Xor A)
    Show[Xor[A]]       : ⦃ Show A ⦄ → Show (Xor A)
    Eq[Xor[A]]         : ⦃ Eq A ⦄ → Eq (Xor A)

    Monoid[Iff[A]]     : ⦃ FiniteBits A ⦄ → Monoid (Iff A)
    Semigroup[Iff[A]]  : ⦃ FiniteBits A ⦄ → Semigroup (Iff A)
    Bits[Iff[A]]       : ⦃ Bits A ⦄ → Bits (Iff A)
    FiniteBits[Iff[A]] : ⦃ FiniteBits A ⦄ → FiniteBits (Iff A)
    Bounded[Iff[A]]    : ⦃ Bounded A ⦄ → Bounded (Iff A)
    Enum[Iff[A]]       : ⦃ Enum A ⦄ → Enum (Iff A)
    Read[Iff[A]]       : ⦃ Read A ⦄ → Read (Iff A)
    Show[Iff[A]]       : ⦃ Show A ⦄ → Show (Iff A)
    Eq[Iff[A]]         : ⦃ Eq A ⦄ → Eq (Iff A)

{-# COMPILE GHC Bits[A]⇒Eq[A]         = \ aℓ a AgdaBits       -> AgdaEq   #-}
{-# COMPILE GHC FiniteBits[A]⇒Bits[A] = \ aℓ a AgdaFiniteBits -> AgdaBits #-}

{-# COMPILE GHC Monoid[And[A]]     = \ aℓ a AgdaFiniteBits -> AgdaMonoid     #-}
{-# COMPILE GHC Semigroup[And[A]]  = \ aℓ a AgdaBits       -> AgdaSemigroup  #-}
{-# COMPILE GHC Bits[And[A]]       = \ aℓ a AgdaBits       -> AgdaBits       #-}
{-# COMPILE GHC FiniteBits[And[A]] = \ aℓ a AgdaFiniteBits -> AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[And[A]]    = \ aℓ a AgdaBounded    -> AgdaBounded    #-}
{-# COMPILE GHC Enum[And[A]]       = \ aℓ a AgdaEnum       -> AgdaEnum       #-}
{-# COMPILE GHC Read[And[A]]       = \ aℓ a AgdaRead       -> AgdaRead       #-}
{-# COMPILE GHC Show[And[A]]       = \ aℓ a AgdaShow       -> AgdaShow       #-}
{-# COMPILE GHC Eq[And[A]]         = \ aℓ a AgdaEq         -> AgdaEq         #-}

{-# COMPILE GHC Monoid[Ior[A]]     = \ aℓ a AgdaBits       -> AgdaMonoid     #-}
{-# COMPILE GHC Semigroup[Ior[A]]  = \ aℓ a AgdaBits       -> AgdaSemigroup  #-}
{-# COMPILE GHC Bits[Ior[A]]       = \ aℓ a AgdaBits       -> AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Ior[A]] = \ aℓ a AgdaFiniteBits -> AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Ior[A]]    = \ aℓ a AgdaBounded    -> AgdaBounded    #-}
{-# COMPILE GHC Enum[Ior[A]]       = \ aℓ a AgdaEnum       -> AgdaEnum       #-}
{-# COMPILE GHC Read[Ior[A]]       = \ aℓ a AgdaRead       -> AgdaRead       #-}
{-# COMPILE GHC Show[Ior[A]]       = \ aℓ a AgdaShow       -> AgdaShow       #-}
{-# COMPILE GHC Eq[Ior[A]]         = \ aℓ a AgdaEq         -> AgdaEq         #-}

{-# COMPILE GHC Monoid[Xor[A]]     = \ aℓ a AgdaBits       -> AgdaMonoid     #-}
{-# COMPILE GHC Semigroup[Xor[A]]  = \ aℓ a AgdaBits       -> AgdaSemigroup  #-}
{-# COMPILE GHC Bits[Xor[A]]       = \ aℓ a AgdaBits       -> AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Xor[A]] = \ aℓ a AgdaFiniteBits -> AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Xor[A]]    = \ aℓ a AgdaBounded    -> AgdaBounded    #-}
{-# COMPILE GHC Enum[Xor[A]]       = \ aℓ a AgdaEnum       -> AgdaEnum       #-}
{-# COMPILE GHC Read[Xor[A]]       = \ aℓ a AgdaRead       -> AgdaRead       #-}
{-# COMPILE GHC Show[Xor[A]]       = \ aℓ a AgdaShow       -> AgdaShow       #-}
{-# COMPILE GHC Eq[Xor[A]]         = \ aℓ a AgdaEq         -> AgdaEq         #-}

{-# COMPILE GHC Monoid[Iff[A]]     = \ aℓ a AgdaFiniteBits -> AgdaMonoid     #-}
{-# COMPILE GHC Semigroup[Iff[A]]  = \ aℓ a AgdaFiniteBits -> AgdaSemigroup  #-}
{-# COMPILE GHC Bits[Iff[A]]       = \ aℓ a AgdaBits       -> AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Iff[A]] = \ aℓ a AgdaFiniteBits -> AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Iff[A]]    = \ aℓ a AgdaBounded    -> AgdaBounded    #-}
{-# COMPILE GHC Enum[Iff[A]]       = \ aℓ a AgdaEnum       -> AgdaEnum       #-}
{-# COMPILE GHC Read[Iff[A]]       = \ aℓ a AgdaRead       -> AgdaRead       #-}
{-# COMPILE GHC Show[Iff[A]]       = \ aℓ a AgdaShow       -> AgdaShow       #-}
{-# COMPILE GHC Eq[Iff[A]]         = \ aℓ a AgdaEq         -> AgdaEq         #-}
