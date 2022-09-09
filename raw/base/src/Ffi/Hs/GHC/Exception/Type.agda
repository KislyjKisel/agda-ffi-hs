{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Exception.Type where

open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.List      using (List)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Show; Eq; Ord)
open import Ffi.Hs.Type.Reflection using (Typeable)

open Ffi.Hs.-base.Class public
    using (Exception)

{-# FOREIGN GHC
import qualified GHC.Exception.Type
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    (AgdaShow, AgdaEq, AgdaOrd, AgdaException)
import MAlonzo.Code.Ffi.Hs.Type.Reflection
    (AgdaTypeable)
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    SomeException : Set -- todo: 0ℓ but contains any-level exceptions
    divZeroException        : SomeException
    overflowException       : SomeException
    ratioZeroDenomException : SomeException
    underflowException      : SomeException

    toException      : ⦃ Exception A ⦄ → A → SomeException
    fromException    : ⦃ Exception A ⦄ → SomeException → Maybe A
    displayException : ⦃ Exception A ⦄ → A → List Char

    Exception[A]⇒Typeable[A] : ⦃ Exception A ⦄ → Typeable A
    Exception[A]⇒Show[A]     : ⦃ Exception A ⦄ → Show A

    Exception[SomeException] : Exception SomeException
    Show[SomeException]      : Show SomeException

{-# COMPILE GHC SomeException = type GHC.Exception.Type.SomeException #-}
{-# COMPILE GHC divZeroException        = GHC.Exception.Type.divZeroException        #-}
{-# COMPILE GHC overflowException       = GHC.Exception.Type.overflowException       #-}
{-# COMPILE GHC ratioZeroDenomException = GHC.Exception.Type.ratioZeroDenomException #-}
{-# COMPILE GHC underflowException      = GHC.Exception.Type.underflowException      #-}

{-# COMPILE GHC toException      = \ aℓ a AgdaException -> GHC.Exception.Type.toException      #-}
{-# COMPILE GHC fromException    = \ aℓ a AgdaException -> GHC.Exception.Type.                 #-}
{-# COMPILE GHC displayException = \ aℓ a AgdaException -> GHC.Exception.Type.displayException #-}

{-# COMPILE GHC Exception[A]⇒Typeable[A] = \ aℓ a AgdaException -> AgdaTypeable #-}
{-# COMPILE GHC Exception[A]⇒Show[A]     = \ aℓ a AgdaException -> AgdaShow     #-}

{-# COMPILE GHC Exception[SomeException] = AgdaException #-}
{-# COMPILE GHC Show[SomeException]      = AgdaShow      #-}

mkSomeException : ⦃ Exception A ⦄ → A → SomeException
mkSomeException ⦃ _ ⦄ = toException

data ArithException : Set where
    Overflow             : ArithException
    Underflow            : ArithException
    LossOfPrecision      : ArithException
    DivideByZero         : ArithException
    Denormal             : ArithException
    RatioZeroDenominator : ArithException

{-# COMPILE GHC ArithException = data GHC.Exception.Type.ArithException
    ( GHC.Exception.Type.Overflow
    | GHC.Exception.Type.Underflow
    | GHC.Exception.Type.LossOfPrecision
    | GHC.Exception.Type.DivideByZero
    | GHC.Exception.Type.Denormal
    | GHC.Exception.Type.RatioZeroDenominator
    ) #-}

postulate
    Exception[ArithException] : Exception ArithException
    Show[ArithException]      : Show ArithException
    Eq[ArithException]        : Eq ArithException
    Ord[ArithException]       : Ord ArithException

{-# COMPILE GHC Exception[ArithException] = AgdaException #-}
{-# COMPILE GHC Show[ArithException]      = AgdaShow      #-}
{-# COMPILE GHC Eq[ArithException]        = AgdaEq        #-}
{-# COMPILE GHC Ord[ArithException]       = AgdaOrd       #-}
