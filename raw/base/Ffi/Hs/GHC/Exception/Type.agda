{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Exception.Type where

open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.List      using (List)
open import Agda.Builtin.Sigma     using (Σ)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Show; Eq; Ord)
open import Ffi.Hs.Type.Reflection using (Typeable)

open Ffi.Hs.-base.Class public
    using (Exception)

{-# FOREIGN GHC
import qualified GHC.Exception.Type
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ seℓ : Level
        A : Set aℓ

-- instance not erased, but this type can be used with a pair of cast fns?
-- data SomeException {seℓ} : Set (lsuc seℓ) where
--     mkSomeException : {A : Set seℓ} → ⦃ Exception A ⦄ → A → SomeException 

-- {-# FOREIGN GHC type AgdaSomeException seℓ = GHC.Exception.Type.SomeException #-}
-- {-# COMPILE GHC SomeException = data(1) AgdaSomeException (GHC.Exception.Type.SomeException) #-}

postulate
    SomeException : Set (lsuc seℓ)
    mkSomeException : {A : Set seℓ} → ⦃ Exception A ⦄ → A → SomeException {seℓ}
    -- todo: (sigma not compiled, issue agda/4984) (other solutions?) unSomeException : SomeException {seℓ} → Σ (Set seℓ) λ A → (Σ (Exception A) λ _ → A)

    divZeroException        : SomeException {seℓ}
    overflowException       : SomeException {seℓ}
    ratioZeroDenomException : SomeException {seℓ}
    underflowException      : SomeException {seℓ}

    toException      : ⦃ Exception A ⦄ → A → SomeException {seℓ}
    fromException    : ⦃ Exception A ⦄ → SomeException {seℓ} → Maybe A
    displayException : ⦃ Exception A ⦄ → A → List Char

    Exception[A]⇒Typeable[A] : ⦃ Exception A ⦄ → Typeable A
    Exception[A]⇒Show[A]     : ⦃ Exception A ⦄ → Show A

    Exception[SomeException] : Exception {lsuc seℓ} SomeException
    Show[SomeException]      : Show {lsuc seℓ} SomeException

{-# FOREIGN GHC type AgdaSomeException seℓ = GHC.Exception.Type.SomeException #-}
{-# COMPILE GHC SomeException = type(1) AgdaSomeException #-}

{-# COMPILE GHC mkSomeException = \ seℓ a AgdaException -> GHC.Exception.Type.SomeException #-}
-- {-# COMPILE GHC unSomeException = \ seℓ (GHC.Exception.Type.SomeException x) -> C__'44'__32 () (C__'44'__32 AgdaException x) #-}

{-# COMPILE GHC divZeroException        = \ seℓ -> GHC.Exception.Type.divZeroException        #-}
{-# COMPILE GHC overflowException       = \ seℓ -> GHC.Exception.Type.overflowException       #-}
{-# COMPILE GHC ratioZeroDenomException = \ seℓ -> GHC.Exception.Type.ratioZeroDenomException #-}
{-# COMPILE GHC underflowException      = \ seℓ -> GHC.Exception.Type.underflowException      #-}

{-# COMPILE GHC toException      = \ aℓ a seℓ AgdaException -> GHC.Exception.Type.toException      #-}
{-# COMPILE GHC fromException    = \ aℓ a seℓ AgdaException -> GHC.Exception.Type.fromException    #-}
{-# COMPILE GHC displayException = \ aℓ a     AgdaException -> GHC.Exception.Type.displayException #-}

{-# COMPILE GHC Exception[A]⇒Typeable[A] = \ aℓ a AgdaException -> AgdaTypeable #-}
{-# COMPILE GHC Exception[A]⇒Show[A]     = \ aℓ a AgdaException -> AgdaShow     #-}

{-# COMPILE GHC Exception[SomeException] = \ seℓ -> AgdaException #-}
{-# COMPILE GHC Show[SomeException]      = \ seℓ -> AgdaShow      #-}

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
