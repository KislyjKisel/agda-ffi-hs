{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Exception.Base where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Eq; Show; Exception)
open import Ffi.Hs.Data.Either using (Either)
open import Ffi.Hs.GHC.Exts    using (Addr#)

open import Ffi.Hs.GHC.Exception public
    using
    ( SomeException; Show[SomeException]; Exception[SomeException]
    ; Exception; Exception[A]⇒Typeable[A]; Exception[A]⇒Show[A]; toException; fromException; displayException
    ; ArithException; Overflow; Underflow; LossOfPrecision; DivideByZero; Denormal; RatioZeroDenominator
    ; Eq[ArithException]; Ord[ArithException]; Show[ArithException]; Exception[ArithException]
    ; ErrorCall; ErrorCallWithLocation; Eq[ErrorCall]; Ord[ErrorCall]; Show[ErrorCall]; Exception[ErrorCall]
    )

open import Ffi.Hs.GHC.IO.Exception public
    using
    ( IOException; Eq[IOException]; Show[IOException]; Exception[IOException]
    ; ArrayException; IndexOutOfBounds; UndefinedElement
    ; Eq[ArrayException]; Ord[ArrayException]; Show[ArrayException]; Exception[ArrayException]
    ; AssertionFailed; mkAssertionFailed; Show[AssertionFailed]; Exception[AssertionFailed]
    ; SomeAsyncException; Show[SomeAsyncException]; Exception[SomeAsyncException]
    ; AsyncException; StackOverflow; HeapOverflow; ThreadKilled; UserInterrupt
    ; Eq[AsyncException]; Ord[AsyncException]; Show[AsyncException]; Exception[AsyncException]
    ; asyncExceptionToException; asyncExceptionFromException
    ; BlockedIndefinitelyOnMVar; mkBlockedIndefinitelyOnMVar
    ; Show[BlockedIndefinitelyOnMVar]; Exception[BlockedIndefinitelyOnMVar]
    ; FixIOException; mkFixIOException; Show[FixIOException]; Exception[FixIOException]
    ; BlockedIndefinitelyOnSTM; mkBlockedIndefinitelyOnSTM
    ; Show[BlockedIndefinitelyOnSTM]; Exception[BlockedIndefinitelyOnSTM]
    ; AllocationLimitExceeded; mkAllocationLimitExceeded
    ; Show[AllocationLimitExceeded]; Exception[AllocationLimitExceeded]
    ; CompactionFailed; mkCompactionFailed
    ; Show[CompactionFailed]; Exception[CompactionFailed]
    ; Deadlock; mkDeadlock
    ; Show[Deadlock]; Exception[Deadlock]
    )

open import Ffi.Hs.Control.Concurrent public
    using (throwTo)

{-# FOREIGN GHC
import qualified Control.Exception.Base
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ seℓ : Level
        A B C E E₁ E₂ : Set aℓ

data NonTermination : Set where
    mkNonTermination : NonTermination

{-# COMPILE GHC NonTermination = data Control.Exception.Base.NonTermination (Control.Exception.Base.NonTermination) #-}

postulate
    Show[NonTermination]      : Show NonTermination
    Exception[NonTermination] : Exception NonTermination

{-# COMPILE GHC Show[NonTermination]      = AgdaShow      #-}
{-# COMPILE GHC Exception[NonTermination] = AgdaException #-}

data NestedAtomically : Set where
    mkNestedAtomically : NestedAtomically

{-# COMPILE GHC NestedAtomically = data Control.Exception.Base.NestedAtomically (Control.Exception.Base.NestedAtomically) #-}

postulate
    Show[NestedAtomically]      : Show NestedAtomically
    Exception[NestedAtomically] : Exception NestedAtomically

{-# COMPILE GHC Show[NestedAtomically]      = AgdaShow      #-}
{-# COMPILE GHC Exception[NestedAtomically] = AgdaException #-}

data NoMethodError : Set where
    mkNoMethodError : NoMethodError

{-# COMPILE GHC NoMethodError = data Control.Exception.Base.NoMethodError (Control.Exception.Base.NoMethodError) #-}

postulate
    Show[NoMethodError]      : Show NoMethodError
    Exception[NoMethodError] : Exception NoMethodError

{-# COMPILE GHC Show[NoMethodError]      = AgdaShow      #-}
{-# COMPILE GHC Exception[NoMethodError] = AgdaException #-}

data PatternMatchFail : Set where
    mkPatternMatchFail : PatternMatchFail

{-# COMPILE GHC PatternMatchFail = data Control.Exception.Base.PatternMatchFail (Control.Exception.Base.PatternMatchFail) #-}

postulate
    Show[PatternMatchFail]      : Show PatternMatchFail
    Exception[PatternMatchFail] : Exception PatternMatchFail

{-# COMPILE GHC Show[PatternMatchFail]      = AgdaShow      #-}
{-# COMPILE GHC Exception[PatternMatchFail] = AgdaException #-}

data RecConError : Set where
    mkRecConError : RecConError

{-# COMPILE GHC RecConError = data Control.Exception.Base.RecConError (Control.Exception.Base.RecConError) #-}

postulate
    Show[RecConError]      : Show RecConError
    Exception[RecConError] : Exception RecConError

{-# COMPILE GHC Show[RecConError]      = AgdaShow      #-}
{-# COMPILE GHC Exception[RecConError] = AgdaException #-}

data RecSelError : Set where
    mkRecSelError : RecSelError

{-# COMPILE GHC RecSelError = data Control.Exception.Base.RecSelError (Control.Exception.Base.RecSelError) #-}

postulate
    Show[RecSelError]      : Show RecSelError
    Exception[RecSelError] : Exception RecSelError

{-# COMPILE GHC Show[RecSelError]      = AgdaShow      #-}
{-# COMPILE GHC Exception[RecSelError] = AgdaException #-}

data RecUpdError : Set where
    mkRecUpdError : RecUpdError

{-# COMPILE GHC RecUpdError = data Control.Exception.Base.RecUpdError (Control.Exception.Base.RecUpdError) #-}

postulate
    Show[RecUpdError]      : Show RecUpdError
    Exception[RecUpdError] : Exception RecUpdError

{-# COMPILE GHC Show[RecUpdError]      = AgdaShow      #-}
{-# COMPILE GHC Exception[RecUpdError] = AgdaException #-}

data TypeError : Set where
    mkTypeError : TypeError

{-# COMPILE GHC TypeError = data Control.Exception.Base.TypeError (Control.Exception.Base.TypeError) #-}

postulate
    Show[TypeError]      : Show TypeError
    Exception[TypeError] : Exception TypeError

{-# COMPILE GHC Show[TypeError]      = AgdaShow      #-}
{-# COMPILE GHC Exception[TypeError] = AgdaException #-}

data MaskingState : Set where
    Unmasked              : MaskingState
    MaskedInterruptible   : MaskingState
    MaskedUninterruptible : MaskingState

{-# COMPILE GHC MaskingState = data Control.Exception.Base.MaskingState
    ( Control.Exception.Base.Unmasked
    | Control.Exception.Base.MaskedInterruptible
    | Control.Exception.Base.MaskedUninterruptible
    ) #-}

postulate
    Eq[MaskingState]   : Eq MaskingState
    Show[MaskingState] : Show MaskingState

    getMaskingState : IO MaskingState

{-# COMPILE GHC Eq[MaskingState]   = AgdaEq   #-}
{-# COMPILE GHC Show[MaskingState] = AgdaShow #-}

{-# COMPILE GHC getMaskingState = Control.Exception.Base.getMaskingState #-}

postulate
    throwIO              : ⦃ Exception E ⦄ → E → IO A
    catch                : ⦃ Exception E ⦄ → IO A → (E → IO A) → IO A
    catchJust            : ⦃ Exception E ⦄ → (E → Maybe B) → IO A → (B → IO A) → IO A
    handle               : ⦃ Exception E ⦄ → (E → IO A) → IO A → IO A
    handleJust           : ⦃ Exception E ⦄ → (E → Maybe B) → (B → IO A) → IO A → IO A
    try                  : ⦃ Exception E ⦄ → IO A → IO (Either E A)
    tryJust              : ⦃ Exception E ⦄ → (E → Maybe B) → IO A → IO (Either B A)
    onException          : IO A → IO B → IO A
    evaluate             : A → IO A
    mapException         : ⦃ Exception E₁ ⦄ → ⦃ Exception E₂ ⦄ → (E₁ → E₂) → A → A
    mask                 : ((∀{aℓ}{A : Set aℓ} → IO A → IO A) → IO B) → IO B
    mask-                : IO A → IO A
    uninterruptibleMask  : ((∀{aℓ}{A : Set aℓ} → IO A → IO A) → IO B) → IO B
    uninterruptibleMask- : IO A → IO A
    assert               : Bool → A → A
    bracket              : IO A → (A → IO B) → (A → IO C) → IO C
    bracket-             : IO A → IO B → IO C → IO C
    bracketOnError       : IO A → (A → IO B) → (A → IO C) → IO C
    finally              : IO A → IO B → IO A

    recSelError              : Addr# → A
    recConError              : Addr# → A
    runtimeError             : Addr# → A
    nonExhaustiveGuardsError : Addr# → A
    patError                 : Addr# → A
    noMethodBindingError     : Addr# → A
    absentError              : Addr# → A
    absentSumFieldError      : A
    typeError                : Addr# → A
    nonTermination           : SomeException {seℓ}
    nestedAtomically         : SomeException {seℓ}

{-# COMPILE GHC throwIO              = \ eℓ e aℓ a AgdaException                        -> Control.Exception.Base.throwIO                       #-}
{-# COMPILE GHC catch                = \ eℓ e aℓ a AgdaException                        -> Control.Exception.Base.catch                         #-}
{-# COMPILE GHC catchJust            = \ eℓ e aℓ a bℓ b AgdaException                   -> Control.Exception.Base.catchJust                     #-}
{-# COMPILE GHC handle               = \ eℓ e aℓ a AgdaException                        -> Control.Exception.Base.handle                        #-}
{-# COMPILE GHC handleJust           = \ eℓ e aℓ a bℓ b AgdaException                   -> Control.Exception.Base.handleJust                    #-}
{-# COMPILE GHC try                  = \ eℓ e aℓ a AgdaException                        -> Control.Exception.Base.try                           #-}
{-# COMPILE GHC tryJust              = \ eℓ e aℓ a bℓ b AgdaException                   -> Control.Exception.Base.tryJust                       #-}
{-# COMPILE GHC onException          = \ aℓ a bℓ b                                      -> Control.Exception.Base.onException                   #-}
{-# COMPILE GHC evaluate             = \ aℓ a                                           -> Control.Exception.Base.evaluate                      #-}
{-# COMPILE GHC mapException         = \ e1ℓ e1 e2ℓ e2 aℓ a AgdaException AgdaException -> Control.Exception.Base.mapException                  #-}
{-# COMPILE GHC mask                 = \ bℓ b f                                         -> Control.Exception.Base.mask (f () ())                #-}
{-# COMPILE GHC mask-                = \ aℓ a                                           -> Control.Exception.Base.mask_                         #-}
{-# COMPILE GHC uninterruptibleMask  = \ bℓ b f                                         -> Control.Exception.Base.uninterruptibleMask (f () ()) #-}
{-# COMPILE GHC uninterruptibleMask- = \ aℓ a                                           -> Control.Exception.Base.uninterruptibleMask_          #-}
{-# COMPILE GHC assert               = \ aℓ a                                           -> Control.Exception.Base.assert                        #-}
{-# COMPILE GHC bracket              = \ aℓ a bℓ b cℓ c                                 -> Control.Exception.Base.bracket                       #-}
{-# COMPILE GHC bracket-             = \ aℓ a bℓ b cℓ c                                 -> Control.Exception.Base.bracket_                      #-}
{-# COMPILE GHC bracketOnError       = \ aℓ a bℓ b cℓ c                                 -> Control.Exception.Base.bracketOnError                #-}
{-# COMPILE GHC finally              = \ aℓ a bℓ b                                      -> Control.Exception.Base.finally                       #-}

{-# COMPILE GHC recSelError              = \ aℓ a -> Control.Exception.Base.recSelError              #-}
{-# COMPILE GHC recConError              = \ aℓ a -> Control.Exception.Base.recConError              #-}
{-# COMPILE GHC runtimeError             = \ aℓ a -> Control.Exception.Base.runtimeError             #-}
{-# COMPILE GHC nonExhaustiveGuardsError = \ aℓ a -> Control.Exception.Base.nonExhaustiveGuardsError #-}
{-# COMPILE GHC patError                 = \ aℓ a -> Control.Exception.Base.patError                 #-}
{-# COMPILE GHC noMethodBindingError     = \ aℓ a -> Control.Exception.Base.noMethodBindingError     #-}
{-# COMPILE GHC absentError              = \ aℓ a -> Control.Exception.Base.absentError              #-}
{-# COMPILE GHC absentSumFieldError      = \ aℓ a -> Control.Exception.Base.absentSumFieldError      #-}
{-# COMPILE GHC typeError                = \ aℓ a -> Control.Exception.Base.typeError                #-}
{-# COMPILE GHC nonTermination           = \ seℓ  -> Control.Exception.Base.nonTermination           #-}
{-# COMPILE GHC nestedAtomically         = \ seℓ  -> Control.Exception.Base.nestedAtomically         #-}
