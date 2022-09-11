{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.IO.Exception where

open import Agda.Builtin.Bool         using (Bool)
open import Agda.Builtin.Char         using (Char)
open import Agda.Builtin.List         using (List)
open import Agda.Builtin.Maybe        using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class        using (Read; Show; Eq; Ord; Exception)
open import Ffi.Hs.Foreign.C.Types    using (CInt)
open import Ffi.Hs.GHC.Exception.Type using (SomeException)
open import Ffi.Hs.GHC.Exts           using (Addr#; Int)
open import Ffi.Hs.GHC.Stack          using (HasCallStack)
open import Ffi.Hs.System.IO          using (IO; Handle)

{-# FOREIGN GHC
import qualified GHC.IO.Exception
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack)
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaShow, AgdaRead, AgdaEq, AgdaOrd, AgdaException
    )
#-}

private
    variable
        aℓ seℓ : Level
        A : Set aℓ

data BlockedIndefinitelyOnMVar : Set where
    mkBlockedIndefinitelyOnMVar : BlockedIndefinitelyOnMVar

{-# COMPILE GHC BlockedIndefinitelyOnMVar
    = data GHC.IO.Exception.BlockedIndefinitelyOnMVar (GHC.IO.Exception.BlockedIndefinitelyOnMVar) #-}

postulate
    Exception[BlockedIndefinitelyOnMVar] : Exception BlockedIndefinitelyOnMVar
    Show[BlockedIndefinitelyOnMVar]      : Show BlockedIndefinitelyOnMVar
    
    blockedIndefinitelyOnMVar : SomeException {seℓ}

{-# COMPILE GHC Exception[BlockedIndefinitelyOnMVar] = AgdaException #-}
{-# COMPILE GHC Show[BlockedIndefinitelyOnMVar]      = AgdaShow      #-}

{-# COMPILE GHC blockedIndefinitelyOnMVar = \ seℓ -> GHC.IO.Exception.blockedIndefinitelyOnMVar #-}

data BlockedIndefinitelyOnSTM : Set where
    mkBlockedIndefinitelyOnSTM : BlockedIndefinitelyOnSTM

{-# COMPILE GHC BlockedIndefinitelyOnSTM
    = data GHC.IO.Exception.BlockedIndefinitelyOnSTM (GHC.IO.Exception.BlockedIndefinitelyOnSTM) #-}

postulate
    Exception[BlockedIndefinitelyOnSTM] : Exception BlockedIndefinitelyOnSTM
    Show[BlockedIndefinitelyOnSTM]      : Show BlockedIndefinitelyOnSTM
    
    blockedIndefinitelyOnSTM : SomeException {seℓ}

{-# COMPILE GHC Exception[BlockedIndefinitelyOnSTM] = AgdaException #-}
{-# COMPILE GHC Show[BlockedIndefinitelyOnSTM]      = AgdaShow      #-}

{-# COMPILE GHC blockedIndefinitelyOnSTM = \ seℓ -> GHC.IO.Exception.blockedIndefinitelyOnSTM #-}

data Deadlock : Set where
    mkDeadlock : Deadlock

{-# COMPILE GHC Deadlock
    = data GHC.IO.Exception.Deadlock (GHC.IO.Exception.Deadlock) #-}

postulate
    Exception[Deadlock] : Exception Deadlock
    Show[Deadlock]      : Show Deadlock

{-# COMPILE GHC Exception[Deadlock] = AgdaException #-}
{-# COMPILE GHC Show[Deadlock]      = AgdaShow      #-}

data AllocationLimitExceeded : Set where
    mkAllocationLimitExceeded : AllocationLimitExceeded

{-# COMPILE GHC AllocationLimitExceeded
    = data GHC.IO.Exception.AllocationLimitExceeded (GHC.IO.Exception.AllocationLimitExceeded) #-}

postulate
    Exception[AllocationLimitExceeded] : Exception AllocationLimitExceeded
    Show[AllocationLimitExceeded]      : Show AllocationLimitExceeded
    
    allocationLimitExceeded : SomeException {seℓ}

{-# COMPILE GHC Exception[AllocationLimitExceeded] = AgdaException #-}
{-# COMPILE GHC Show[AllocationLimitExceeded]      = AgdaShow      #-}

{-# COMPILE GHC allocationLimitExceeded = \ seℓ -> GHC.IO.Exception.allocationLimitExceeded #-}

data AssertionFailed : Set where
    mkAssertionFailed : List Char → AssertionFailed

{-# COMPILE GHC AssertionFailed
    = data GHC.IO.Exception.AssertionFailed (GHC.IO.Exception.AssertionFailed) #-}

postulate
    Exception[AssertionFailed] : Exception AssertionFailed
    Show[AssertionFailed] : Show AssertionFailed

{-# COMPILE GHC Exception[AssertionFailed] = AgdaException #-}
{-# COMPILE GHC Show[AssertionFailed]      = AgdaShow      #-}

data CompactionFailed : Set where
    mkCompactionFailed : List Char → CompactionFailed

{-# COMPILE GHC CompactionFailed
    = data GHC.IO.Exception.CompactionFailed (GHC.IO.Exception.CompactionFailed) #-}

postulate
    Exception[CompactionFailed] : Exception CompactionFailed
    Show[CompactionFailed]      : Show CompactionFailed

    cannotCompactFunction : SomeException {seℓ}
    cannotCompactPinned   : SomeException {seℓ}
    cannotCompactMutable  : SomeException {seℓ}

{-# COMPILE GHC Exception[CompactionFailed] = AgdaException #-}
{-# COMPILE GHC Show[CompactionFailed]      = AgdaShow      #-}

{-# COMPILE GHC cannotCompactFunction = \ seℓ -> GHC.IO.Exception.cannotCompactFunction #-}
{-# COMPILE GHC cannotCompactPinned   = \ seℓ -> GHC.IO.Exception.cannotCompactPinned   #-}
{-# COMPILE GHC cannotCompactMutable  = \ seℓ -> GHC.IO.Exception.cannotCompactMutable  #-}

data SomeAsyncException {sℓ} : Set (lsuc sℓ) where
    mkSomeAsyncException : {A : Set sℓ} → ⦃ Exception A ⦄ → A → SomeAsyncException 

{-# FOREIGN GHC type AgdaSomeAsyncException sℓ = GHC.Exception.Type.SomeAsyncException #-}
{-# COMPILE GHC SomeAsyncException = data(1) AgdaSomeAsyncException (GHC.Exception.Type.SomeAsyncException) #-}

postulate    
    Exception[SomeAsyncException] : Exception {lsuc seℓ} SomeAsyncException
    Show[SomeAsyncException]      : Show {lsuc seℓ} SomeAsyncException

    asyncExceptionToException   : ⦃ Exception A ⦄ → A → SomeException {seℓ}
    asyncExceptionFromException : ⦃ Exception A ⦄ → SomeException {seℓ} → Maybe A

{-# COMPILE GHC Exception[SomeAsyncException] = AgdaException #-}
{-# COMPILE GHC Show[SomeAsyncException]      = AgdaShow      #-}

{-# COMPILE GHC asyncExceptionToException   = \ aℓ a seℓ AgdaException -> GHC.IO.Exception.asyncExceptionToException   #-}
{-# COMPILE GHC asyncExceptionFromException = \ aℓ a seℓ AgdaException -> GHC.IO.Exception.asyncExceptionFromException #-}

data AsyncException : Set where
    StackOverflow : AsyncException
    HeapOverflow : AsyncException
    ThreadKilled : AsyncException
    UserInterrupt : AsyncException

{-# COMPILE GHC AsyncException
    = data GHC.IO.Exception.AsyncException
    ( GHC.IO.Exception.StackOverflow
    | GHC.IO.Exception.HeapOverflow
    | GHC.IO.Exception.ThreadKilled
    | GHC.IO.Exception.UserInterrupt
    ) #-}

postulate
    Exception[AsyncException] : Exception AsyncException
    Show[AsyncException]      : Show AsyncException
    Eq[AsyncException]        : Eq AsyncException
    Ord[AsyncException]       : Ord AsyncException

    stackOverflow : SomeException {seℓ}
    heapOverflow  : SomeException {seℓ}

{-# COMPILE GHC Exception[AsyncException] = AgdaException #-}
{-# COMPILE GHC Show[AsyncException]      = AgdaShow      #-}
{-# COMPILE GHC Eq[AsyncException]        = AgdaEq        #-}
{-# COMPILE GHC Ord[AsyncException]       = AgdaOrd       #-}

{-# COMPILE GHC stackOverflow = \ seℓ -> GHC.IO.Exception.stackOverflow #-}
{-# COMPILE GHC heapOverflow  = \ seℓ -> GHC.IO.Exception.heapOverflow  #-}

data ArrayException : Set where
    IndexOutOfBounds : List Char → ArrayException
    UndefinedElement : List Char → ArrayException

{-# COMPILE GHC ArrayException = data GHC.IO.Exception.ArrayException
    ( GHC.IO.Exception.IndexOutOfBounds
    | GHC.IO.Exception.UndefinedElement
    ) #-}

postulate
    Exception[ArrayException] : Exception ArrayException
    Show[ArrayException]      : Show ArrayException
    Eq[ArrayException]        : Eq ArrayException
    Ord[ArrayException]       : Ord ArrayException

{-# COMPILE GHC Exception[ArrayException] = AgdaException #-}
{-# COMPILE GHC Show[ArrayException]      = AgdaShow      #-}
{-# COMPILE GHC Eq[ArrayException]        = AgdaEq        #-}
{-# COMPILE GHC Ord[ArrayException]       = AgdaOrd       #-}

data ExitCode : Set where
    ExitSuccess : ExitCode
    ExitFailure : Int → ExitCode

{-# COMPILE GHC ExitCode = data System.Exit.ExitCode
    ( System.Exit.ExitSuccess
    | System.Exit.ExitFailure
    ) #-}

postulate
    Show[ExitCode]      : Show ExitCode
    Read[ExitCode]      : Read ExitCode
    Exception[ExitCode] : Exception ExitCode
    Eq[ExitCode]        : Eq ExitCode
    Ord[ExitCode]       : Ord ExitCode

{-# COMPILE GHC Show[ExitCode]      = AgdaShow      #-}
{-# COMPILE GHC Read[ExitCode]      = AgdaRead      #-}
{-# COMPILE GHC Exception[ExitCode] = AgdaException #-}
{-# COMPILE GHC Eq[ExitCode]        = AgdaEq        #-}
{-# COMPILE GHC Ord[ExitCode]       = AgdaOrd       #-}

data FixIOException : Set where
    mkFixIOException : FixIOException

{-# COMPILE GHC FixIOException = data GHC.IO.Exception.FixIOException
    ( GHC.IO.Exception.FixIOException
    ) #-}

postulate
    Exception[FixIOException] : Exception FixIOException
    Show[FixIOException]      : Show FixIOException

{-# COMPILE GHC Exception[FixIOException] = AgdaException #-}
{-# COMPILE GHC Show[FixIOException]      = AgdaShow      #-}

data IOErrorType : Set where
    AlreadyExists          : IOErrorType
    NoSuchThing            : IOErrorType
    ResourceBusy           : IOErrorType
    ResourceExhausted      : IOErrorType
    EOF                    : IOErrorType
    IllegalOperation       : IOErrorType
    PermissionDenied       : IOErrorType
    UserError              : IOErrorType
    UnsatisfiedConstraints : IOErrorType
    SystemError            : IOErrorType
    ProtocolError          : IOErrorType
    OtherError             : IOErrorType
    InvalidArgument        : IOErrorType
    InappropriateType      : IOErrorType
    HardwareFault          : IOErrorType
    UnsupportedOperation   : IOErrorType
    TimeExpired            : IOErrorType
    ResourceVanished       : IOErrorType
    Interrupted            : IOErrorType

{-# COMPILE GHC IOErrorType = data GHC.IO.Exception.IOErrorType
    ( AlreadyExists
    | GHC.IO.Exception.NoSuchThing
    | GHC.IO.Exception.ResourceBusy
    | GHC.IO.Exception.ResourceExhausted
    | GHC.IO.Exception.EOF
    | GHC.IO.Exception.IllegalOperation
    | GHC.IO.Exception.PermissionDenied
    | GHC.IO.Exception.UserError
    | GHC.IO.Exception.UnsatisfiedConstraints
    | GHC.IO.Exception.SystemError
    | GHC.IO.Exception.ProtocolError
    | GHC.IO.Exception.OtherError
    | GHC.IO.Exception.InvalidArgument
    | GHC.IO.Exception.InappropriateType
    | GHC.IO.Exception.HardwareFault
    | GHC.IO.Exception.UnsupportedOperation
    | GHC.IO.Exception.TimeExpired
    | GHC.IO.Exception.ResourceVanished
    | GHC.IO.Exception.Interrupted
    ) #-}

postulate
    Show[IOErrorType] : Show IOErrorType
    Eq[IOErrorType]   : Eq IOErrorType

{-# COMPILE GHC Show[IOErrorType] = AgdaException #-}
{-# COMPILE GHC Eq[IOErrorType]   = AgdaEq        #-}

record IOException : Set where
    constructor mkIOError
    field
        ioe-handle      : Maybe Handle
        ioe-type        : IOErrorType
        ioe-location    : List Char
        ioe-description : List Char
        ioe-errno       : Maybe CInt
        ioe-filename    : Maybe (List Char)

{-# COMPILE GHC IOException = data GHC.IO.Exception.IOException (GHC.IO.Exception.IOError) #-}

IOError : Set
IOError = IOException -- ? "In Haskell 2010, this is an opaque type."

postulate
    Exception[IOException] : Exception IOException
    Show[IOException]      : Show IOException
    Eq[IOException]        : Eq IOException

    ioException : IOException → IO A
    ioError     : IOError → IO A

{-# COMPILE GHC Exception[IOException] = AgdaException #-}
{-# COMPILE GHC Show[IOException]      = AgdaShow      #-}
{-# COMPILE GHC Eq[IOException]        = AgdaEq        #-}

{-# COMPILE GHC ioException = \ aℓ a -> GHC.IO.Exception.ioException #-}
{-# COMPILE GHC ioError     = \ aℓ a -> GHC.IO.Exception.ioError     #-}

postulate
    userError            : List Char → IOError
    assertError          : ⦃ HasCallStack ⦄ → Bool → A → A
    unsupportedOperation : IOError
    untangle             : Addr# → List Char → List Char

{-# COMPILE GHC userError            =                       GHC.IO.Exception.userError            #-}
{-# COMPILE GHC assertError          = \ AgdaHasCallStack -> GHC.IO.Exception.assertError          #-}
{-# COMPILE GHC unsupportedOperation =                       GHC.IO.Exception.unsupportedOperation #-}
{-# COMPILE GHC untangle             =                       GHC.IO.Exception.untangle             #-}
