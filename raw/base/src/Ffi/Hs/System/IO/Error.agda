{-# OPTIONS --without-K #-}

module Ffi.Hs.System.IO.Error where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.Data.Either using (Either)
open import Ffi.Hs.System.IO   using (Handle; IO; FilePath)

open import Ffi.Hs.GHC.IO.Exception public
    using
    ( IOError; userError; ioError
    ; IOErrorType; Show[IOErrorType]; Eq[IOErrorType]
    )

{-# FOREIGN GHC
import qualified System.IO.Error
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    mkIOError       : IOErrorType → List Char → Maybe Handle → Maybe FilePath → IOError
    annotateIOError : IOError → List Char → Maybe Handle → Maybe FilePath → IOError

    isAlreadyExistsError    : IOError → Bool
    isDoesNotExistError     : IOError → Bool
    isAlreadyInUseError     : IOError → Bool
    isFullError             : IOError → Bool
    isEOFError              : IOError → Bool
    isIllegalOperation      : IOError → Bool
    isPermissionError       : IOError → Bool
    isUserError             : IOError → Bool
    isResourceVanishedError : IOError → Bool

    ioeGetErrorType   : IOError → IOErrorType
    ioeGetLocation    : IOError → List Char
    ioeGetErrorString : IOError → List Char
    ioeGetHandle      : IOError → Maybe Handle
    ioeGetFileName    : IOError → Maybe FilePath
    ioeSetErrorType   : IOError → IOErrorType → IOError
    ioeSetErrorString : IOError → List Char → IOError
    ioeSetLocation    : IOError → List Char → IOError
    ioeSetHandle      : IOError → Handle → IOError
    ioeSetFileName    : IOError → FilePath → IOError

    alreadyExistsErrorType    : IOErrorType
    doesNotExistErrorType     : IOErrorType
    alreadyInUseErrorType     : IOErrorType
    fullErrorType             : IOErrorType
    eofErrorType              : IOErrorType
    illegalOperationErrorType : IOErrorType
    permissionErrorType       : IOErrorType
    userErrorType             : IOErrorType
    resourceVanishedErrorType : IOErrorType

    isAlreadyExistsErrorType    : IOErrorType → Bool
    isDoesNotExistErrorType     : IOErrorType → Bool
    isAlreadyInUseErrorType     : IOErrorType → Bool
    isFullErrorType             : IOErrorType → Bool
    isEOFErrorType              : IOErrorType → Bool
    isIllegalOperationErrorType : IOErrorType → Bool
    isPermissionErrorType       : IOErrorType → Bool
    isUserErrorType             : IOErrorType → Bool
    isResourceVanishedErrorType : IOErrorType → Bool

    catchIOError  : IO A → (IOError → IO A) → IO A
    tryIOError    : IO A → IO (Either IOError A)
    modifyIOError : (IOError → IOError) → IO A → IO A

{-# COMPILE GHC mkIOError       = System.IO.Error.mkIOError       #-}
{-# COMPILE GHC annotateIOError = System.IO.Error.annotateIOError #-}

{-# COMPILE GHC isAlreadyExistsError    = System.IO.Error.isAlreadyExistsError    #-}
{-# COMPILE GHC isDoesNotExistError     = System.IO.Error.isDoesNotExistError     #-}
{-# COMPILE GHC isAlreadyInUseError     = System.IO.Error.isAlreadyInUseError     #-}
{-# COMPILE GHC isFullError             = System.IO.Error.isFullError             #-}
{-# COMPILE GHC isEOFError              = System.IO.Error.isEOFError              #-}
{-# COMPILE GHC isIllegalOperation      = System.IO.Error.isIllegalOperation      #-}
{-# COMPILE GHC isPermissionError       = System.IO.Error.isPermissionError       #-}
{-# COMPILE GHC isUserError             = System.IO.Error.isUserError             #-}
{-# COMPILE GHC isResourceVanishedError = System.IO.Error.isResourceVanishedError #-}

{-# COMPILE GHC ioeGetErrorType   = System.IO.Error.ioeGetErrorType   #-}
{-# COMPILE GHC ioeGetLocation    = System.IO.Error.ioeGetLocation    #-}
{-# COMPILE GHC ioeGetErrorString = System.IO.Error.ioeGetErrorString #-}
{-# COMPILE GHC ioeGetHandle      = System.IO.Error.ioeGetHandle      #-}
{-# COMPILE GHC ioeGetFileName    = System.IO.Error.ioeGetFileName    #-}
{-# COMPILE GHC ioeSetErrorType   = System.IO.Error.ioeSetErrorType   #-}
{-# COMPILE GHC ioeSetErrorString = System.IO.Error.ioeSetErrorString #-}
{-# COMPILE GHC ioeSetLocation    = System.IO.Error.ioeSetLocation    #-}
{-# COMPILE GHC ioeSetHandle      = System.IO.Error.ioeSetHandle      #-}
{-# COMPILE GHC ioeSetFileName    = System.IO.Error.ioeSetFileName    #-}

{-# COMPILE GHC alreadyExistsErrorType    = System.IO.Error.alreadyExistsErrorType    #-}
{-# COMPILE GHC doesNotExistErrorType     = System.IO.Error.doesNotExistErrorType     #-}
{-# COMPILE GHC alreadyInUseErrorType     = System.IO.Error.alreadyInUseErrorType     #-}
{-# COMPILE GHC fullErrorType             = System.IO.Error.fullErrorType             #-}
{-# COMPILE GHC eofErrorType              = System.IO.Error.eofErrorType              #-}
{-# COMPILE GHC illegalOperationErrorType = System.IO.Error.illegalOperationErrorType #-}
{-# COMPILE GHC permissionErrorType       = System.IO.Error.permissionErrorType       #-}
{-# COMPILE GHC userErrorType             = System.IO.Error.userErrorType             #-}
{-# COMPILE GHC resourceVanishedErrorType = System.IO.Error.resourceVanishedErrorType #-}

{-# COMPILE GHC isAlreadyExistsErrorType    = System.IO.Error.isAlreadyExistsErrorType    #-}
{-# COMPILE GHC isDoesNotExistErrorType     = System.IO.Error.isDoesNotExistErrorType     #-}
{-# COMPILE GHC isAlreadyInUseErrorType     = System.IO.Error.isAlreadyInUseErrorType     #-}
{-# COMPILE GHC isFullErrorType             = System.IO.Error.isFullErrorType             #-}
{-# COMPILE GHC isEOFErrorType              = System.IO.Error.isEOFErrorType              #-}
{-# COMPILE GHC isIllegalOperationErrorType = System.IO.Error.isIllegalOperationErrorType #-}
{-# COMPILE GHC isPermissionErrorType       = System.IO.Error.isPermissionErrorType       #-}
{-# COMPILE GHC isUserErrorType             = System.IO.Error.isUserErrorType             #-}
{-# COMPILE GHC isResourceVanishedErrorType = System.IO.Error.isResourceVanishedErrorType #-}

{-# COMPILE GHC catchIOError  = \ aℓ a -> System.IO.Error.catchIOError  #-}
{-# COMPILE GHC tryIOError    = \ aℓ a -> System.IO.Error.tryIOError    #-}
{-# COMPILE GHC modifyIOError = \ aℓ a -> System.IO.Error.modifyIOError #-}
