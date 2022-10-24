{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GLU.Errors where

open import Agda.Builtin.Char    using (Char)
open import Agda.Builtin.List    using (List)
open import Ffi.Hs.-base.Class   using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar using (GettableStateVar)

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GLU.Errors
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}


data ErrorCategory : Set where
    ContextLost                 : ErrorCategory
    InvalidEnum                 : ErrorCategory
    InvalidValue                : ErrorCategory
    InvalidOperation            : ErrorCategory
    InvalidFramebufferOperation : ErrorCategory
    OutOfMemory                 : ErrorCategory
    StackOverflow               : ErrorCategory
    StackUnderflow              : ErrorCategory
    TableTooLarge               : ErrorCategory
    TesselatorError             : ErrorCategory
    NURBSError                  : ErrorCategory

{-# COMPILE GHC ErrorCategory = data Graphics.Rendering.OpenGL.GLU.Errors.ErrorCategory
    ( Graphics.Rendering.OpenGL.GLU.Errors.ContextLost
    | Graphics.Rendering.OpenGL.GLU.Errors.InvalidEnum
    | Graphics.Rendering.OpenGL.GLU.Errors.InvalidValue
    | Graphics.Rendering.OpenGL.GLU.Errors.InvalidOperation
    | Graphics.Rendering.OpenGL.GLU.Errors.InvalidFramebufferOperation
    | Graphics.Rendering.OpenGL.GLU.Errors.OutOfMemory
    | Graphics.Rendering.OpenGL.GLU.Errors.StackOverflow
    | Graphics.Rendering.OpenGL.GLU.Errors.StackUnderflow
    | Graphics.Rendering.OpenGL.GLU.Errors.TableTooLarge
    | Graphics.Rendering.OpenGL.GLU.Errors.TesselatorError
    | Graphics.Rendering.OpenGL.GLU.Errors.NURBSError
    ) #-}

postulate
    Eq[ErrorCategory]   : Eq ErrorCategory
    Ord[ErrorCategory]  : Ord ErrorCategory
    Show[ErrorCategory] : Show ErrorCategory

{-# COMPILE GHC Eq[ErrorCategory]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ErrorCategory]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ErrorCategory] = AgdaShow #-}


data Error : Set where
    mkError : ErrorCategory → List Char → Error

{-# COMPILE GHC Error = data Graphics.Rendering.OpenGL.GLU.Errors.Error
    ( Graphics.Rendering.OpenGL.GLU.Errors.Error
    ) #-}

postulate
    Eq[Error]   : Eq Error
    Ord[Error]  : Ord Error
    Show[Error] : Show Error

{-# COMPILE GHC Eq[Error]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Error]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Error] = AgdaShow #-}


postulate
    errors : GettableStateVar (List Error)

{-# COMPILE GHC errors = Graphics.Rendering.OpenGL.GLU.Errors.errors #-}
