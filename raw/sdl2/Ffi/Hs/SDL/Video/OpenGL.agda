{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Video.OpenGL where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit             using (⊤)
open import Ffi.Hs.Control.Monad.IO.Class using (MonadIO)
open import Ffi.Hs.Data.StateVar          using (StateVar)
open import Ffi.Hs.Foreign.C.String       using (CString)
open import Ffi.Hs.Foreign.C.Types        using (CInt)
open import Ffi.Hs.Foreign.Ptr            using (Ptr)
open import Ffi.Hs.SDL.Internal.Types     using (Window)
open import Ffi.Hs.SDL.Vect               using (V2; V4)

-- todo: compile, check

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Video.OpenGL
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ

data Mode : Set where
    Normal : Mode
    Debug  : Mode

data Profile : Set where
    Core          : Mode → CInt → CInt → Profile
    Compatibility : Mode → CInt → CInt → Profile
    ES            : Mode → CInt → CInt → Profile

postulate
    Eq[Profile]   : Eq Profile
    Ord[Profile]  : Ord Profile
    Read[Profile] : Read Profile
    Show[Profile] : Show Profile

record OpenGLConfig : Set where
    constructor mkOpenGLConfig
    field
        glColorPrecision     : V4 CInt
        glDepthPrecision     : CInt
        glStencilPrecision   : CInt
        glMultisampleSamples : CInt
        glProfile            : Profile

postulate
    Eq[OpenGLConfig]   : Eq OpenGLConfig
    Ord[OpenGLConfig]  : Ord OpenGLConfig
    Read[OpenGLConfig] : Read OpenGLConfig
    Show[OpenGLConfig] : Show OpenGLConfig

postulate
    defaultOpenGL : OpenGLConfig

    glGetDrawableSize : ⦃ MonadIO M ⦄ → Window → M (V2 CInt)
    glGetProcAddress  : ⦃ MonadIO M ⦄ → CString → M (Ptr ⊤)

postulate
    GLContext : Set
    glCreateContext : ⦃ MonadIO M ⦄ → Window → M GLContext
    glMakeCurrent   : ⦃ MonadIO M ⦄ → Window → GLContext → M ⊤
    glDeleteContext : ⦃ MonadIO M ⦄ → GLContext → M ⊤

data SwapInterval : Set where
    ImmediateUpdates    : SwapInterval
    SynchronizedUpdates : SwapInterval
    LateSwapTearing     : SwapInterval

postulate
    Bounded[SwapInterval] : Bounded SwapInterval
    Enum[SwapInterval]    : Enum SwapInterval
    Eq[SwapInterval]      : Eq SwapInterval
    Data[SwapInterval]    : Data SwapInterval
    Ord[SwapInterval]     : Ord SwapInterval
    Read[SwapInterval]    : Read SwapInterval
    Show[SwapInterval]    : Show SwapInterval

postulate
    glSwapWindow : ⦃ MonadIO M ⦄ → Window → M ⊤
    swapInterval : StateVar SwapInterval
