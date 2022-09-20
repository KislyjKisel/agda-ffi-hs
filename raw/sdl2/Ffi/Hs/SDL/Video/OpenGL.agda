{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Video.OpenGL where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit             using (⊤; ⊤′)
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

{-# COMPILE GHC Mode = data SDL.Video.OpenGL.Mode
    ( SDL.Video.OpenGL.Normal
    | SDL.Video.OpenGL.Debug
    ) #-}

postulate
    Bounded[Mode] : Bounded Mode
    Enum[Mode]    : Enum Mode
    Eq[Mode]      : Eq Mode
    Data[Mode]    : Data Mode
    Ord[Mode]     : Ord Mode
    Read[Mode]    : Read Mode
    Show[Mode]    : Show Mode

{-# COMPILE GHC Bounded[Mode] = AgdaBounded #-}
{-# COMPILE GHC Enum[Mode]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[Mode]      = AgdaEq      #-}
{-# COMPILE GHC Data[Mode]    = AgdaData    #-}
{-# COMPILE GHC Ord[Mode]     = AgdaOrd     #-}
{-# COMPILE GHC Read[Mode]    = AgdaRead    #-}
{-# COMPILE GHC Show[Mode]    = AgdaShow    #-}


data Profile : Set where
    Core          : Mode → CInt → CInt → Profile
    Compatibility : Mode → CInt → CInt → Profile
    ES            : Mode → CInt → CInt → Profile

{-# COMPILE GHC Profile = data SDL.Video.OpenGL.Profile
    ( SDL.Video.OpenGL.Core
    | SDL.Video.OpenGL.Compatibility
    | SDL.Video.OpenGL.ES
    ) #-}

postulate
    Eq[Profile]   : Eq Profile
    Ord[Profile]  : Ord Profile
    Read[Profile] : Read Profile
    Show[Profile] : Show Profile

{-# COMPILE GHC Eq[Profile]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Profile]  = AgdaOrd  #-}
{-# COMPILE GHC Read[Profile] = AgdaRead #-}
{-# COMPILE GHC Show[Profile] = AgdaShow #-}


record OpenGLConfig : Set where
    constructor mkOpenGLConfig
    field
        glColorPrecision     : V4 CInt
        glDepthPrecision     : CInt
        glStencilPrecision   : CInt
        glMultisampleSamples : CInt
        glProfile            : Profile

{-# COMPILE GHC OpenGLConfig = data SDL.Video.OpenGL.OpenGLConfig (SDL.Video.OpenGL.OpenGLConfig) #-}

postulate
    Eq[OpenGLConfig]   : Eq OpenGLConfig
    Ord[OpenGLConfig]  : Ord OpenGLConfig
    Read[OpenGLConfig] : Read OpenGLConfig
    Show[OpenGLConfig] : Show OpenGLConfig

{-# COMPILE GHC Eq[OpenGLConfig]   = AgdaEq   #-}
{-# COMPILE GHC Ord[OpenGLConfig]  = AgdaOrd  #-}
{-# COMPILE GHC Read[OpenGLConfig] = AgdaRead #-}
{-# COMPILE GHC Show[OpenGLConfig] = AgdaShow #-}


postulate
    defaultOpenGL : OpenGLConfig

    glGetDrawableSize : ⦃ MonadIO M ⦄ → Window → M (V2 CInt)
    glGetProcAddress  : ⦃ MonadIO M ⦄ → CString → M (Ptr ⊤)

{-# COMPILE GHC defaultOpenGL = SDL.Video.OpenGL.defaultOpenGL #-}

{-# COMPILE GHC glGetDrawableSize = \ m AgdaMonadIO    -> SDL.Video.OpenGL.glGetDrawableSize #-}
{-# COMPILE GHC glGetProcAddress  = \ mℓ m AgdaMonadIO -> SDL.Video.OpenGL.glGetProcAddress  #-}


postulate
    GLContext : Set
    glCreateContext : ⦃ MonadIO M ⦄ → Window → M GLContext
    glMakeCurrent   : ⦃ MonadIO M ⦄ → Window → GLContext → M ⊤′
    glDeleteContext : ⦃ MonadIO M ⦄ → GLContext → M ⊤′

{-# COMPILE GHC GLContext = type SDL.Video.OpenGL.GLContext #-}

{-# COMPILE GHC glCreateContext = \ mℓ m AgdaMonadIO -> SDL.Video.OpenGL.glCreateContext #-}
{-# COMPILE GHC glMakeCurrent   = \ mℓ m AgdaMonadIO -> SDL.Video.OpenGL.glMakeCurrent   #-}
{-# COMPILE GHC glDeleteContext = \ mℓ m AgdaMonadIO -> SDL.Video.OpenGL.glDeleteContext #-}


data SwapInterval : Set where
    ImmediateUpdates    : SwapInterval
    SynchronizedUpdates : SwapInterval
    LateSwapTearing     : SwapInterval

{-# COMPILE GHC SwapInterval = data SDL.Video.OpenGL.SwapInterval
    ( SDL.Video.OpenGL.ImmediateUpdates
    | SDL.Video.OpenGL.SynchronizedUpdates
    | SDL.Video.OpenGL.LateSwapTearing
    ) #-}

postulate
    Bounded[SwapInterval] : Bounded SwapInterval
    Enum[SwapInterval]    : Enum SwapInterval
    Eq[SwapInterval]      : Eq SwapInterval
    Data[SwapInterval]    : Data SwapInterval
    Ord[SwapInterval]     : Ord SwapInterval
    Read[SwapInterval]    : Read SwapInterval
    Show[SwapInterval]    : Show SwapInterval

{-# COMPILE GHC Bounded[SwapInterval] = AgdaBounded #-}
{-# COMPILE GHC Enum[SwapInterval]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[SwapInterval]      = AgdaEq      #-}
{-# COMPILE GHC Data[SwapInterval]    = AgdaData    #-}
{-# COMPILE GHC Ord[SwapInterval]     = AgdaOrd     #-}
{-# COMPILE GHC Read[SwapInterval]    = AgdaRead    #-}
{-# COMPILE GHC Show[SwapInterval]    = AgdaShow    #-}


postulate
    glSwapWindow : ⦃ MonadIO M ⦄ → Window → M ⊤′
    swapInterval : StateVar SwapInterval

{-# COMPILE GHC glSwapWindow = \ mℓ m AgdaMonadIO -> SDL.Video.OpenGL.glSwapWindow #-}
{-# COMPILE GHC swapInterval =                       SDL.Video.OpenGL.swapInterval #-}
