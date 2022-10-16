{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Framebuffer where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Unit        using (⊤)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar     using (StateVar; SettableStateVar; GettableStateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple2)
open import Ffi.Hs.Graphics.GL.Types using (GLuint; GLsizei; GLint; GLfloat; GLdouble)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Colors       using (Face)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.CoordTrans   using (Position; Size)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays using (Capability)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec   using (Color4; Index1)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects
    using (FramebufferObject; FramebufferTarget)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments
    using (FramebufferObjectAttachment; FramebufferAttachment)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Framebuffer
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects.Attachments
    (AgdaFramebufferAttachment(AgdaFramebufferAttachment))
#-}


DrawBufferIndex : Set
DrawBufferIndex = GLuint

open import Ffi.Hs.-OpenGL.BufferMode public

postulate
    Eq[BufferMode]                    : Eq BufferMode
    Ord[BufferMode]                   : Ord BufferMode
    Show[BufferMode]                  : Show BufferMode
    FramebufferAttachment[BufferMode] : FramebufferAttachment BufferMode

{-# COMPILE GHC Eq[BufferMode]                    = AgdaEq                    #-}
{-# COMPILE GHC Ord[BufferMode]                   = AgdaOrd                   #-}
{-# COMPILE GHC Show[BufferMode]                  = AgdaShow                  #-}
{-# COMPILE GHC FramebufferAttachment[BufferMode] = AgdaFramebufferAttachment #-}


data ClearBuffer : Set where
    ColorBuffer   : ClearBuffer
    AccumBuffer   : ClearBuffer
    StencilBuffer : ClearBuffer
    DepthBuffer   : ClearBuffer

{-# COMPILE GHC ClearBuffer = data Graphics.Rendering.OpenGL.GL.Framebuffer.ClearBuffer
    ( Graphics.Rendering.OpenGL.GL.Framebuffer.ColorBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.AccumBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.StencilBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.DepthBuffer
    ) #-}

postulate
    Eq[ClearBuffer]   : Eq ClearBuffer
    Ord[ClearBuffer]  : Ord ClearBuffer
    Show[ClearBuffer] : Show ClearBuffer

{-# COMPILE GHC Eq[ClearBuffer]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ClearBuffer]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ClearBuffer] = AgdaShow #-}


data ClearBufferCommand : Set where
    ClearColorBufferInt         : DrawBufferIndex → Color4 GLint → ClearBufferCommand
    ClearColorBufferFloat       : DrawBufferIndex → Color4 GLfloat → ClearBufferCommand
    ClearColorBufferUint        : DrawBufferIndex → Color4 GLuint → ClearBufferCommand
    ClearDepthBuffer            : GLfloat → ClearBufferCommand
    ClearStencilBuffer          : GLint → ClearBufferCommand
    ClearDepthAndStencilBuffers : GLfloat → GLint → ClearBufferCommand

{-# COMPILE GHC ClearBufferCommand = data Graphics.Rendering.OpenGL.GL.Framebuffer.ClearBufferCommand
    ( Graphics.Rendering.OpenGL.GL.Framebuffer.ClearColorBufferUint
    | Graphics.Rendering.OpenGL.GL.Framebuffer.ClearColorBufferFloat
    | Graphics.Rendering.OpenGL.GL.Framebuffer.ClearColorBufferUint
    | Graphics.Rendering.OpenGL.GL.Framebuffer.ClearDepthBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.ClearStencilBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.ClearDepthAndStencilBuffers
    ) #-}

postulate
    Eq[ClearBufferCommand]   : Eq ClearBufferCommand
    Ord[ClearBufferCommand]  : Ord ClearBufferCommand
    Show[ClearBufferCommand] : Show ClearBufferCommand

{-# COMPILE GHC Eq[ClearBufferCommand]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ClearBufferCommand]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ClearBufferCommand] = AgdaShow #-}


data AccumOp : Set where
    Accum  : AccumOp
    Load   : AccumOp
    Return : AccumOp
    Mult   : AccumOp
    Add    : AccumOp

{-# COMPILE GHC AccumOp = data Graphics.Rendering.OpenGL.GL.Framebuffer.AccumOp
    ( Graphics.Rendering.OpenGL.GL.Framebuffer.Accum
    | Graphics.Rendering.OpenGL.GL.Framebuffer.Load
    | Graphics.Rendering.OpenGL.GL.Framebuffer.Return
    | Graphics.Rendering.OpenGL.GL.Framebuffer.Mult
    | Graphics.Rendering.OpenGL.GL.Framebuffer.Add
    ) #-}

postulate
    Eq[AccumOp]   : Eq AccumOp
    Ord[AccumOp]  : Ord AccumOp
    Show[AccumOp] : Show AccumOp

{-# COMPILE GHC Eq[AccumOp]   = AgdaEq   #-}
{-# COMPILE GHC Ord[AccumOp]  = AgdaOrd  #-}
{-# COMPILE GHC Show[AccumOp] = AgdaShow #-}


postulate
    drawBuffer                        : StateVar BufferMode
    namedFramebufferDrawBuffer        : FramebufferObject → SettableStateVar BufferMode
    drawBuffers                       : StateVar (List BufferMode)
    namedFramebufferDrawBuffers       : FramebufferObject → SettableStateVar (List BufferMode)
    drawBufferi                       : DrawBufferIndex → GettableStateVar BufferMode
    maxDrawBuffers                    : GettableStateVar GLsizei
    indexMask                         : StateVar GLuint
    colorMask                         : StateVar (Color4 Capability)
    colorMaski                        : DrawBufferIndex → StateVar (Color4 Capability)
    depthMask                         : StateVar Capability
    stencilMask                       : StateVar GLuint
    stencilMaskSeparate               : Face → StateVar GLuint
    clear                             : List ClearBuffer → IO ⊤
    clearColor                        : StateVar (Color4 GLfloat)
    clearIndex                        : StateVar (Index1 GLfloat)
    clearDepth                        : StateVar GLdouble
    clearDepthf                       : StateVar GLfloat
    clearStencil                      : StateVar GLint
    clearAccum                        : StateVar (Color4 GLfloat)
    clearBuffer                       : ClearBufferCommand → IO ⊤
    clearNamedFramebuffer             : FramebufferObject → ClearBufferCommand → IO ⊤
    invalidateSubFramebuffer          : FramebufferTarget → List FramebufferObjectAttachment → Tuple2 Position Size → IO ⊤
    invalidateNamedFramebufferSubData : FramebufferObject → List FramebufferObjectAttachment → Tuple2 Position Size → IO ⊤
    invalidateFramebuffer             : FramebufferTarget → List FramebufferObjectAttachment → IO ⊤
    invalidateNamedFramebufferData    : FramebufferObject → List FramebufferObjectAttachment → IO ⊤
    accum                             : AccumOp → GLfloat → IO ⊤
    auxBuffers                        : GettableStateVar GLsizei
    doubleBuffer                      : GettableStateVar Bool
    stereoBuffer                      : GettableStateVar Bool
    rgbaBits                          : GettableStateVar (Color4 GLsizei)
    stencilBits                       : GettableStateVar GLsizei
    depthBits                         : GettableStateVar GLsizei
    accumBits                         : GettableStateVar (Color4 GLsizei)
    rgbaSignedComponents              : GettableStateVar (Color4 Bool)

{-# COMPILE GHC drawBuffer                        = Graphics.Rendering.OpenGL.GL.Framebuffer.drawBuffer                        #-}
{-# COMPILE GHC namedFramebufferDrawBuffer        = Graphics.Rendering.OpenGL.GL.Framebuffer.namedFramebufferDrawBuffer        #-}
{-# COMPILE GHC drawBuffers                       = Graphics.Rendering.OpenGL.GL.Framebuffer.drawBuffers                       #-}
{-# COMPILE GHC namedFramebufferDrawBuffers       = Graphics.Rendering.OpenGL.GL.Framebuffer.namedFramebufferDrawBuffers       #-}
{-# COMPILE GHC drawBufferi                       = Graphics.Rendering.OpenGL.GL.Framebuffer.drawBufferi                       #-}
{-# COMPILE GHC maxDrawBuffers                    = Graphics.Rendering.OpenGL.GL.Framebuffer.maxDrawBuffers                    #-}
{-# COMPILE GHC indexMask                         = Graphics.Rendering.OpenGL.GL.Framebuffer.indexMask                         #-}
{-# COMPILE GHC colorMask                         = Graphics.Rendering.OpenGL.GL.Framebuffer.colorMask                         #-}
{-# COMPILE GHC colorMaski                        = Graphics.Rendering.OpenGL.GL.Framebuffer.colorMaski                        #-}
{-# COMPILE GHC depthMask                         = Graphics.Rendering.OpenGL.GL.Framebuffer.depthMask                         #-}
{-# COMPILE GHC stencilMask                       = Graphics.Rendering.OpenGL.GL.Framebuffer.stencilMask                       #-}
{-# COMPILE GHC stencilMaskSeparate               = Graphics.Rendering.OpenGL.GL.Framebuffer.stencilMaskSeparate               #-}
{-# COMPILE GHC clear                             = Graphics.Rendering.OpenGL.GL.Framebuffer.clear                             #-}
{-# COMPILE GHC clearColor                        = Graphics.Rendering.OpenGL.GL.Framebuffer.clearColor                        #-}
{-# COMPILE GHC clearIndex                        = Graphics.Rendering.OpenGL.GL.Framebuffer.clearIndex                        #-}
{-# COMPILE GHC clearDepth                        = Graphics.Rendering.OpenGL.GL.Framebuffer.clearDepth                        #-}
{-# COMPILE GHC clearDepthf                       = Graphics.Rendering.OpenGL.GL.Framebuffer.clearDepthf                       #-}
{-# COMPILE GHC clearStencil                      = Graphics.Rendering.OpenGL.GL.Framebuffer.clearStencil                      #-}
{-# COMPILE GHC clearAccum                        = Graphics.Rendering.OpenGL.GL.Framebuffer.clearAccum                        #-}
{-# COMPILE GHC clearBuffer                       = Graphics.Rendering.OpenGL.GL.Framebuffer.clearBuffer                       #-}
{-# COMPILE GHC clearNamedFramebuffer             = Graphics.Rendering.OpenGL.GL.Framebuffer.clearNamedFramebuffer             #-}
{-# COMPILE GHC invalidateSubFramebuffer          = Graphics.Rendering.OpenGL.GL.Framebuffer.invalidateSubFramebuffer          #-}
{-# COMPILE GHC invalidateNamedFramebufferSubData = Graphics.Rendering.OpenGL.GL.Framebuffer.invalidateNamedFramebufferSubData #-}
{-# COMPILE GHC invalidateFramebuffer             = Graphics.Rendering.OpenGL.GL.Framebuffer.invalidateFramebuffer             #-}
{-# COMPILE GHC invalidateNamedFramebufferData    = Graphics.Rendering.OpenGL.GL.Framebuffer.invalidateNamedFramebufferData    #-}
{-# COMPILE GHC accum                             = Graphics.Rendering.OpenGL.GL.Framebuffer.accum                             #-}
{-# COMPILE GHC auxBuffers                        = Graphics.Rendering.OpenGL.GL.Framebuffer.auxBuffers                        #-}
{-# COMPILE GHC doubleBuffer                      = Graphics.Rendering.OpenGL.GL.Framebuffer.doubleBuffer                      #-}
{-# COMPILE GHC stereoBuffer                      = Graphics.Rendering.OpenGL.GL.Framebuffer.stereoBuffer                      #-}
{-# COMPILE GHC rgbaBits                          = Graphics.Rendering.OpenGL.GL.Framebuffer.rgbaBits                          #-}
{-# COMPILE GHC stencilBits                       = Graphics.Rendering.OpenGL.GL.Framebuffer.stencilBits                       #-}
{-# COMPILE GHC depthBits                         = Graphics.Rendering.OpenGL.GL.Framebuffer.depthBits                         #-}
{-# COMPILE GHC accumBits                         = Graphics.Rendering.OpenGL.GL.Framebuffer.accumBits                         #-}
{-# COMPILE GHC rgbaSignedComponents              = Graphics.Rendering.OpenGL.GL.Framebuffer.rgbaSignedComponents              #-}
