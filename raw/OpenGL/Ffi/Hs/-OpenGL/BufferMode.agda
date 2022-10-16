{-# OPTIONS --without-K #-}

module Ffi.Hs.-OpenGL.BufferMode where

open import Ffi.Hs.Graphics.GL.Types using (GLsizei)

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Framebuffer
#-}

data BufferMode : Set where
    NoBuffers           : BufferMode
    FrontLeftBuffer     : BufferMode
    FrontRightBuffer    : BufferMode
    BackLeftBuffer      : BufferMode
    BackRightBuffer     : BufferMode
    FrontBuffers        : BufferMode
    BackBuffers         : BufferMode
    LeftBuffers         : BufferMode
    RightBuffers        : BufferMode
    FrontAndBackBuffers : BufferMode
    AuxBuffer           : GLsizei → BufferMode
    FBOColorAttachment  : GLsizei → BufferMode

{-# COMPILE GHC BufferMode = data Graphics.Rendering.OpenGL.GL.Framebuffer.BufferMode
    ( Graphics.Rendering.OpenGL.GL.Framebuffer.NoBuffers
    | Graphics.Rendering.OpenGL.GL.Framebuffer.FrontLeftBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.FrontRightBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.BackLeftBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.BackRightBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.FrontBuffers
    | Graphics.Rendering.OpenGL.GL.Framebuffer.BackBuffers
    | Graphics.Rendering.OpenGL.GL.Framebuffer.LeftBuffers
    | Graphics.Rendering.OpenGL.GL.Framebuffer.RightBuffers
    | Graphics.Rendering.OpenGL.GL.Framebuffer.FrontAndBackBuffers
    | Graphics.Rendering.OpenGL.GL.Framebuffer.AuxBuffer
    | Graphics.Rendering.OpenGL.GL.Framebuffer.FBOColorAttachment
    ) #-}
