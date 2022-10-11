{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Framebuffer where

open import Agda.Builtin.IO   using (IO)
open import Agda.Builtin.List using (List)
open import Agda.Builtin.Unit using (⊤)

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Framebuffer
#-}

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
    clear : List ClearBuffer → IO ⊤

{-# COMPILE GHC clear = Graphics.Rendering.OpenGL.GL.Framebuffer.clear #-}
