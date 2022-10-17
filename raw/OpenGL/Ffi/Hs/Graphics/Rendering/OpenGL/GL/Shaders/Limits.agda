{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.Limits where

open import Ffi.Hs.Data.StateVar     using (GettableStateVar)
open import Ffi.Hs.Graphics.GL.Types using (GLsizei)

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Shaders.Limits
#-}

postulate
    maxVertexTextureImageUnits   : GettableStateVar GLsizei
    maxTextureImageUnits         : GettableStateVar GLsizei
    maxCombinedTextureImageUnits : GettableStateVar GLsizei
    maxTextureCoords             : GettableStateVar GLsizei
    maxVertexUniformComponents   : GettableStateVar GLsizei
    maxFragmentUniformComponents : GettableStateVar GLsizei
    maxVertexAttribs             : GettableStateVar GLsizei
    maxVaryingFloats             : GettableStateVar GLsizei

{-# COMPILE GHC maxVertexTextureImageUnits   = Graphics.Rendering.OpenGL.GL.Shaders.Limits.maxVertexTextureImageUnits   #-}
{-# COMPILE GHC maxTextureImageUnits         = Graphics.Rendering.OpenGL.GL.Shaders.Limits.maxTextureImageUnits         #-}
{-# COMPILE GHC maxCombinedTextureImageUnits = Graphics.Rendering.OpenGL.GL.Shaders.Limits.maxCombinedTextureImageUnits #-}
{-# COMPILE GHC maxTextureCoords             = Graphics.Rendering.OpenGL.GL.Shaders.Limits.maxTextureCoords             #-}
{-# COMPILE GHC maxVertexUniformComponents   = Graphics.Rendering.OpenGL.GL.Shaders.Limits.maxVertexUniformComponents   #-}
{-# COMPILE GHC maxFragmentUniformComponents = Graphics.Rendering.OpenGL.GL.Shaders.Limits.maxFragmentUniformComponents #-}
{-# COMPILE GHC maxVertexAttribs             = Graphics.Rendering.OpenGL.GL.Shaders.Limits.maxVertexAttribs             #-}
{-# COMPILE GHC maxVaryingFloats             = Graphics.Rendering.OpenGL.GL.Shaders.Limits.maxVaryingFloats             #-}
