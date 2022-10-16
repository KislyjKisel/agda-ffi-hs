{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Application where

open import Ffi.Hs.Data.StateVar using (StateVar)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification using (ParameterizedTextureTarget)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays            using (Capability)

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Texturing.Objects
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification
    (AgdaParameterizedTextureTarget(AgdaParameterizedTextureTarget))
#-}

postulate
    texture : ∀{T} → ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar Capability

{-# COMPILE GHC texture = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Application.texture #-}
