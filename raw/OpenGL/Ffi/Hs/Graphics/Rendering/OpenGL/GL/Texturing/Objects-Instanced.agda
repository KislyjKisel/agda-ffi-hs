{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Objects-Instanced where

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Objects

instance
    inst:Eq[TextureObject]                    = Eq[TextureObject]
    inst:Ord[TextureObject]                   = Ord[TextureObject]
    inst:Show[TextureObject]                  = Show[TextureObject]
    inst:ObjectName[TextureObject]            = ObjectName[TextureObject]
    inst:GeneratableObjectName[TextureObject] = GeneratableObjectName[TextureObject]
    inst:CanBeLabeled[TextureObject]          = CanBeLabeled[TextureObject]
