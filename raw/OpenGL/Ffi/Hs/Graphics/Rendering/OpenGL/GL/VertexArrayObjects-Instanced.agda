{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrayObjects-Instanced where

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrayObjects

instance
    inst:Eq[VertexArrayObject]                    = Eq[VertexArrayObject]
    inst:Ord[VertexArrayObject]                   = Ord[VertexArrayObject]
    inst:Show[VertexArrayObject]                  = Show[VertexArrayObject]
    inst:ObjectName[VertexArrayObject]            = ObjectName[VertexArrayObject]
    inst:GeneratableObjectName[VertexArrayObject] = GeneratableObjectName[VertexArrayObject]
    inst:CanBeLabeled[VertexArrayObject]          = CanBeLabeled[VertexArrayObject]
