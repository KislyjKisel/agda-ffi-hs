{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.Uniform-Instanced where

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.Uniform

instance
    inst:Eq[UniformLocation]   = Eq[UniformLocation]
    inst:Ord[UniformLocation]  = Ord[UniformLocation]
    inst:Show[UniformLocation] = Show[UniformLocation]

    inst:UniformComponent[GLint]    = UniformComponent[GLint]
    inst:UniformComponent[GLuint]   = UniformComponent[GLuint]
    inst:UniformComponent[GLfloat]  = UniformComponent[GLfloat]
    inst:UniformComponent[GLdouble] = UniformComponent[GLdouble]

    inst:Uniform[GLint]        = Uniform[GLint]
    inst:Uniform[GLuint]       = Uniform[GLuint]
    inst:Uniform[GLfloat]      = Uniform[GLfloat]
    inst:Uniform[GLdouble]     = Uniform[GLdouble]
    inst:Uniform[TextureUnit]  = Uniform[TextureUnit]
    inst:Uniform[Vector4[A]]   = Uniform[Vector4[A]]
    inst:Uniform[Vector3[A]]   = Uniform[Vector3[A]]
    inst:Uniform[Vector2[A]]   = Uniform[Vector2[A]]
    inst:Uniform[Vector1[A]]   = Uniform[Vector1[A]]
    inst:Uniform[Vertex4[A]]   = Uniform[Vertex4[A]]
    inst:Uniform[Vertex3[A]]   = Uniform[Vertex3[A]]
    inst:Uniform[Vertex2[A]]   = Uniform[Vertex2[A]]
    inst:Uniform[Vertex1[A]]   = Uniform[Vertex1[A]]
    inst:Uniform[Index1[A]]    = Uniform[Index1[A]]
    inst:Uniform[Color4[A]]    = Uniform[Color4[A]]
    inst:Uniform[Color3[A]]    = Uniform[Color3[A]]
    inst:Uniform[FogCoord1[A]] = Uniform[FogCoord1[A]]
    inst:Uniform[Normal3[A]]   = Uniform[Normal3[A]]
    inst:Uniform[TexCoord4[A]] = Uniform[TexCoord4[A]]
    inst:Uniform[TexCoord3[A]] = Uniform[TexCoord3[A]]
    inst:Uniform[TexCoord2[A]] = Uniform[TexCoord2[A]]
    inst:Uniform[TexCoord1[A]] = Uniform[TexCoord1[A]]
    inst:Uniform[GLmatrix[A]]  = Uniform[GLmatrix[A]]
