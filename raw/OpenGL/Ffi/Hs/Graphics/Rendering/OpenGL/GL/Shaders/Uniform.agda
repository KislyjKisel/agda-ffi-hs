{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.Uniform where

open import Agda.Builtin.Char        using (Char)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Unit        using (⊤)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show; Storable)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple3)
open import Ffi.Hs.Foreign.Ptr       using (Ptr)
open import Ffi.Hs.Graphics.GL.Types

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.CoordTrans             using (GLmatrix; MatrixComponent)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.Attribs        using (VariableType)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects using (Program)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Tensor
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Shaders.Uniform
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.CoordTrans (AgdaMatrixComponent(AgdaMatrixComponent))
#-}

private
    variable
        A : Set


data UniformLocation : Set where
    mkUniformLocation : GLint → UniformLocation

{-# COMPILE GHC UniformLocation = data Graphics.Rendering.OpenGL.GL.Shaders.Uniform.UniformLocation
    ( Graphics.Rendering.OpenGL.GL.Shaders.Uniform.UniformLocation
    ) #-}

postulate
    Eq[UniformLocation]   : Eq UniformLocation
    Ord[UniformLocation]  : Ord UniformLocation
    Show[UniformLocation] : Show UniformLocation

{-# COMPILE GHC Eq[UniformLocation]   = AgdaEq   #-}
{-# COMPILE GHC Ord[UniformLocation]  = AgdaOrd  #-}
{-# COMPILE GHC Show[UniformLocation] = AgdaShow #-}

postulate
    uniformLocation : Program → List Char → GettableStateVar UniformLocation
    activeUniforms  : Program → GettableStateVar (List (Tuple3 GLint VariableType (List Char)))

{-# COMPILE GHC uniformLocation = Graphics.Rendering.OpenGL.GL.Shaders.Uniform.uniformLocation #-}
{-# COMPILE GHC activeUniforms  = Graphics.Rendering.OpenGL.GL.Shaders.Uniform.activeUniforms  #-}


postulate
    UniformComponent : Set → Set

    UniformComponent[A]⇒Storable[A] : ⦃ UniformComponent A ⦄ → Storable A

    UniformComponent[GLint]    : UniformComponent GLint
    UniformComponent[GLuint]   : UniformComponent GLuint
    UniformComponent[GLfloat]  : UniformComponent GLfloat
    UniformComponent[GLdouble] : UniformComponent GLdouble

{-# FOREIGN GHC data AgdaUniformComponent a = Graphics.Rendering.OpenGL.GL.Shaders.Uniform.UniformComponent a => AgdaUniformComponent #-}
{-# COMPILE GHC UniformComponent = type(0) AgdaUniformComponent #-}

{-# COMPILE GHC UniformComponent[A]⇒Storable[A] = \ a AgdaUniformComponent -> AgdaStorable #-}

{-# COMPILE GHC UniformComponent[GLint]    = AgdaUniformComponent #-}
{-# COMPILE GHC UniformComponent[GLuint]   = AgdaUniformComponent #-}
{-# COMPILE GHC UniformComponent[GLfloat]  = AgdaUniformComponent #-}
{-# COMPILE GHC UniformComponent[GLdouble] = AgdaUniformComponent #-}


postulate
    Uniform : Set → Set

    uniform  : ⦃ Uniform A ⦄ → UniformLocation → StateVar A
    uniformv : ⦃ Uniform A ⦄ → UniformLocation → GLsizei → Ptr A → IO ⊤

    Uniform[GLint]        : Uniform GLint
    Uniform[GLuint]       : Uniform GLuint
    Uniform[GLfloat]      : Uniform GLfloat
    Uniform[GLdouble]     : Uniform GLdouble
    Uniform[TextureUnit]  : Uniform TextureUnit
    Uniform[Vector4[A]]   : ⦃ UniformComponent A ⦄ → Uniform (Vector4 A)
    Uniform[Vector3[A]]   : ⦃ UniformComponent A ⦄ → Uniform (Vector3 A)
    Uniform[Vector2[A]]   : ⦃ UniformComponent A ⦄ → Uniform (Vector2 A)
    Uniform[Vector1[A]]   : ⦃ UniformComponent A ⦄ → Uniform (Vector1 A)
    Uniform[Vertex4[A]]   : ⦃ UniformComponent A ⦄ → Uniform (Vertex4 A)
    Uniform[Vertex3[A]]   : ⦃ UniformComponent A ⦄ → Uniform (Vertex3 A)
    Uniform[Vertex2[A]]   : ⦃ UniformComponent A ⦄ → Uniform (Vertex2 A)
    Uniform[Vertex1[A]]   : ⦃ UniformComponent A ⦄ → Uniform (Vertex1 A)
    Uniform[Index1[A]]    : ⦃ UniformComponent A ⦄ → Uniform (Index1 A)
    Uniform[Color4[A]]    : ⦃ UniformComponent A ⦄ → Uniform (Color4 A)
    Uniform[Color3[A]]    : ⦃ UniformComponent A ⦄ → Uniform (Color3 A)
    Uniform[FogCoord1[A]] : ⦃ UniformComponent A ⦄ → Uniform (FogCoord1 A)
    Uniform[Normal3[A]]   : ⦃ UniformComponent A ⦄ → Uniform (Normal3 A)
    Uniform[TexCoord4[A]] : ⦃ UniformComponent A ⦄ → Uniform (TexCoord4 A)
    Uniform[TexCoord3[A]] : ⦃ UniformComponent A ⦄ → Uniform (TexCoord3 A)
    Uniform[TexCoord2[A]] : ⦃ UniformComponent A ⦄ → Uniform (TexCoord2 A)
    Uniform[TexCoord1[A]] : ⦃ UniformComponent A ⦄ → Uniform (TexCoord1 A)
    Uniform[GLmatrix[A]]  : ⦃ MatrixComponent A ⦄ → Uniform (GLmatrix A)

{-# FOREIGN GHC data AgdaUniform a = Graphics.Rendering.OpenGL.GL.Shaders.Uniform.Uniform a => AgdaUniform #-}
{-# COMPILE GHC Uniform = type(0) AgdaUniform #-}

{-# COMPILE GHC uniform  = \ a AgdaUniform -> Graphics.Rendering.OpenGL.GL.Shaders.Uniform.uniform  #-}
{-# COMPILE GHC uniformv = \ a AgdaUniform -> Graphics.Rendering.OpenGL.GL.Shaders.Uniform.uniformv #-}

{-# COMPILE GHC Uniform[GLint]        =                             AgdaUniform #-}
{-# COMPILE GHC Uniform[GLuint]       =                             AgdaUniform #-}
{-# COMPILE GHC Uniform[GLfloat]      =                             AgdaUniform #-}
{-# COMPILE GHC Uniform[GLdouble]     =                             AgdaUniform #-}
{-# COMPILE GHC Uniform[TextureUnit]  =                             AgdaUniform #-}
{-# COMPILE GHC Uniform[Vector4[A]]   = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Vector3[A]]   = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Vector2[A]]   = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Vector1[A]]   = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Vertex4[A]]   = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Vertex3[A]]   = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Vertex2[A]]   = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Vertex1[A]]   = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Index1[A]]    = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Color4[A]]    = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Color3[A]]    = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[FogCoord1[A]] = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[Normal3[A]]   = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[TexCoord4[A]] = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[TexCoord3[A]] = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[TexCoord2[A]] = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[TexCoord1[A]] = \ a AgdaUniformComponent -> AgdaUniform #-}
{-# COMPILE GHC Uniform[GLmatrix[A]]  = \ a AgdaMatrixComponent -> AgdaUniform #-}
