{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.Attribs where

open import Agda.Builtin.Char        using (Char)
open import Agda.Builtin.List        using (List)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple3)
open import Ffi.Hs.Graphics.GL.Types using (GLint)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects using (Program)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec             using (AttribLocation)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Shaders.Attribs
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}


postulate
    attribLocation : Program → List Char → StateVar AttribLocation

{-# COMPILE GHC attribLocation = Graphics.Rendering.OpenGL.GL.Shaders.Attribs.attribLocation #-}


data VariableType : Set where
    Float'                    : VariableType
    FloatVec2                 : VariableType
    FloatVec3                 : VariableType
    FloatVec4                 : VariableType
    Int'                      : VariableType
    IntVec2                   : VariableType
    IntVec3                   : VariableType
    IntVec4                   : VariableType
    UnsignedInt'              : VariableType
    UnsignedIntVec2           : VariableType
    UnsignedIntVec3           : VariableType
    UnsignedIntVec4           : VariableType
    Bool                      : VariableType
    BoolVec2                  : VariableType
    BoolVec3                  : VariableType
    BoolVec4                  : VariableType
    FloatMat2                 : VariableType
    FloatMat3                 : VariableType
    FloatMat4                 : VariableType
    FloatMat2x3               : VariableType
    FloatMat2x4               : VariableType
    FloatMat3x2               : VariableType
    FloatMat3x4               : VariableType
    FloatMat4x2               : VariableType
    FloatMat4x3               : VariableType
    Sampler1D                 : VariableType
    Sampler2D                 : VariableType
    Sampler3D                 : VariableType
    SamplerCube               : VariableType
    Sampler1DShadow           : VariableType
    Sampler2DShadow           : VariableType
    Sampler1DArray            : VariableType
    Sampler2DArray            : VariableType
    Sampler1DArrayShadow      : VariableType
    Sampler2DArrayShadow      : VariableType
    SamplerCubeShadow         : VariableType
    Sampler2DRect             : VariableType
    Sampler2DRectShadow       : VariableType
    IntSampler1D              : VariableType
    IntSampler2D              : VariableType
    IntSampler3D              : VariableType
    IntSamplerCube            : VariableType
    IntSampler1DArray         : VariableType
    IntSampler2DArray         : VariableType
    UnsignedIntSampler1D      : VariableType
    UnsignedIntSampler2D      : VariableType
    UnsignedIntSampler3D      : VariableType
    UnsignedIntSamplerCube    : VariableType
    UnsignedIntSampler1DArray : VariableType
    UnsignedIntSampler2DArray : VariableType

{-# COMPILE GHC VariableType = data Graphics.Rendering.OpenGL.GL.Shaders.Attribs.VariableType
    ( Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Float'
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatVec2
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatVec3
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatVec4
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Int'
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.IntVec2
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.IntVec3
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.IntVec4
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedInt'
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedIntVec2
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedIntVec3
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedIntVec4
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Bool
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.BoolVec2
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.BoolVec3
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.BoolVec4
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatMat2
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatMat3
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatMat4
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatMat2x3
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatMat2x4
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatMat3x2
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatMat3x4
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatMat4x2
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.FloatMat4x3
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler1D
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler2D
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler3D
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.SamplerCube
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler1DShadow
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler2DShadow
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler1DArray
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler2DArray
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler1DArrayShadow
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler2DArrayShadow
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.SamplerCubeShadow
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler2DRect
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.Sampler2DRectShadow
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.IntSampler1D
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.IntSampler2D
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.IntSampler3D
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.IntSamplerCube
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.IntSampler1DArray
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.IntSampler2DArray
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedIntSampler1D
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedIntSampler2D
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedIntSampler3D
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedIntSamplerCube
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedIntSampler1DArray
    | Graphics.Rendering.OpenGL.GL.Shaders.Attribs.UnsignedIntSampler2DArray
    ) #-}

postulate
    Eq[VariableType]   : Eq VariableType
    Ord[VariableType]  : Ord VariableType
    Show[VariableType] : Show VariableType

{-# COMPILE GHC Eq[VariableType]   = AgdaEq   #-}
{-# COMPILE GHC Ord[VariableType]  = AgdaOrd  #-}
{-# COMPILE GHC Show[VariableType] = AgdaShow #-}

postulate
    activeAttribs : Program → GettableStateVar (List (Tuple3 GLint VariableType (List Char)))

{-# COMPILE GHC activeAttribs = Graphics.Rendering.OpenGL.GL.Shaders.Attribs.activeAttribs #-}
