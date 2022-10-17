{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.Char        using (Char)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Unit        using (⊤)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.ByteString   using (ByteString)
open import Ffi.Hs.Data.ObjectName   using (ObjectName)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple2)
open import Ffi.Hs.Graphics.GL.Types using (GLint)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput using (CanBeLabeled)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput (AgdaCanBeLabeled(AgdaCanBeLabeled))
import MAlonzo.Code.Ffi.Hs.Data.ObjectName (AgdaObjectName(AgdaObjectName))
#-}

postulate
    shaderCompiler : GettableStateVar Bool

{-# COMPILE GHC shaderCompiler = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.shaderCompiler #-}

data ShaderType : Set where
    VertexShader         : ShaderType
    TessControlShader    : ShaderType
    TessEvaluationShader : ShaderType
    GeometryShader       : ShaderType
    FragmentShader       : ShaderType
    ComputeShader        : ShaderType

{-# COMPILE GHC ShaderType = data Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.ShaderType
    ( Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.VertexShader
    | Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.TessControlShader
    | Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.TessEvaluationShader
    | Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.GeometryShader
    | Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.FragmentShader
    | Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.ComputeShader
    ) #-}

postulate
    Eq[ShaderType]   : Eq ShaderType
    Ord[ShaderType]  : Ord ShaderType
    Show[ShaderType] : Show ShaderType

{-# COMPILE GHC Eq[ShaderType]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ShaderType]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ShaderType] = AgdaShow #-}


postulate
    Shader : Set

{-# COMPILE GHC Shader = type Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.Shader #-}

postulate
    Eq[Shader]           : Eq Shader
    Ord[Shader]          : Ord Shader
    Show[Shader]         : Show Shader
    ObjectName[Shader]   : ObjectName Shader
    CanBeLabeled[Shader] : CanBeLabeled Shader

{-# COMPILE GHC Eq[Shader]           = AgdaEq           #-}
{-# COMPILE GHC Ord[Shader]          = AgdaOrd          #-}
{-# COMPILE GHC Show[Shader]         = AgdaShow         #-}
{-# COMPILE GHC ObjectName[Shader]   = AgdaObjectName   #-}
{-# COMPILE GHC CanBeLabeled[Shader] = AgdaCanBeLabeled #-}


postulate
    PrecisionType : Set

{-# COMPILE GHC PrecisionType = type Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.PrecisionType #-}

postulate
    Eq[PrecisionType]   : Eq PrecisionType
    Ord[PrecisionType]  : Ord PrecisionType
    Show[PrecisionType] : Show PrecisionType

{-# COMPILE GHC Eq[PrecisionType]   = AgdaEq   #-}
{-# COMPILE GHC Ord[PrecisionType]  = AgdaOrd  #-}
{-# COMPILE GHC Show[PrecisionType] = AgdaShow #-}


postulate
    createShader          : ShaderType → IO Shader
    shaderSourceBS        : Shader → StateVar ByteString
    compileShader         : Shader → IO ⊤
    releaseShaderCompiler : IO ⊤
    shaderType            : Shader → GettableStateVar ShaderType
    shaderDeleteStatus    : Shader → GettableStateVar Bool
    compileStatus         : Shader → GettableStateVar Bool
    shaderInfoLog         : Shader → GettableStateVar (List Char)
    shaderPrecisionFormat : ShaderType → PrecisionType → GettableStateVar (Tuple2 (Tuple2 GLint GLint) GLint) 
    packUtf8              : List Char → ByteString
    unpackUtf8            : ByteString → List Char

{-# COMPILE GHC createShader          = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.createShader          #-}
{-# COMPILE GHC shaderSourceBS        = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.shaderSourceBS        #-}
{-# COMPILE GHC compileShader         = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.compileShader         #-}
{-# COMPILE GHC releaseShaderCompiler = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.releaseShaderCompiler #-}
{-# COMPILE GHC shaderType            = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.shaderType            #-}
{-# COMPILE GHC shaderDeleteStatus    = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.shaderDeleteStatus    #-}
{-# COMPILE GHC compileStatus         = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.compileStatus         #-}
{-# COMPILE GHC shaderInfoLog         = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.shaderInfoLog         #-}
{-# COMPILE GHC shaderPrecisionFormat = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.shaderPrecisionFormat #-}
{-# COMPILE GHC packUtf8              = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.packUtf8              #-}
{-# COMPILE GHC unpackUtf8            = Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects.unpackUtf8            #-}
