{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries where

open import Agda.Builtin.List        using (List)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.ByteString   using (ByteString)
open import Ffi.Hs.Data.StateVar     using (SettableStateVar; GettableStateVar)
open import Ffi.Hs.Graphics.GL.Types using (GLenum)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects using (Shader)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}


data ShaderBinaryFormat : Set where
    mkShaderBinaryFormat : GLenum → ShaderBinaryFormat

{-# COMPILE GHC ShaderBinaryFormat = data Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries.ShaderBinaryFormat
    ( Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries.ShaderBinaryFormat
    ) #-}

postulate
    Eq[ShaderBinaryFormat]   : Eq ShaderBinaryFormat
    Ord[ShaderBinaryFormat]  : Ord ShaderBinaryFormat
    Show[ShaderBinaryFormat] : Show ShaderBinaryFormat

{-# COMPILE GHC Eq[ShaderBinaryFormat]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ShaderBinaryFormat]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ShaderBinaryFormat] = AgdaShow #-}

postulate
    shaderBinaryFormats : GettableStateVar (List ShaderBinaryFormat)

{-# COMPILE GHC shaderBinaryFormats = Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries.shaderBinaryFormats #-}


data ShaderBinary : Set where
    mkShaderBinary : ShaderBinaryFormat → ByteString → ShaderBinary

{-# COMPILE GHC ShaderBinary = data Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries.ShaderBinary
    ( Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries.ShaderBinary
    ) #-}

postulate
    Eq[ShaderBinary]   : Eq ShaderBinary
    Ord[ShaderBinary]  : Ord ShaderBinary
    Show[ShaderBinary] : Show ShaderBinary

{-# COMPILE GHC Eq[ShaderBinary]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ShaderBinary]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ShaderBinary] = AgdaShow #-}

postulate
    shaderBinary : List Shader → SettableStateVar ShaderBinary

{-# COMPILE GHC shaderBinary = Graphics.Rendering.OpenGL.GL.Shaders.ShaderBinaries.shaderBinary #-}
