{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Parameters where

open import Agda.Builtin.Maybe       using (Maybe)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple2)
open import Ffi.Hs.Graphics.GL.Types using (GLfloat; GLclampf)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.CoordTrans                 using (TextureCoordName)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PerFragment                using (ComparisonFunction)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable using (PixelInternalFormat)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification    using (ParameterizedTextureTarget; Level)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays               using (Capability)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec                 using (Color4)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Texturing.Parameters
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification
    (AgdaParameterizedTextureTarget(AgdaParameterizedTextureTarget))
#-}

private
    variable
        T : Set


data TextureFilter : Set where
    Nearest : TextureFilter
    Linear' : TextureFilter

{-# COMPILE GHC TextureFilter = data Graphics.Rendering.OpenGL.GL.Texturing.Parameters.TextureFilter
    ( Graphics.Rendering.OpenGL.GL.Texturing.Parameters.Nearest
    | Graphics.Rendering.OpenGL.GL.Texturing.Parameters.Linear'
    ) #-}

postulate
    Eq[TextureFilter]   : Eq TextureFilter
    Ord[TextureFilter]  : Ord TextureFilter
    Show[TextureFilter] : Show TextureFilter

{-# COMPILE GHC Eq[TextureFilter]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureFilter]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureFilter] = AgdaShow #-}


MinificationFilter : Set
MinificationFilter = Tuple2 TextureFilter (Maybe TextureFilter)

MagnificationFilter : Set
MagnificationFilter = TextureFilter


postulate
    textureFilter : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar (Tuple2 MinificationFilter MagnificationFilter)

{-# COMPILE GHC textureFilter = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureFilter #-}


data Repetition : Set where
    Repeated : Repetition
    Mirrored : Repetition

{-# COMPILE GHC Repetition = data Graphics.Rendering.OpenGL.GL.Texturing.Parameters.Repetition
    ( Graphics.Rendering.OpenGL.GL.Texturing.Parameters.Repeated
    | Graphics.Rendering.OpenGL.GL.Texturing.Parameters.Mirrored
    ) #-}

postulate
    Eq[Repetition]   : Eq Repetition
    Ord[Repetition]  : Ord Repetition
    Show[Repetition] : Show Repetition

{-# COMPILE GHC Eq[Repetition]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Repetition]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Repetition] = AgdaShow #-}


data Clamping : Set where
    Clamp         : Clamping
    Repeat        : Clamping
    ClampToEdge   : Clamping
    ClampToBorder : Clamping

{-# COMPILE GHC Clamping = data Clamping
    ( Graphics.Rendering.OpenGL.GL.Texturing.Parameters.Clamp
    | Graphics.Rendering.OpenGL.GL.Texturing.Parameters.Repeat
    | Graphics.Rendering.OpenGL.GL.Texturing.Parameters.ClampToEdge
    | Graphics.Rendering.OpenGL.GL.Texturing.Parameters.ClampToBorder
    )  #-}

postulate
    Eq[Clamping]   : Eq Clamping
    Ord[Clamping]  : Ord Clamping
    Show[Clamping] : Show Clamping

{-# COMPILE GHC Eq[Clamping]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Clamping]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Clamping] = AgdaShow #-}


LOD : Set
LOD = GLfloat

data TextureCompareOperator : Set where
    LequalR : TextureCompareOperator
    GequalR : TextureCompareOperator

{-# COMPILE GHC TextureCompareOperator = data Graphics.Rendering.OpenGL.GL.Texturing.Parameters.TextureCompareOperator
    ( Graphics.Rendering.OpenGL.GL.Texturing.Parameters.LequalR
    | Graphics.Rendering.OpenGL.GL.Texturing.Parameters.GequalR
    ) #-}

postulate
    Eq[TextureCompareOperator]   : Eq TextureCompareOperator
    Ord[TextureCompareOperator]  : Ord TextureCompareOperator
    Show[TextureCompareOperator] : Show TextureCompareOperator

{-# COMPILE GHC Eq[TextureCompareOperator]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureCompareOperator]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureCompareOperator] = AgdaShow #-}


postulate
    textureWrapMode         : ⦃ ParameterizedTextureTarget T ⦄ → T → TextureCoordName → StateVar (Tuple2 Repetition Clamping)
    textureBorderColor      : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar (Color4 GLfloat)
    textureObjectLODBias    : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar LOD
    maxTextureLODBias       : GettableStateVar LOD
    textureLODRange         : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar (Tuple2 LOD LOD)
    textureMaxAnisotropy    : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar GLfloat
    maxTextureMaxAnisotropy : GettableStateVar GLfloat
    textureLevelRange       : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar (Tuple2 Level Level)
    generateMipmap          : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar Capability
    depthTextureMode        : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar PixelInternalFormat
    textureCompareMode      : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar (Maybe ComparisonFunction)
    textureCompareFailValue : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar GLclampf
    textureCompareOperator  : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar (Maybe TextureCompareOperator)

-- {-# WARNING_ON_USAGE generateMipmap "OpenGL 3.1 deprecated this texture parameter, use `generateMipmap'` instead." #-}

{-# COMPILE GHC textureWrapMode         = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureWrapMode         #-}
{-# COMPILE GHC textureBorderColor      = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureBorderColor      #-}
{-# COMPILE GHC textureObjectLODBias    = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureObjectLODBias    #-}
{-# COMPILE GHC maxTextureLODBias       =                                       Graphics.Rendering.OpenGL.GL.Texturing.Parameters.maxTextureLODBias       #-}
{-# COMPILE GHC textureLODRange         = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureLODRange         #-}
{-# COMPILE GHC textureMaxAnisotropy    = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureMaxAnisotropy    #-}
{-# COMPILE GHC maxTextureMaxAnisotropy =                                       Graphics.Rendering.OpenGL.GL.Texturing.Parameters.maxTextureMaxAnisotropy #-}
{-# COMPILE GHC textureLevelRange       = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureLevelRange       #-}
{-# COMPILE GHC generateMipmap          = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.generateMipmap          #-}
{-# COMPILE GHC depthTextureMode        = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.depthTextureMode        #-}
{-# COMPILE GHC textureCompareMode      = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureCompareMode      #-}
{-# COMPILE GHC textureCompareFailValue = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureCompareFailValue #-}
{-# COMPILE GHC textureCompareOperator  = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Parameters.textureCompareOperator  #-}
