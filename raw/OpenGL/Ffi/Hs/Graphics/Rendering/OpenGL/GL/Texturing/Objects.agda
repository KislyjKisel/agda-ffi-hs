{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Objects where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Builtin.Unit        using (⊤)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.ObjectName   using (ObjectName; GeneratableObjectName)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple2)
open import Ffi.Hs.Graphics.GL.Types using (GLuint; GLclampf)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput using (CanBeLabeled)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification
    using (BindableTextureTarget; ParameterizedTextureTarget)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Texturing.Objects
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput (AgdaCanBeLabeled(AgdaCanBeLabeled))
import MAlonzo.Code.Ffi.Hs.Data.ObjectName (AgdaObjectName(AgdaObjectName), AgdaGeneratableObjectName(AgdaGeneratableObjectName))
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification
    (AgdaBindableTextureTarget(AgdaBindableTextureTarget), AgdaParameterizedTextureTarget(AgdaParameterizedTextureTarget))
#-}

private
    variable
        T : Set


data TextureObject : Set where
    mkTextureObject : GLuint → TextureObject

{-# COMPILE GHC TextureObject = data Graphics.Rendering.OpenGL.GL.Texturing.Objects.TextureObject
    ( Graphics.Rendering.OpenGL.GL.Texturing.Objects.TextureObject
    ) #-}

postulate
    Eq[TextureObject]                    : Eq TextureObject
    Ord[TextureObject]                   : Ord TextureObject
    Show[TextureObject]                  : Show TextureObject
    ObjectName[TextureObject]            : ObjectName TextureObject
    GeneratableObjectName[TextureObject] : GeneratableObjectName TextureObject
    CanBeLabeled[TextureObject]          : CanBeLabeled TextureObject

{-# COMPILE GHC Eq[TextureObject]                    = AgdaEq                    #-}
{-# COMPILE GHC Ord[TextureObject]                   = AgdaOrd                   #-}
{-# COMPILE GHC Show[TextureObject]                  = AgdaShow                  #-}
{-# COMPILE GHC ObjectName[TextureObject]            = AgdaObjectName            #-}
{-# COMPILE GHC GeneratableObjectName[TextureObject] = AgdaGeneratableObjectName #-}
{-# COMPILE GHC CanBeLabeled[TextureObject]          = AgdaCanBeLabeled          #-}


TexturePriority : Set
TexturePriority = GLclampf

postulate
    textureBinding      : ⦃ BindableTextureTarget T ⦄ → T → StateVar (Maybe TextureObject)
    textureResident     : ⦃ ParameterizedTextureTarget T ⦄ → T → GettableStateVar Bool
    areTexturesResident : List TextureObject → IO (Tuple2 (List TextureObject) (List TextureObject))
    texturePriority     : ⦃ ParameterizedTextureTarget T ⦄ → T → StateVar TexturePriority
    prioritizeTextures  : List (Tuple2 TextureObject TexturePriority) → IO ⊤
    generateMipmap'     : ⦃ ParameterizedTextureTarget T ⦄ → T → IO ⊤

{-# COMPILE GHC textureBinding      = \ t AgdaBindableTextureTarget      -> Graphics.Rendering.OpenGL.GL.Texturing.Objects.textureBinding      #-}
{-# COMPILE GHC textureResident     = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Objects.textureResident     #-}
{-# COMPILE GHC areTexturesResident =                                       Graphics.Rendering.OpenGL.GL.Texturing.Objects.areTexturesResident #-}
{-# COMPILE GHC texturePriority     = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Objects.texturePriority     #-}
{-# COMPILE GHC prioritizeTextures  =                                       Graphics.Rendering.OpenGL.GL.Texturing.Objects.prioritizeTextures  #-}
{-# COMPILE GHC generateMipmap'     = \ t AgdaParameterizedTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Objects.generateMipmap'     #-}
