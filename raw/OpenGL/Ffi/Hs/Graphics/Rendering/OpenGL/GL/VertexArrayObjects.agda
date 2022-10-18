{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrayObjects where

open import Agda.Builtin.Maybe     using (Maybe)
open import Ffi.Hs.-base.Class     using (Eq; Ord; Show)
open import Ffi.Hs.Data.ObjectName using (ObjectName; GeneratableObjectName)
open import Ffi.Hs.Data.StateVar   using (StateVar)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput using (CanBeLabeled)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.VertexArrayObjects
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput (AgdaCanBeLabeled(AgdaCanBeLabeled))
import MAlonzo.Code.Ffi.Hs.Data.ObjectName
    (AgdaObjectName(AgdaObjectName), AgdaGeneratableObjectName(AgdaGeneratableObjectName))
#-}

postulate
    VertexArrayObject : Set

    Eq[VertexArrayObject]                    : Eq VertexArrayObject
    Ord[VertexArrayObject]                   : Ord VertexArrayObject
    Show[VertexArrayObject]                  : Show VertexArrayObject
    ObjectName[VertexArrayObject]            : ObjectName VertexArrayObject
    GeneratableObjectName[VertexArrayObject] : GeneratableObjectName VertexArrayObject
    CanBeLabeled[VertexArrayObject]          : CanBeLabeled VertexArrayObject

    bindVertexArrayObject : StateVar (Maybe VertexArrayObject)

{-# COMPILE GHC VertexArrayObject = type Graphics.Rendering.OpenGL.GL.VertexArrayObjects.VertexArrayObject #-}

{-# COMPILE GHC Eq[VertexArrayObject]                     = AgdaEq                     #-}
{-# COMPILE GHC Ord[VertexArrayObject]                    = AgdaOrd                    #-}
{-# COMPILE GHC Show[VertexArrayObject]                   = AgdaShow                   #-}
{-# COMPILE GHC ObjectName[VertexArrayObject]             = AgdaObjectName             #-}
{-# COMPILE GHC GeneratableObjectName[VertexArrayObject]  = AgdaGeneratableObjectName  #-}
{-# COMPILE GHC CanBeLabeled[VertexArrayObject]           = AgdaCanBeLabeled           #-}

{-# COMPILE GHC bindVertexArrayObject = Graphics.Rendering.OpenGL.GL.VertexArrayObjects.bindVertexArrayObject #-}
