{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects where

open import Ffi.Hs.-base.Class     using (Eq; Ord; Show)
open import Ffi.Hs.Data.ObjectName using (ObjectName; GeneratableObjectName)
open import Ffi.Hs.Data.StateVar   using (StateVar; GettableStateVar)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput using (CanBeLabeled)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput (AgdaCanBeLabeled(AgdaCanBeLabeled))
import MAlonzo.Code.Ffi.Hs.Data.ObjectName (AgdaObjectName(AgdaObjectName), AgdaGeneratableObjectName(AgdaGeneratableObjectName))
#-}

postulate
    FramebufferObject : Set

    Eq[FramebufferObject]                    : Eq FramebufferObject
    Ord[FramebufferObject]                   : Ord FramebufferObject
    Show[FramebufferObject]                  : Show FramebufferObject
    ObjectName[FramebufferObject]            : ObjectName FramebufferObject
    GeneratableObjectName[FramebufferObject] : GeneratableObjectName FramebufferObject
    CanBeLabeled[FramebufferObject]          : CanBeLabeled FramebufferObject

    defaultFramebufferObject : FramebufferObject

{-# COMPILE GHC FramebufferObject = type Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.FramebufferObject #-}

{-# COMPILE GHC Eq[FramebufferObject]                    = AgdaEq                    #-}
{-# COMPILE GHC Ord[FramebufferObject]                   = AgdaOrd                   #-}
{-# COMPILE GHC Show[FramebufferObject]                  = AgdaShow                  #-}
{-# COMPILE GHC ObjectName[FramebufferObject]            = AgdaObjectName            #-}
{-# COMPILE GHC GeneratableObjectName[FramebufferObject] = AgdaGeneratableObjectName #-}
{-# COMPILE GHC CanBeLabeled[FramebufferObject]          = AgdaCanBeLabeled          #-}

{-# COMPILE GHC defaultFramebufferObject = Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.defaultFramebufferObject #-}


data FramebufferTarget : Set where
    DrawFramebuffer : FramebufferTarget
    ReadFramebuffer : FramebufferTarget
    Framebuffer     : FramebufferTarget

{-# COMPILE GHC FramebufferTarget = data Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.FramebufferTarget
    ( Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.DrawFramebuffer
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.ReadFramebuffer
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.Framebuffer
    ) #-}

postulate
    Eq[FramebufferTarget]   : Eq FramebufferTarget
    Ord[FramebufferTarget]  : Ord FramebufferTarget
    Show[FramebufferTarget] : Show FramebufferTarget

{-# COMPILE GHC Eq[FramebufferTarget]   = AgdaEq   #-}
{-# COMPILE GHC Ord[FramebufferTarget]  = AgdaOrd  #-}
{-# COMPILE GHC Show[FramebufferTarget] = AgdaShow #-}

postulate
    bindFramebuffer : FramebufferTarget → StateVar FramebufferObject

{-# COMPILE GHC bindFramebuffer = Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.bindFramebuffer #-}


data FramebufferStatus : Set where
    Complete                    : FramebufferStatus
    Undefined                   : FramebufferStatus
    IncompleteMissingAttachment : FramebufferStatus
    IncompleteDrawBuffer        : FramebufferStatus
    IncompleteReadBuffer        : FramebufferStatus
    IncompleteMultiSample       : FramebufferStatus
    Unsupported                 : FramebufferStatus

{-# COMPILE GHC FramebufferStatus = data Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.FramebufferStatus
    ( Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.Complete
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.Undefined
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.IncompleteMissingAttachment
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.IncompleteDrawBuffer
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.IncompleteReadBuffer
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.IncompleteMultiSample
    | Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.Unsupported
    ) #-}

postulate
    Eq[FramebufferStatus]   : Eq FramebufferStatus
    Ord[FramebufferStatus]  : Ord FramebufferStatus
    Show[FramebufferStatus] : Show FramebufferStatus

{-# COMPILE GHC Eq[FramebufferStatus]   = AgdaEq   #-}
{-# COMPILE GHC Ord[FramebufferStatus]  = AgdaOrd  #-}
{-# COMPILE GHC Show[FramebufferStatus] = AgdaShow #-}

postulate
    framebufferStatus : FramebufferTarget → GettableStateVar FramebufferStatus

{-# COMPILE GHC framebufferStatus = Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferObjects.framebufferStatus #-}
