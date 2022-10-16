{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects where

open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.Unit        using (⊤)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.ObjectName   using (ObjectName; GeneratableObjectName)
open import Ffi.Hs.Data.StateVar     using (StateVar)
open import Ffi.Hs.Graphics.GL.Types using (GLsizei)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput                using (CanBeLabeled)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable using (PixelInternalFormat)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput (AgdaCanBeLabeled(AgdaCanBeLabeled))
import MAlonzo.Code.Ffi.Hs.Data.ObjectName (AgdaObjectName(AgdaObjectName), AgdaGeneratableObjectName(AgdaGeneratableObjectName))
#-}


postulate
    RenderbufferObject : Set

    Eq[RenderbufferObject]                    : Eq RenderbufferObject
    Ord[RenderbufferObject]                   : Ord RenderbufferObject
    Show[RenderbufferObject]                  : Show RenderbufferObject
    ObjectName[RenderbufferObject]            : ObjectName RenderbufferObject
    GeneratableObjectName[RenderbufferObject] : GeneratableObjectName RenderbufferObject
    CanBeLabeled[RenderbufferObject]          : CanBeLabeled RenderbufferObject

    noRenderbufferObject : RenderbufferObject

{-# COMPILE GHC RenderbufferObject = type Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.RenderbufferObject #-}

{-# COMPILE GHC Eq[RenderbufferObject]                    = AgdaEq                    #-}
{-# COMPILE GHC Ord[RenderbufferObject]                   = AgdaOrd                   #-}
{-# COMPILE GHC Show[RenderbufferObject]                  = AgdaShow                  #-}
{-# COMPILE GHC ObjectName[RenderbufferObject]            = AgdaObjectName            #-}
{-# COMPILE GHC GeneratableObjectName[RenderbufferObject] = AgdaGeneratableObjectName #-}
{-# COMPILE GHC CanBeLabeled[RenderbufferObject]          = AgdaCanBeLabeled          #-}

{-# COMPILE GHC noRenderbufferObject = Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.noRenderbufferObject #-}


data RenderbufferTarget : Set where
    Renderbuffer : RenderbufferTarget

{-# COMPILE GHC RenderbufferTarget = data Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.RenderbufferTarget
    ( Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.Renderbuffer
    ) #-}

postulate
    Eq[RenderbufferTarget]   : Eq RenderbufferTarget
    Ord[RenderbufferTarget]  : Ord RenderbufferTarget
    Show[RenderbufferTarget] : Show RenderbufferTarget

{-# COMPILE GHC Eq[RenderbufferTarget]   = AgdaEq   #-}
{-# COMPILE GHC Ord[RenderbufferTarget]  = AgdaOrd  #-}
{-# COMPILE GHC Show[RenderbufferTarget] = AgdaShow #-}


data RenderbufferSize : Set where
    mkRenderbufferSize : GLsizei → GLsizei → RenderbufferSize

{-# COMPILE GHC RenderbufferSize = data Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.RenderbufferSize
    ( Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.RenderbufferSize
    ) #-}

postulate
    Eq[RenderbufferSize]   : Eq RenderbufferSize
    Ord[RenderbufferSize]  : Ord RenderbufferSize
    Show[RenderbufferSize] : Show RenderbufferSize

{-# COMPILE GHC Eq[RenderbufferSize]   = AgdaEq   #-}
{-# COMPILE GHC Ord[RenderbufferSize]  = AgdaOrd  #-}
{-# COMPILE GHC Show[RenderbufferSize] = AgdaShow #-}


data Samples : Set where
    mkSamples : GLsizei → Samples

{-# COMPILE GHC Samples = data Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.Samples
    ( Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.Samples
    ) #-}

postulate
    Eq[Samples]   : Eq Samples
    Ord[Samples]  : Ord Samples
    Show[Samples] : Show Samples

{-# COMPILE GHC Eq[Samples]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Samples]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Samples] = AgdaShow #-}


postulate
    bindRenderbuffer               : RenderbufferTarget → StateVar RenderbufferObject
    renderbufferStorage            : RenderbufferTarget → PixelInternalFormat → RenderbufferSize → IO ⊤
    renderbufferStorageMultiSample : RenderbufferTarget → Samples → PixelInternalFormat → RenderbufferSize → IO ⊤

{-# COMPILE GHC bindRenderbuffer               = Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.bindRenderbuffer               #-}
{-# COMPILE GHC renderbufferStorage            = Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.renderbufferStorage            #-}
{-# COMPILE GHC renderbufferStorageMultiSample = Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects.renderbufferStorageMultiSample #-}
