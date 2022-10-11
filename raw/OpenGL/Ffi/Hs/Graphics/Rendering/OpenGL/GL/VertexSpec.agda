{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec where

open import Ffi.Hs.-base.Class       using (Eq; Ord; Show; Storable)
open import Ffi.Hs.Data.StateVar     using (GettableStateVar)
open import Ffi.Hs.Graphics.GL.Types using (GLuint)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.VertexSpec
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

-- todo: Graphics.Rendering.OpenGL.GL.VertexSpec
-- ...

data IntegerHandling : Set where
    ToFloat           : IntegerHandling
    ToNormalizedFloat : IntegerHandling
    KeepIntegral      : IntegerHandling

{-# COMPILE GHC IntegerHandling = data Graphics.Rendering.OpenGL.GL.VertexSpec.IntegerHandling 
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.ToFloat
    | Graphics.Rendering.OpenGL.GL.VertexSpec.ToNormalizedFloat
    | Graphics.Rendering.OpenGL.GL.VertexSpec.KeepIntegral
    ) #-}

postulate
    Eq[IntegerHandling]   : Eq IntegerHandling
    Ord[IntegerHandling]  : Ord IntegerHandling
    Show[IntegerHandling] : Show IntegerHandling

{-# COMPILE GHC Eq[IntegerHandling]   = AgdaEq   #-}
{-# COMPILE GHC Ord[IntegerHandling]  = AgdaOrd  #-}
{-# COMPILE GHC Show[IntegerHandling] = AgdaShow #-}


data AttribLocation : Set where
    mkAttribLocation : GLuint → AttribLocation

{-# COMPILE GHC AttribLocation = data Graphics.Rendering.OpenGL.GL.VertexSpec.AttribLocation (Graphics.Rendering.OpenGL.GL.VertexSpec.AttribLocation) #-}

postulate
    Eq[AttribLocation]   : Eq AttribLocation
    Ord[AttribLocation]  : Ord AttribLocation
    Show[AttribLocation] : Show AttribLocation

{-# COMPILE GHC Eq[AttribLocation]   = AgdaEq   #-}
{-# COMPILE GHC Ord[AttribLocation]  = AgdaOrd  #-}
{-# COMPILE GHC Show[AttribLocation] = AgdaShow #-}

-- postulate
    -- currentVertexAttrib   : AttribLocation → StateVar (Vertex4 GLfloat)
    -- currentVertexAttribI  : AttribLocation → StateVar (Vertex4 GLint)
    -- currentVertexAttribIu : AttribLocation → StateVar (Vertex4 GLuint)


-- ...

data TextureUnit : Set where
    mkTextureUnit : GLuint → TextureUnit

{-# COMPILE GHC TextureUnit = data Graphics.Rendering.OpenGL.GL.VertexSpec.TextureUnit (Graphics.Rendering.OpenGL.GL.VertexSpec.TextureUnit) #-}

postulate
    Eq[TextureUnit]       : Eq TextureUnit
    Ord[TextureUnit]      : Ord TextureUnit
    Show[TextureUnit]     : Show TextureUnit
    Storable[TextureUnit] : Storable TextureUnit

{-# COMPILE GHC Eq[TextureUnit]       = AgdaEq       #-}
{-# COMPILE GHC Ord[TextureUnit]      = AgdaOrd      #-}
{-# COMPILE GHC Show[TextureUnit]     = AgdaShow     #-}
{-# COMPILE GHC Storable[TextureUnit] = AgdaStorable #-}

postulate
    maxTextureUnit : GettableStateVar TextureUnit

{-# COMPILE GHC maxTextureUnit = Graphics.Rendering.OpenGL.GL.VertexSpec.maxTextureUnit #-}
