{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.PrimitiveMode where

open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple2; Tuple4)
open import Ffi.Hs.Graphics.GL.Types using (GLfloat; GLsizei)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.PrimitiveMode
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}


data PrimitiveMode : Set where
    Points        : PrimitiveMode
    Lines         : PrimitiveMode
    LineLoop      : PrimitiveMode
    LineStrip     : PrimitiveMode
    Triangles     : PrimitiveMode
    TriangleStrip : PrimitiveMode
    TriangleFan   : PrimitiveMode
    Quads         : PrimitiveMode
    QuadStrip     : PrimitiveMode
    Polygon       : PrimitiveMode
    Patches       : PrimitiveMode

{-# COMPILE GHC PrimitiveMode = data Graphics.Rendering.OpenGL.GL.PrimitiveMode.PrimitiveMode
    ( Graphics.Rendering.OpenGL.GL.PrimitiveMode.Points
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.Lines
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.LineLoop
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.LineStrip
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.Triangles
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.TriangleStrip
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.TriangleFan
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.Quads
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.QuadStrip
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.Polygon
    | Graphics.Rendering.OpenGL.GL.PrimitiveMode.Patches
    ) #-}

postulate
    Eq[PrimitiveMode]   : Eq PrimitiveMode
    Ord[PrimitiveMode]  : Ord PrimitiveMode
    Show[PrimitiveMode] : Show PrimitiveMode

{-# COMPILE GHC Eq[PrimitiveMode]   = AgdaEq   #-}
{-# COMPILE GHC Ord[PrimitiveMode]  = AgdaOrd  #-}
{-# COMPILE GHC Show[PrimitiveMode] = AgdaShow #-}


postulate
    patchVertices          : StateVar GLsizei
    maxPatchVertices       : GettableStateVar GLsizei
    patchDefaultOuterLevel : StateVar (Tuple4 GLfloat GLfloat GLfloat GLfloat)
    patchDefaultInnerLevel : StateVar (Tuple2 GLfloat GLfloat)
    maxTessGenLevel        : GettableStateVar GLsizei

{-# COMPILE GHC patchVertices          = Graphics.Rendering.OpenGL.GL.PrimitiveMode.patchVertices          #-}
{-# COMPILE GHC maxPatchVertices       = Graphics.Rendering.OpenGL.GL.PrimitiveMode.maxPatchVertices       #-}
{-# COMPILE GHC patchDefaultOuterLevel = Graphics.Rendering.OpenGL.GL.PrimitiveMode.patchDefaultOuterLevel #-}
{-# COMPILE GHC patchDefaultInnerLevel = Graphics.Rendering.OpenGL.GL.PrimitiveMode.patchDefaultInnerLevel #-}
{-# COMPILE GHC maxTessGenLevel        = Graphics.Rendering.OpenGL.GL.PrimitiveMode.maxTessGenLevel        #-}
