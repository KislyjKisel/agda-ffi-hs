{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL where

open import Ffi.Hs.Graphics.GL.Types                        public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.FlushFinish public
open import Ffi.Hs.Data.ObjectName                          public

-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.SyncObjects  public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.QueryObjects public

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PrimitiveMode public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.BeginEnd public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Rectangles public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.ConditionalRendering public

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrayObjects public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.BufferObjects public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.CoordTrans public hiding (Color)
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Clipping public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.RasterPos public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Colors public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders public

-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Antialiasing public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Points public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.LineSegments public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Polygons public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PixelRectangles public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Bitmaps public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.ColorSum public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Fog public

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PerFragment public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Framebuffer public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.ReadCopyPixels public

-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Evaluators public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Selection public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Feedback public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.DisplayLists public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Hints public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PixellikeObject public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.TransformFeedback public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput public

open import Ffi.Hs.Data.StateVar public
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Tensor public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.StringQueries public
-- open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.SavingState public
