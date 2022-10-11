{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization where

open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.Unit        using (⊤)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar     using (StateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple2)
open import Ffi.Hs.Foreign.Ptr       using (Ptr)
open import Ffi.Hs.Graphics.GL.Types using (GLfloat)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.CoordTrans   using (Size)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays using (DataType)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


data PixelFormat : Set where
    ColorIndex           : PixelFormat
    StencilIndex         : PixelFormat
    DepthComponent       : PixelFormat
    DepthStencil         : PixelFormat
    Red                  : PixelFormat
    Green                : PixelFormat
    Blue                 : PixelFormat
    Alpha                : PixelFormat
    RG                   : PixelFormat
    RGB                  : PixelFormat
    RGBA                 : PixelFormat
    Luminance            : PixelFormat
    LuminanceAlpha       : PixelFormat
    RedInteger           : PixelFormat
    GreenInteger         : PixelFormat
    BlueInteger          : PixelFormat
    AlphaInteger         : PixelFormat
    RGInteger            : PixelFormat
    RGBInteger           : PixelFormat
    RGBAInteger          : PixelFormat
    BGRInteger           : PixelFormat
    BGRAInteger          : PixelFormat
    ABGR                 : PixelFormat
    BGR                  : PixelFormat
    BGRA                 : PixelFormat
    CMYK                 : PixelFormat
    CMYKA                : PixelFormat
    FourTwoTwo           : PixelFormat
    FourTwoTwoRev        : PixelFormat
    FourTwoTwoAverage    : PixelFormat
    FourTwoTwoRevAverage : PixelFormat
    YCBCR422             : PixelFormat

{-# COMPILE GHC PixelFormat = data Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.PixelFormat
    ( Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.ColorIndex
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.StencilIndex
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.DepthComponent
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.DepthStencil
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.Red
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.Green
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.Blue
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.Alpha
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.RG
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.RGB
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.RGBA
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.Luminance
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.LuminanceAlpha
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.RedInteger
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.GreenInteger
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.BlueInteger
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.AlphaInteger
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.RGInteger
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.RGBInteger
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.RGBAInteger
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.BGRInteger
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.BGRAInteger
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.ABGR
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.BGR
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.BGRA
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.CMYK
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.CMYKA
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.FourTwoTwo
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.FourTwoTwoRev
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.FourTwoTwoAverage
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.FourTwoTwoRevAverage
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.YCBCR422
    ) #-}

postulate
    Eq[PixelFormat]   : Eq PixelFormat
    Ord[PixelFormat]  : Ord PixelFormat
    Show[PixelFormat] : Show PixelFormat

{-# COMPILE GHC Eq[PixelFormat]   = AgdaEq   #-}
{-# COMPILE GHC Ord[PixelFormat]  = AgdaOrd  #-}
{-# COMPILE GHC Show[PixelFormat] = AgdaShow #-}

postulate
    drawPixels : Size → PixelData A → IO ⊤
    pixelZoom  : StateVar (Tuple2 GLfloat GLfloat)

{-# COMPILE GHC drawPixels = \ aℓ a -> Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.drawPixels #-}
{-# COMPILE GHC pixelZoom  =           Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.pixelZoom  #-}


data PixelData (A : Set aℓ) : Set aℓ where
    mkPixelData : PixelFormat → DataType → Ptr A → PixelData A

{-# FOREIGN GHC type AgdaPixelData aℓ = Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.PixelData #-}
{-# COMPILE GHC PixelData = data(1) AgdaPixelData (Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization.PixelData) #-}

postulate
    Eq[PixelData[A]]   : Eq (PixelData A)
    Ord[PixelData[A]]  : Ord (PixelData A)
    Show[PixelData[A]] : Show (PixelData A)

{-# COMPILE GHC Eq[PixelData[A]]   = \ aℓ a -> AgdaEq   #-}
{-# COMPILE GHC Ord[PixelData[A]]  = \ aℓ a -> AgdaOrd  #-}
{-# COMPILE GHC Show[PixelData[A]] = \ aℓ a -> AgdaShow #-}
