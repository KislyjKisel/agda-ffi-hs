{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable where

open import Agda.Builtin.Unit    using (⊤)
open import Agda.Primitive
open import Ffi.Hs.-base.Class   using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar using (StateVar)
open import Ffi.Hs.Graphics.GL.Types using (GLsizei)
open import Agda.Builtin.IO using (IO)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization using (PixelData)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays using (Capability)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


data ColorTableStage : Set where
    mkColorTableStage              : ColorTableStage
    PostConvolutionColorTableStage : ColorTableStage
    PostColorMatrixColorTableStage : ColorTableStage
    TextureColorTableStage         : ColorTableStage

{-# COMPILE GHC ColorTableStage = data Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.ColorTableStage
    ( Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.ColorTableStage
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.PostConvolutionColorTableStage
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.PostColorMatrixColorTableStage
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.TextureColorTableStage
    ) #-}

postulate
    Eq[ColorTableStage]   : Eq ColorTableStage
    Ord[ColorTableStage]  : Ord ColorTableStage
    Show[ColorTableStage] : Show ColorTableStage

{-# COMPILE GHC Eq[ColorTableStage]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ColorTableStage]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ColorTableStage] = AgdaShow #-}

postulate
    colorTableStage : ColorTableStage → StateVar Capability

{-# COMPILE GHC colorTableStage = Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorTableStage #-}


data Proxy : Set where
    NoProxy : Proxy
    mkProxy : Proxy

{-# COMPILE GHC Proxy = data Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Proxy
    ( Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.NoProxy
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Proxy
    ) #-}

postulate
    Eq[Proxy]   : Eq Proxy
    Ord[Proxy]  : Ord Proxy
    Show[Proxy] : Show Proxy

{-# COMPILE GHC Eq[Proxy]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Proxy]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Proxy] = AgdaShow #-}


data ColorTable : Set where
    mkColorTable              : ColorTable
    PostConvolutionColorTable : ColorTable
    PostColorMatrixColorTable : ColorTable
    Texture1DColorTable       : ColorTable
    Texture2DColorTable       : ColorTable
    Texture3DColorTable       : ColorTable
    TextureCubeMapColorTable  : ColorTable
    TextureColorTable         : ColorTable
    SharedTexturePalette      : ColorTable

{-# COMPILE GHC ColorTable = data Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.ColorTable
    ( Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.mkColorTable
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.PostConvolutionColorTable
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.PostColorMatrixColorTable
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Texture1DColorTable
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Texture2DColorTable
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Texture3DColorTable
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.TextureCubeMapColorTable
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.TextureColorTable
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.SharedTexturePalette
    ) #-}

postulate
    Eq[ColorTable]   : Eq ColorTable
    Ord[ColorTable]  : Ord ColorTable
    Show[ColorTable] : Show ColorTable

{-# COMPILE GHC Eq[ColorTable]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ColorTable]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ColorTable] = AgdaShow #-}


data PixelInternalFormat : Set where
    Alpha'                    : PixelInternalFormat
    DepthComponent'           : PixelInternalFormat
    Luminance'                : PixelInternalFormat
    LuminanceAlpha'           : PixelInternalFormat
    Intensity                 : PixelInternalFormat
    R8                        : PixelInternalFormat
    R16                       : PixelInternalFormat
    RG8                       : PixelInternalFormat
    RG16                      : PixelInternalFormat
    RGB'                      : PixelInternalFormat
    RGBA'                     : PixelInternalFormat
    SRGB                      : PixelInternalFormat
    SRGBAlpha                 : PixelInternalFormat
    SLuminance                : PixelInternalFormat
    SLuminanceAlpha           : PixelInternalFormat
    Alpha4                    : PixelInternalFormat
    Alpha8                    : PixelInternalFormat
    Alpha12                   : PixelInternalFormat
    Alpha16                   : PixelInternalFormat
    DepthComponent16          : PixelInternalFormat
    DepthComponent24          : PixelInternalFormat
    DepthComponent32          : PixelInternalFormat
    Luminance4                : PixelInternalFormat
    Luminance8                : PixelInternalFormat
    Luminance12               : PixelInternalFormat
    Luminance16               : PixelInternalFormat
    Luminance4Alpha4          : PixelInternalFormat
    Luminance6Alpha2          : PixelInternalFormat
    Luminance8Alpha8          : PixelInternalFormat
    Luminance12Alpha4         : PixelInternalFormat
    Luminance12Alpha12        : PixelInternalFormat
    Luminance16Alpha16        : PixelInternalFormat
    Intensity4                : PixelInternalFormat
    Intensity8                : PixelInternalFormat
    Intensity12               : PixelInternalFormat
    Intensity16               : PixelInternalFormat
    R3G3B2                    : PixelInternalFormat
    RGB4                      : PixelInternalFormat
    RGB5                      : PixelInternalFormat
    RGB8                      : PixelInternalFormat
    RGB10                     : PixelInternalFormat
    RGB12                     : PixelInternalFormat
    RGB16                     : PixelInternalFormat
    RGBA2                     : PixelInternalFormat
    RGBA4                     : PixelInternalFormat
    RGB5A1                    : PixelInternalFormat
    RGBA8                     : PixelInternalFormat
    RGB10A2                   : PixelInternalFormat
    RGBA12                    : PixelInternalFormat
    RGBA16                    : PixelInternalFormat
    SRGB8                     : PixelInternalFormat
    SRGB8Alpha8               : PixelInternalFormat
    R16F                      : PixelInternalFormat
    RG16F                     : PixelInternalFormat
    RGB16F                    : PixelInternalFormat
    RGBA16F                   : PixelInternalFormat
    R32F                      : PixelInternalFormat
    RG32F                     : PixelInternalFormat
    RGB32F                    : PixelInternalFormat
    RGBA32F                   : PixelInternalFormat
    R8I                       : PixelInternalFormat
    R8UI                      : PixelInternalFormat
    R16I                      : PixelInternalFormat
    R16UI                     : PixelInternalFormat
    R32I                      : PixelInternalFormat
    R32UI                     : PixelInternalFormat
    RG8I                      : PixelInternalFormat
    RG8UI                     : PixelInternalFormat
    RG16I                     : PixelInternalFormat
    RG16UI                    : PixelInternalFormat
    RG32I                     : PixelInternalFormat
    RG32UI                    : PixelInternalFormat
    RGB8I                     : PixelInternalFormat
    RGB8UI                    : PixelInternalFormat
    RGB16I                    : PixelInternalFormat
    RGB16UI                   : PixelInternalFormat
    RGB32I                    : PixelInternalFormat
    RGB32UI                   : PixelInternalFormat
    RGBA8I                    : PixelInternalFormat
    RGBA8UI                   : PixelInternalFormat
    RGBA16I                   : PixelInternalFormat
    RGBA16UI                  : PixelInternalFormat
    RGBA32I                   : PixelInternalFormat
    RGBA32UI                  : PixelInternalFormat
    SLuminance8               : PixelInternalFormat
    SLuminance8Alpha8         : PixelInternalFormat
    CompressedAlpha           : PixelInternalFormat
    CompressedLuminance       : PixelInternalFormat
    CompressedLuminanceAlpha  : PixelInternalFormat
    CompressedIntensity       : PixelInternalFormat
    CompressedRed             : PixelInternalFormat
    CompressedRG              : PixelInternalFormat
    CompressedRGB             : PixelInternalFormat
    CompressedRGBA            : PixelInternalFormat
    CompressedSRGB            : PixelInternalFormat
    CompressedSRGBAlpha       : PixelInternalFormat
    CompressedSLuminance      : PixelInternalFormat
    CompressedSLuminanceAlpha : PixelInternalFormat
    CompressedRedRGTC1        : PixelInternalFormat
    CompressedSignedRedRGTC1  : PixelInternalFormat
    CompressedRG-RGTC2        : PixelInternalFormat
    CompressedSignedRG-RGTC2  : PixelInternalFormat
    DepthComponent32f         : PixelInternalFormat
    Depth32fStencil8          : PixelInternalFormat
    RGB9E5                    : PixelInternalFormat
    R11fG11fB10f              : PixelInternalFormat
    StencilIndex1             : PixelInternalFormat
    StencilIndex4             : PixelInternalFormat
    StencilIndex8             : PixelInternalFormat
    StencilIndex16            : PixelInternalFormat
    RGBS3TC                   : PixelInternalFormat
    RGB4S3TC                  : PixelInternalFormat
    RGBAS3TC                  : PixelInternalFormat
    RGBA4S3TC                 : PixelInternalFormat
    RGBADXT5S3TC              : PixelInternalFormat
    RGBA4DXT5S3TC             : PixelInternalFormat
    CompressedRGBAS3TCDXT1    : PixelInternalFormat
    CompressedRGBAS3TCDXT3    : PixelInternalFormat
    CompressedRGBAS3TCDXT5    : PixelInternalFormat
    CompressedRGBS3TCDXT1     : PixelInternalFormat
    Alpha32F                  : PixelInternalFormat
    Intensity32F              : PixelInternalFormat
    Luminance32F              : PixelInternalFormat
    LuminanceAlpha32F         : PixelInternalFormat
    Alpha16F                  : PixelInternalFormat
    Intensity16F              : PixelInternalFormat
    Luminance16F              : PixelInternalFormat
    LuminanceAlpha16F         : PixelInternalFormat
    Depth24Stencil8           : PixelInternalFormat

{-# COMPILE GHC PixelInternalFormat = data Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.PixelInternalFormat
    ( Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Alpha'
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.DepthComponent'
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance'
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.LuminanceAlpha'
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Intensity
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB'
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA'
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.SRGB
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.SRGBAlpha
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.SLuminance
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.SLuminanceAlpha
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Alpha4
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Alpha8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Alpha12
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Alpha16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.DepthComponent16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.DepthComponent24
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.DepthComponent32
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance4
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance12
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance4Alpha4
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance6Alpha2
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance8Alpha8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance12Alpha4
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance12Alpha12
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance16Alpha16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Intensity4
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Intensity8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Intensity12
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Intensity16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R3G3B2
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB4
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB5
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB10
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB12
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA2
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA4
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB5A1
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB10A2
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA12
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.SRGB8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.SRGB8Alpha8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R16F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG16F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB16F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA16F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R32F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG32F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB32F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA32F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R8I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R8UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R16I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R16UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R32I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R32UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG8I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG8UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG16I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG16UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG32I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RG32UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB8I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB8UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB16I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB16UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB32I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB32UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA8I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA8UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA16I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA16UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA32I
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA32UI
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.SLuminance8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.SLuminance8Alpha8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedAlpha
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedLuminance
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedLuminanceAlpha
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedIntensity
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRed
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRG
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRGB
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRGBA
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedSRGB
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedSRGBAlpha
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedSLuminance
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedSLuminanceAlpha
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRedRGTC1
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedSignedRedRGTC1
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRG_RGTC2
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedSignedRG_RGTC2
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.DepthComponent32f
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Depth32fStencil8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB9E5
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.R11fG11fB10f
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.StencilIndex1
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.StencilIndex4
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.StencilIndex8
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.StencilIndex16
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBS3TC
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGB4S3TC
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBAS3TC
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA4S3TC
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBADXT5S3TC
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.RGBA4DXT5S3TC
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRGBAS3TCDXT1
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRGBAS3TCDXT3
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRGBAS3TCDXT5
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.CompressedRGBS3TCDXT1
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Alpha32F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Intensity32F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance32F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.LuminanceAlpha32F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Alpha16F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Intensity16F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Luminance16F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.LuminanceAlpha16F
    | Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.Depth24Stencil8
    ) #-}

postulate
    Eq[PixelInternalFormat]   : Eq PixelInternalFormat
    Ord[PixelInternalFormat]  : Ord PixelInternalFormat
    Show[PixelInternalFormat] : Show PixelInternalFormat

{-# COMPILE GHC Eq[PixelInternalFormat]   = AgdaEq   #-}
{-# COMPILE GHC Ord[PixelInternalFormat]  = AgdaOrd  #-}
{-# COMPILE GHC Show[PixelInternalFormat] = AgdaShow #-}


postulate
    colorTable              : Proxy → ColorTable → PixelInternalFormat → GLsizei → PixelData A → IO ⊤
    getColorTable           : ColorTable → PixelData A → IO ⊤
    copyColorTable          : ColorTable → PixelInternalFormat → Position → GLsizei → IO ⊤
    colorSubTable           : ColorTable → GLsizei → GLsizei → PixelData A → IO ⊤
    copyColorSubTable       : ColorTable → GLsizei → Position → GLsizei → IO ⊤
    colorTableScale         : ColorTableStage → StateVar (Color4 GLfloat)
    colorTableBias          : ColorTableStage → StateVar (Color4 GLfloat)
    colorTableFormat        : ColorTable → GettableStateVar PixelInternalFormat
    colorTableWidth         : ColorTable → GettableStateVar GLsizei
    colorTableRGBASizes     : ColorTable → GettableStateVar (Color4 GLsizei)
    colorTableLuminanceSize : ColorTable → GettableStateVar GLsizei
    colorTableIntesitySize  : ColorTable → GettableStateVar GLsizei

{-# COMPILE GHC colorTable              = \ aℓ a -> Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorTable              #-}
{-# COMPILE GHC getColorTable           = \ aℓ a -> Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.getColorTable           #-}
{-# COMPILE GHC copyColorTable          =           Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.copyColorTable          #-}
{-# COMPILE GHC colorSubTable           = \ aℓ a -> Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorSubTable           #-}
{-# COMPILE GHC copyColorSubTable       =           Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.copyColorSubTable       #-}
{-# COMPILE GHC colorTableScale         =           Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorTableScale         #-}
{-# COMPILE GHC colorTableBias          =           Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorTableBias          #-}
{-# COMPILE GHC colorTableFormat        =           Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorTableFormat        #-}
{-# COMPILE GHC colorTableWidth         =           Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorTableWidth         #-}
{-# COMPILE GHC colorTableRGBASizes     =           Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorTableRGBASizes     #-}
{-# COMPILE GHC colorTableLuminanceSize =           Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorTableLuminanceSize #-}
{-# COMPILE GHC colorTableIntesitySize  =           Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable.colorTableIntesitySize  #-}
