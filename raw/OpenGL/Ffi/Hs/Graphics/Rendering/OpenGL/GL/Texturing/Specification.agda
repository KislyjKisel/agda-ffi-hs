{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification where

open import Ffi.Hs.Graphics.GL.Types using (GLint)
open import Ffi.Hs.-base.Class using (Eq; Ord; Show)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.IO using (IO)
open import Agda.Primitive using ()

private
    variable
        aℓ : Agda.Primitive.Level
        A T : Set aℓ


data TextureTarget1D : Set where
    Texture1D : TextureTarget1D

postulate
    Eq[TextureTarget1D]   : Eq TextureTarget1D
    Ord[TextureTarget1D]  : Ord TextureTarget1D
    Show[TextureTarget1D] : Show TextureTarget1D

{-# COMPILE GHC Eq[TextureTarget1D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget1D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget1D] = AgdaShow #-}


data TextureTarget2D : Set where
    Texture2D        : TextureTarget2D
    Texture1DArray   : TextureTarget2D
    TextureRectangle : TextureTarget2D

postulate
    Eq[TextureTarget2D]   : Eq TextureTarget2D
    Ord[TextureTarget2D]  : Ord TextureTarget2D
    Show[TextureTarget2D] : Show TextureTarget2D

{-# COMPILE GHC Eq[TextureTarget2D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget2D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget2D] = AgdaShow #-}


data TextureTarget2DMultisample : Set where
    Texture2DMultisample : TextureTarget2DMultisample

postulate
    Eq[TextureTarget2DMultisample]   : Eq TextureTarget2DMultisample
    Ord[TextureTarget2DMultisample]  : Ord TextureTarget2DMultisample
    Show[TextureTarget2DMultisample] : Show TextureTarget2DMultisample

{-# COMPILE GHC Eq[TextureTarget2DMultisample]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget2DMultisample]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget2DMultisample] = AgdaShow #-}


data TextureTargetCubeMap : Set where
    TextureCubeMap : TextureTargetCubeMap

postulate
    Eq[TextureTargetCubeMap]   : Eq TextureTargetCubeMap
    Ord[TextureTargetCubeMap]  : Ord TextureTargetCubeMap
    Show[TextureTargetCubeMap] : Show TextureTargetCubeMap

{-# COMPILE GHC Eq[TextureTargetCubeMap]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTargetCubeMap]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTargetCubeMap] = AgdaShow #-}


data TextureTargetCubeMapFace : Set where
    TextureCubeMapPositiveX : TextureTargetCubeMapFace
    TextureCubeMapNegativeX : TextureTargetCubeMapFace
    TextureCubeMapPositiveY : TextureTargetCubeMapFace
    TextureCubeMapNegativeY : TextureTargetCubeMapFace
    TextureCubeMapPositiveZ : TextureTargetCubeMapFace
    TextureCubeMapNegativeZ : TextureTargetCubeMapFace

postulate
    Eq[TextureTargetCubeMapFace]   : Eq TextureTargetCubeMapFace
    Ord[TextureTargetCubeMapFace]  : Ord TextureTargetCubeMapFace
    Show[TextureTargetCubeMapFace] : Show TextureTargetCubeMapFace

{-# COMPILE GHC Eq[TextureTargetCubeMapFace]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTargetCubeMapFace]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTargetCubeMapFace] = AgdaShow #-}


data TextureTarget3D : Set where
    Texture3D           : TextureTarget3D
    Texture2DArray      : TextureTarget3D
    TextureCubeMapArray : TextureTarget3D

postulate
    Eq[TextureTarget3D]   : Eq TextureTarget3D
    Ord[TextureTarget3D]  : Ord TextureTarget3D
    Show[TextureTarget3D] : Show TextureTarget3D

{-# COMPILE GHC Eq[TextureTarget3D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget3D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget3D] = AgdaShow #-}


data TextureTarget2DMultisampleArray : Set where
    Texture2DMultisampleArray : TextureTarget2DMultisampleArray

postulate
    Eq[TextureTarget2DMultisampleArray]   : Eq TextureTarget2DMultisampleArray
    Ord[TextureTarget2DMultisampleArray]  : Ord TextureTarget2DMultisampleArray
    Show[TextureTarget2DMultisampleArray] : Show TextureTarget2DMultisampleArray

{-# COMPILE GHC Eq[TextureTarget2DMultisampleArray]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget2DMultisampleArray]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget2DMultisampleArray] = AgdaShow #-}


data TextureTargetBuffer : Set where
    TextureBuffer' : TextureTargetBuffer

postulate
    Eq[TextureTargetBuffer]   : Eq TextureTargetBuffer
    Ord[TextureTargetBuffer]  : Ord TextureTargetBuffer
    Show[TextureTargetBuffer] : Show TextureTargetBuffer

{-# COMPILE GHC Eq[TextureTargetBuffer]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTargetBuffer]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTargetBuffer] = AgdaShow #-}


postulate
    BindableTextureTarget : Set → Set

    BindableTextureTarget[TextureTargetBuffer]             : BindableTextureTarget TextureTargetBuffer
    BindableTextureTarget[TextureTarget2DMultisampleArray] : BindableTextureTarget TextureTarget2DMultisampleArray
    BindableTextureTarget[TextureTarget3D]                 : BindableTextureTarget TextureTarget3D
    BindableTextureTarget[TextureTargetCubeMap]            : BindableTextureTarget TextureTargetCubeMap
    BindableTextureTarget[TextureTarget2DMultisample]      : BindableTextureTarget TextureTarget2DMultisample
    BindableTextureTarget[TextureTarget2D]                 : BindableTextureTarget TextureTarget2D
    BindableTextureTarget[TextureTarget1D]                 : BindableTextureTarget TextureTarget1D


postulate
    ParameterizedTextureTarget : Set → Set

    ParameterizedTextureTarget[TextureTarget2DMultisampleArray] : ParameterizedTextureTarget TextureTarget2DMultisampleArray
    ParameterizedTextureTarget[TextureTarget3D]                 : ParameterizedTextureTarget TextureTarget3D
    ParameterizedTextureTarget[TextureTargetCubeMap]            : ParameterizedTextureTarget TextureTargetCubeMap
    ParameterizedTextureTarget[TextureTarget2DMultisample]      : ParameterizedTextureTarget TextureTarget2DMultisample
    ParameterizedTextureTarget[TextureTarget2D]                 : ParameterizedTextureTarget TextureTarget2D
    ParameterizedTextureTarget[TextureTarget1D]                 : ParameterizedTextureTarget TextureTarget1D


postulate
    OneDimensionalTextureTarget : Set → Set

    OneDimensionalTextureTarget[TextureTarget1D] : OneDimensionalTextureTarget TextureTarget1D


postulate
    TwoDimensionalTextureTarget : Set → Set

    TwoDimensionalTextureTarget[TextureTargetCubeMapFace] : TwoDimensionalTextureTarget TextureTargetCubeMapFace
    TwoDimensionalTextureTarget[TextureTargetCubeMap]     : TwoDimensionalTextureTarget TextureTargetCubeMap
    TwoDimensionalTextureTarget[TextureTarget2D]          : TwoDimensionalTextureTarget TextureTarget2D


postulate
    ThreeDimensionalTextureTarget : Set → Set

    ThreeDimensionalTextureTarget[TextureTarget3D] : ThreeDimensionalTextureTarget TextureTarget3D


postulate
    QueryableTextureTarget : Set → Set

    QueryableTextureTarget[TextureTarget2DMultisampleArray] : QueryableTextureTarget TextureTarget2DMultisampleArray
    QueryableTextureTarget[TextureTarget3D]                 : QueryableTextureTarget TextureTarget3D
    QueryableTextureTarget[TextureTargetCubeMapFace]        : QueryableTextureTarget TextureTargetCubeMapFace
    QueryableTextureTarget[TextureTarget2DMultisample]      : QueryableTextureTarget TextureTarget2DMultisample
    QueryableTextureTarget[TextureTarget2D]                 : QueryableTextureTarget TextureTarget2D
    QueryableTextureTarget[TextureTarget1D]                 : QueryableTextureTarget TextureTarget1D


postulate
    GettableTextureTarget : Set → Set

    GettableTextureTarget[TextureTarget3D]          : GettableTextureTarget TextureTarget3D
    GettableTextureTarget[TextureTargetCubeMapFace] : GettableTextureTarget TextureTargetCubeMapFace
    GettableTextureTarget[TextureTarget2D]          : GettableTextureTarget TextureTarget2D
    GettableTextureTarget[TextureTarget1D]          : GettableTextureTarget TextureTarget1D


Level : Set
Level = GLint

Border : Set
Border = GLint


data TexturePosition1D : Set where
    mkTexturePosition1D : GLint → TexturePosition1D

postulate
    Eq[TexturePosition1D]   : Eq TexturePosition1D
    Ord[TexturePosition1D]  : Ord TexturePosition1D
    Show[TexturePosition1D] : Show TexturePosition1D

{-# COMPILE GHC Eq[TexturePosition1D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TexturePosition1D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TexturePosition1D] = AgdaShow #-}


data TexturePosition2D : Set where
    mkTexturePosition2D : GLint → GLint → TexturePosition2D

postulate
    Eq[TexturePosition2D]   : Eq TexturePosition2D
    Ord[TexturePosition2D]  : Ord TexturePosition2D
    Show[TexturePosition2D] : Show TexturePosition2D

{-# COMPILE GHC Eq[TexturePosition2D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TexturePosition2D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TexturePosition2D] = AgdaShow #-}


data TexturePosition3D : Set where
    mkTexturePosition3D : GLint → GLint → GLint → TexturePosition3D

postulate
    Eq[TexturePosition3D]   : Eq TexturePosition3D
    Ord[TexturePosition3D]  : Ord TexturePosition3D
    Show[TexturePosition3D] : Show TexturePosition3D

{-# COMPILE GHC Eq[TexturePosition3D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TexturePosition3D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TexturePosition3D] = AgdaShow #-}


data TextureSize1D : Set where
    mkTextureSize1D : GLint → TextureSize1D

postulate
    Eq[TextureSize1D]   : Eq TextureSize1D
    Ord[TextureSize1D]  : Ord TextureSize1D
    Show[TextureSize1D] : Show TextureSize1D

{-# COMPILE GHC Eq[TextureSize1D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureSize1D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureSize1D] = AgdaShow #-}


data TextureSize2D : Set where
    mkTextureSize2D : GLint → GLint → TextureSize2D

postulate
    Eq[TextureSize2D]   : Eq TextureSize2D
    Ord[TextureSize2D]  : Ord TextureSize2D
    Show[TextureSize2D] : Show TextureSize2D

{-# COMPILE GHC Eq[TextureSize2D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureSize2D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureSize2D] = AgdaShow #-}


data TextureSize3D : Set where
    mkTextureSize3D : GLint → GLint → GLint → TextureSize3D

postulate
    Eq[TextureSize3D]   : Eq TextureSize3D
    Ord[TextureSize3D]  : Ord TextureSize3D
    Show[TextureSize3D] : Show TextureSize3D

{-# COMPILE GHC Eq[TextureSize3D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureSize3D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureSize3D] = AgdaShow #-}


postulate
    texImage1D     : ⦃ OneDimensionalTextureTarget T ⦄ → T → Proxy → Level → PixelInternalFormat → TextureSize1D → Border → PixelData A → IO ⊤
    texImage2D     : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Proxy → Level → PixelInternalFormat → TextureSize2D → Border → PixelData A → IO ⊤
    texImage3D     : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Proxy → Level → PixelInternalFormat → TextureSize3D → Border → PixelData A → IO ⊤
    copyTexImage1D : ⦃ OneDimensionalTextureTarget T ⦄ → T → Level → PixelInternalFormat → Position → TextureSize1D → Border → IO ⊤
    copyTexImage2D : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Level → PixelInternalFormat → Position → TextureSize2D → Border → IO ⊤
    texSubImage1D  : ⦃ OneDimensionalTextureTarget T ⦄ → T → Level → TexturePosition1D → TextureSize1D → PixelData A → IO ⊤
    texSubImage2D  : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Level → TexturePosition2D → TextureSize2D → PixelData A → IO ⊤
    texSubImage3D  : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Level → TexturePosition3D → TextureSize3D → PixelData A → IO ⊤
    getTexImage    : ⦃ GettableTextureTarget T ⦄ → T → Level → PixelData A → IO ⊤

    copyTexSubImage1D : ⦃ OneDimensionalTextureTarget T ⦄ → T → Level → TexturePosition1D → Position → TextureSize1D → IO ⊤
    copyTexSubImage2D : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Level → TexturePosition2D → Position → TextureSize2D → IO ⊤
    copyTexSubImage3D : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Level → TexturePosition3D → Position → TextureSize2D → IO ⊤


data CompressedTextureFormat : Set where
    mkCompressedTextureFormat : GLenum → CompressedTextureFormat

postulate
    Eq[CompressedTextureFormat]   : Eq CompressedTextureFormat
    Ord[CompressedTextureFormat]  : Ord CompressedTextureFormat
    Show[CompressedTextureFormat] : Show CompressedTextureFormat

{-# COMPILE GHC Eq[CompressedTextureFormat]   = AgdaEq   #-}
{-# COMPILE GHC Ord[CompressedTextureFormat]  = AgdaOrd  #-}
{-# COMPILE GHC Show[CompressedTextureFormat] = AgdaShow #-}

postulate
    compressedTextureFormats : GettableStateVar (List CompressedTextureFormat)


data CompressedPixelData : Set where
    mkCompressedPixelData : CompressedTextureFormat → GLsizei → Ptr A → CompressedPixelData

postulate
    Eq[CompressedPixelData]   : Eq CompressedPixelData
    Ord[CompressedPixelData]  : Ord CompressedPixelData
    Show[CompressedPixelData] : Show CompressedPixelData

{-# COMPILE GHC Eq[CompressedPixelData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[CompressedPixelData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[CompressedPixelData] = AgdaShow #-}


postulate
    compressedTexImage1D    : ⦃ OneDimensionalTextureTarget T ⦄ → T → Proxy → Level → TextureSize1D → Border → CompressedPixelData A → IO ⊤
    compressedTexImage2D    : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Proxy → Level → TextureSize2D → Border → CompressedPixelData A → IO ⊤
    compressedTexImage3D    : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Proxy → Level → TextureSize3D → Border → CompressedPixelData A → IO ⊤
    compressedTexSubImage1D : ⦃ OneDimensionalTextureTarget T ⦄ → T → Level → TexturePosition1D → TextureSize1D → CompressedPixelData A → IO ⊤
    compressedTexSubImage2D : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Level → TexturePosition2D → TextureSize2D → CompressedPixelData A → IO ⊤
    compressedTexSubImage3D : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Level → TexturePosition3D → TextureSize3D → CompressedPixelData A → IO ⊤
    getCompressedTexImage   : ⦃ GettableTextureTarget T ⦄ → T → Level → Ptr a → IO ⊤


data SampleLocations : Set where
    FlexibleSampleLocations : SampleLocations
    FixedSampleLocations    : SampleLocations

postulate
    Eq[SampleLocations]   : Eq SampleLocations
    Ord[SampleLocations]  : Ord SampleLocations
    Show[SampleLocations] : Show SampleLocations

{-# COMPILE GHC Eq[SampleLocations]   = AgdaEq   #-}
{-# COMPILE GHC Ord[SampleLocations]  = AgdaOrd  #-}
{-# COMPILE GHC Show[SampleLocations] = AgdaShow #-}

postulate
    texImage2DMultisample : TextureTarget2DMultisample → Proxy → Samples → PixelInternalFormat → TextureSize2D → SampleLocations → IO ⊤
    texImage3DMultisample : TextureTarget2DMultisampleArray → Proxy → Samples → PixelInternalFormat → TextureSize3D → SampleLocations → IO ⊤


postulate
    maxTextureSize          : GettableStateVar GLsizei
    maxCubeMapTextureSize   : GettableStateVar GLsizei
    maxRectangleTextureSize : GettableStateVar GLsizei
    max3DTextureSize        : GettableStateVar GLsizei
    maxArrayTextureLayers   : GettableStateVar GLsizei
    maxSampleMaskWords      : GettableStateVar GLsizei
    maxColorTextureSamples  : GettableStateVar GLsizei
    maxDepthTextureSamples  : GettableStateVar GLsizei
    maxIntegerSamples       : GettableStateVar GLsizei

