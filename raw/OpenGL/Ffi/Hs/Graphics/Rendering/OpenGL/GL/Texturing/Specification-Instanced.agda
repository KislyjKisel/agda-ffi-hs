{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification-Instanced where

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification

instance
    inst:Eq[TextureTarget1D]   = Eq[TextureTarget1D]
    inst:Ord[TextureTarget1D]  = Ord[TextureTarget1D]
    inst:Show[TextureTarget1D] = Show[TextureTarget1D]

    inst:Eq[TextureTarget2D]   = Eq[TextureTarget2D]
    inst:Ord[TextureTarget2D]  = Ord[TextureTarget2D]
    inst:Show[TextureTarget2D] = Show[TextureTarget2D]

    inst:Eq[TextureTarget2DMultisample]   = Eq[TextureTarget2DMultisample]
    inst:Ord[TextureTarget2DMultisample]  = Ord[TextureTarget2DMultisample]
    inst:Show[TextureTarget2DMultisample] = Show[TextureTarget2DMultisample]

    inst:Eq[TextureTargetCubeMap]   = Eq[TextureTargetCubeMap]
    inst:Ord[TextureTargetCubeMap]  = Ord[TextureTargetCubeMap]
    inst:Show[TextureTargetCubeMap] = Show[TextureTargetCubeMap]

    inst:Eq[TextureTargetCubeMapFace]   = Eq[TextureTargetCubeMapFace]
    inst:Ord[TextureTargetCubeMapFace]  = Ord[TextureTargetCubeMapFace]
    inst:Show[TextureTargetCubeMapFace] = Show[TextureTargetCubeMapFace]

    inst:Eq[TextureTarget3D]   = Eq[TextureTarget3D]
    inst:Ord[TextureTarget3D]  = Ord[TextureTarget3D]
    inst:Show[TextureTarget3D] = Show[TextureTarget3D]

    inst:Eq[TextureTarget2DMultisampleArray]   = Eq[TextureTarget2DMultisampleArray]
    inst:Ord[TextureTarget2DMultisampleArray]  = Ord[TextureTarget2DMultisampleArray]
    inst:Show[TextureTarget2DMultisampleArray] = Show[TextureTarget2DMultisampleArray]

    inst:Eq[TextureTargetBuffer]   = Eq[TextureTargetBuffer]
    inst:Ord[TextureTargetBuffer]  = Ord[TextureTargetBuffer]
    inst:Show[TextureTargetBuffer] = Show[TextureTargetBuffer]

    inst:BindableTextureTarget[TextureTargetBuffer]             = BindableTextureTarget[TextureTargetBuffer]
    inst:BindableTextureTarget[TextureTarget2DMultisampleArray] = BindableTextureTarget[TextureTarget2DMultisampleArray]
    inst:BindableTextureTarget[TextureTarget3D]                 = BindableTextureTarget[TextureTarget3D]
    inst:BindableTextureTarget[TextureTargetCubeMap]            = BindableTextureTarget[TextureTargetCubeMap]
    inst:BindableTextureTarget[TextureTarget2DMultisample]      = BindableTextureTarget[TextureTarget2DMultisample]
    inst:BindableTextureTarget[TextureTarget2D]                 = BindableTextureTarget[TextureTarget2D]
    inst:BindableTextureTarget[TextureTarget1D]                 = BindableTextureTarget[TextureTarget1D]

    inst:ParameterizedTextureTarget[TextureTarget2DMultisampleArray] = ParameterizedTextureTarget[TextureTarget2DMultisampleArray]
    inst:ParameterizedTextureTarget[TextureTarget3D]                 = ParameterizedTextureTarget[TextureTarget3D]
    inst:ParameterizedTextureTarget[TextureTargetCubeMap]            = ParameterizedTextureTarget[TextureTargetCubeMap]
    inst:ParameterizedTextureTarget[TextureTarget2DMultisample]      = ParameterizedTextureTarget[TextureTarget2DMultisample]
    inst:ParameterizedTextureTarget[TextureTarget2D]                 = ParameterizedTextureTarget[TextureTarget2D]
    inst:ParameterizedTextureTarget[TextureTarget1D]                 = ParameterizedTextureTarget[TextureTarget1D]

    inst:OneDimensionalTextureTarget[TextureTarget1D] = OneDimensionalTextureTarget[TextureTarget1D]

    inst:TwoDimensionalTextureTarget[TextureTargetCubeMapFace] = TwoDimensionalTextureTarget[TextureTargetCubeMapFace]
    inst:TwoDimensionalTextureTarget[TextureTargetCubeMap]     = TwoDimensionalTextureTarget[TextureTargetCubeMap]
    inst:TwoDimensionalTextureTarget[TextureTarget2D]          = TwoDimensionalTextureTarget[TextureTarget2D]

    inst:ThreeDimensionalTextureTarget[TextureTarget3D] = ThreeDimensionalTextureTarget[TextureTarget3D]

    inst:QueryableTextureTarget[TextureTarget2DMultisampleArray] = QueryableTextureTarget[TextureTarget2DMultisampleArray]
    inst:QueryableTextureTarget[TextureTarget3D]                 = QueryableTextureTarget[TextureTarget3D]
    inst:QueryableTextureTarget[TextureTargetCubeMapFace]        = QueryableTextureTarget[TextureTargetCubeMapFace]
    inst:QueryableTextureTarget[TextureTarget2DMultisample]      = QueryableTextureTarget[TextureTarget2DMultisample]
    inst:QueryableTextureTarget[TextureTarget2D]                 = QueryableTextureTarget[TextureTarget2D]
    inst:QueryableTextureTarget[TextureTarget1D]                 = QueryableTextureTarget[TextureTarget1D]

    inst:GettableTextureTarget[TextureTarget3D]          = GettableTextureTarget[TextureTarget3D]
    inst:GettableTextureTarget[TextureTargetCubeMapFace] = GettableTextureTarget[TextureTargetCubeMapFace]
    inst:GettableTextureTarget[TextureTarget2D]          = GettableTextureTarget[TextureTarget2D]
    inst:GettableTextureTarget[TextureTarget1D]          = GettableTextureTarget[TextureTarget1D]

    inst:Eq[TexturePosition1D]   = Eq[TexturePosition1D]
    inst:Ord[TexturePosition1D]  = Ord[TexturePosition1D]
    inst:Show[TexturePosition1D] = Show[TexturePosition1D]

    inst:Eq[TexturePosition2D]   = Eq[TexturePosition2D]
    inst:Ord[TexturePosition2D]  = Ord[TexturePosition2D]
    inst:Show[TexturePosition2D] = Show[TexturePosition2D]

    inst:Eq[TexturePosition3D]   = Eq[TexturePosition3D]
    inst:Ord[TexturePosition3D]  = Ord[TexturePosition3D]
    inst:Show[TexturePosition3D] = Show[TexturePosition3D]

    inst:Eq[TextureSize1D]   = Eq[TextureSize1D]
    inst:Ord[TextureSize1D]  = Ord[TextureSize1D]
    inst:Show[TextureSize1D] = Show[TextureSize1D]

    inst:Eq[TextureSize2D]   = Eq[TextureSize2D]
    inst:Ord[TextureSize2D]  = Ord[TextureSize2D]
    inst:Show[TextureSize2D] = Show[TextureSize2D]

    inst:Eq[TextureSize3D]   = Eq[TextureSize3D]
    inst:Ord[TextureSize3D]  = Ord[TextureSize3D]
    inst:Show[TextureSize3D] = Show[TextureSize3D]

    inst:Eq[CompressedTextureFormat]   = Eq[CompressedTextureFormat]
    inst:Ord[CompressedTextureFormat]  = Ord[CompressedTextureFormat]
    inst:Show[CompressedTextureFormat] = Show[CompressedTextureFormat]

    inst:Eq[CompressedPixelData[A]]   = Eq[CompressedPixelData[A]]
    inst:Ord[CompressedPixelData[A]]  = Ord[CompressedPixelData[A]]
    inst:Show[CompressedPixelData[A]] = Show[CompressedPixelData[A]]

    inst:Eq[SampleLocations]   = Eq[SampleLocations]
    inst:Ord[SampleLocations]  = Ord[SampleLocations]
    inst:Show[SampleLocations] = Show[SampleLocations]
