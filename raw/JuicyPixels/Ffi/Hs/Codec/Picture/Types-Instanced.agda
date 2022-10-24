{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Types-Instanced where

open import Ffi.Hs.Codec.Picture.Types

instance
    inst:Eq[PixelYA8]    = Eq[PixelYA8]
    inst:Ord[PixelYA8]   = Ord[PixelYA8]
    inst:Show[PixelYA8]  = Show[PixelYA8]
    inst:Pixel[PixelYA8] = Pixel[PixelYA8]

    inst:Eq[PixelYA16]    = Eq[PixelYA16]
    inst:Ord[PixelYA16]   = Ord[PixelYA16]
    inst:Show[PixelYA16]  = Show[PixelYA16]
    inst:Pixel[PixelYA16] = Pixel[PixelYA16]

    inst:Eq[PixelRGB8]    = Eq[PixelRGB8]
    inst:Ord[PixelRGB8]   = Ord[PixelRGB8]
    inst:Show[PixelRGB8]  = Show[PixelRGB8]
    inst:Pixel[PixelRGB8] = Pixel[PixelRGB8]

    inst:Eq[PixelRGB16]    = Eq[PixelRGB16]
    inst:Ord[PixelRGB16]   = Ord[PixelRGB16]
    inst:Show[PixelRGB16]  = Show[PixelRGB16]
    inst:Pixel[PixelRGB16] = Pixel[PixelRGB16]

    inst:Eq[PixelRGBF]    = Eq[PixelRGBF]
    inst:Ord[PixelRGBF]   = Ord[PixelRGBF]
    inst:Show[PixelRGBF]  = Show[PixelRGBF]
    inst:Pixel[PixelRGBF] = Pixel[PixelRGBF]

    inst:Eq[PixelRGBA8]    = Eq[PixelRGBA8]
    inst:Ord[PixelRGBA8]   = Ord[PixelRGBA8]
    inst:Show[PixelRGBA8]  = Show[PixelRGBA8]
    inst:Pixel[PixelRGBA8] = Pixel[PixelRGBA8]

    inst:Eq[PixelRGBA16]    = Eq[PixelRGBA16]
    inst:Ord[PixelRGBA16]   = Ord[PixelRGBA16]
    inst:Show[PixelRGBA16]  = Show[PixelRGBA16]
    inst:Pixel[PixelRGBA16] = Pixel[PixelRGBA16]

    inst:Eq[PixelCMYK8]    = Eq[PixelCMYK8]
    inst:Ord[PixelCMYK8]   = Ord[PixelCMYK8]
    inst:Show[PixelCMYK8]  = Show[PixelCMYK8]
    inst:Pixel[PixelCMYK8] = Pixel[PixelCMYK8]

    inst:Eq[PixelCMYK16]    = Eq[PixelCMYK16]
    inst:Ord[PixelCMYK16]   = Ord[PixelCMYK16]
    inst:Show[PixelCMYK16]  = Show[PixelCMYK16]
    inst:Pixel[PixelCMYK16] = Pixel[PixelCMYK16]

    inst:Eq[PixelYCbCr8]    = Eq[PixelYCbCr8]
    inst:Ord[PixelYCbCr8]   = Ord[PixelYCbCr8]
    inst:Show[PixelYCbCr8]  = Show[PixelYCbCr8]
    inst:Pixel[PixelYCbCr8] = Pixel[PixelYCbCr8]

    inst:Eq[PixelYCbCrK8]    = Eq[PixelYCbCrK8]
    inst:Ord[PixelYCbCrK8]   = Ord[PixelYCbCrK8]
    inst:Show[PixelYCbCrK8]  = Show[PixelYCbCrK8]
    inst:Pixel[PixelYCbCrK8] = Pixel[PixelYCbCrK8]

    inst:ColorConvertible[A,A]                    = ColorConvertible[A,A]
    inst:ColorConvertible[PixelRGBA8,PixelRGBA16] = ColorConvertible[PixelRGBA8,PixelRGBA16]
    inst:ColorConvertible[PixelRGB16,PixelRGBA16] = ColorConvertible[PixelRGB16,PixelRGBA16]
    inst:ColorConvertible[PixelRGB8,PixelRGBA16]  = ColorConvertible[PixelRGB8,PixelRGBA16]
    inst:ColorConvertible[PixelRGB8,PixelRGBA8]   = ColorConvertible[PixelRGB8,PixelRGBA8]
    inst:ColorConvertible[PixelRGB8,PixelRGBF]    = ColorConvertible[PixelRGB8,PixelRGBF]
    inst:ColorConvertible[PixelRGB8,PixelRGB16]   = ColorConvertible[PixelRGB8,PixelRGB16]
    inst:ColorConvertible[PixelYA16,PixelRGBA16]  = ColorConvertible[PixelYA16,PixelRGBA16]
    inst:ColorConvertible[PixelYA16,PixelRGB16]   = ColorConvertible[PixelYA16,PixelRGB16]
    inst:ColorConvertible[PixelYA8,PixelRGBA8]    = ColorConvertible[PixelYA8,PixelRGBA8]
    inst:ColorConvertible[PixelYA8,PixelRGB16]    = ColorConvertible[PixelYA8,PixelRGB16]
    inst:ColorConvertible[PixelYA8,PixelRGB8]     = ColorConvertible[PixelYA8,PixelRGB8]
    inst:ColorConvertible[PixelF,PixelRGBF]       = ColorConvertible[PixelF,PixelRGBF]
    inst:ColorConvertible[Pixel16,PixelRGBA16]    = ColorConvertible[Pixel16,PixelRGBA16]
    inst:ColorConvertible[Pixel16,PixelRGB16]     = ColorConvertible[Pixel16,PixelRGB16]
    inst:ColorConvertible[Pixel16,PixelYA16]      = ColorConvertible[Pixel16,PixelYA16]
    inst:ColorConvertible[Pixel8,PixelRGBA8]      = ColorConvertible[Pixel8,PixelRGBA8]
    inst:ColorConvertible[Pixel8,PixelRGB16]      = ColorConvertible[Pixel8,PixelRGB16]
    inst:ColorConvertible[Pixel8,PixelRGB8]       = ColorConvertible[Pixel8,PixelRGB8]
    inst:ColorConvertible[Pixel8,PixelYA8]        = ColorConvertible[Pixel8,PixelYA8]
    inst:ColorConvertible[Pixel8,PixelF]          = ColorConvertible[Pixel8,PixelF]
    inst:ColorConvertible[Pixel8,Pixel16]         = ColorConvertible[Pixel8,Pixel16]

    inst:ColorSpaceConvertible[A,A]                     = ColorSpaceConvertible[A,A]
    inst:ColorSpaceConvertible[PixelCMYK16,PixelRGB16]  = ColorSpaceConvertible[PixelCMYK16,PixelRGB16]
    inst:ColorSpaceConvertible[PixelCMYK8,PixelRGB8]    = ColorSpaceConvertible[PixelCMYK8,PixelRGB8]
    inst:ColorSpaceConvertible[PixelYCbCr8,PixelRGB8]   = ColorSpaceConvertible[PixelYCbCr8,PixelRGB8]
    inst:ColorSpaceConvertible[PixelRGB16,PixelCMYK16]  = ColorSpaceConvertible[PixelRGB16,PixelCMYK16]
    inst:ColorSpaceConvertible[PixelYCbCrK8,PixelCMYK8] = ColorSpaceConvertible[PixelYCbCrK8,PixelCMYK8]
    inst:ColorSpaceConvertible[PixelYCbCrK8,PixelRGB8]  = ColorSpaceConvertible[PixelYCbCrK8,PixelRGB8]
    inst:ColorSpaceConvertible[PixelRGB8,PixelCMYK8]    = ColorSpaceConvertible[PixelRGB8,PixelCMYK8]
    inst:ColorSpaceConvertible[PixelRGB8,PixelYCbCr8]   = ColorSpaceConvertible[PixelRGB8,PixelYCbCr8]

    inst:LumaPlaneExtractable[PixelRGBA8]  = LumaPlaneExtractable[PixelRGBA8]
    inst:LumaPlaneExtractable[PixelYCbCr8] = LumaPlaneExtractable[PixelYCbCr8]
    inst:LumaPlaneExtractable[PixelRGBF]   = LumaPlaneExtractable[PixelRGBF]
    inst:LumaPlaneExtractable[PixelRGB16]  = LumaPlaneExtractable[PixelRGB16]
    inst:LumaPlaneExtractable[PixelRGB8]   = LumaPlaneExtractable[PixelRGB8]
    inst:LumaPlaneExtractable[PixelYA8]    = LumaPlaneExtractable[PixelYA8]
    inst:LumaPlaneExtractable[PixelF]      = LumaPlaneExtractable[PixelF]
    inst:LumaPlaneExtractable[Pixel32]     = LumaPlaneExtractable[Pixel32]
    inst:LumaPlaneExtractable[Pixel16]     = LumaPlaneExtractable[Pixel16]
    inst:LumaPlaneExtractable[Pixel8]      = LumaPlaneExtractable[Pixel8]

    inst:TransparentPixel[PixelRGBA16,PixelRGB16] = TransparentPixel[PixelRGBA16,PixelRGB16]
    inst:TransparentPixel[PixelRGBA8,PixelRGB8]   = TransparentPixel[PixelRGBA8,PixelRGB8]
    inst:TransparentPixel[PixelYA16,Pixel16]      = TransparentPixel[PixelYA16,Pixel16]
    inst:TransparentPixel[PixelYA8,Pixel8]        = TransparentPixel[PixelYA8,Pixel8]

    inst:ColorPlane[PixelRGBA16,PlaneAlpha]   = ColorPlane[PixelRGBA16,PlaneAlpha]
    inst:ColorPlane[PixelRGBA16,PlaneBlue]    = ColorPlane[PixelRGBA16,PlaneBlue]
    inst:ColorPlane[PixelRGBA16,PlaneGreen]   = ColorPlane[PixelRGBA16,PlaneGreen]
    inst:ColorPlane[PixelRGBA16,PlaneRed]     = ColorPlane[PixelRGBA16,PlaneRed]
    inst:ColorPlane[PixelRGBA8,PlaneAlpha]    = ColorPlane[PixelRGBA8,PlaneAlpha]
    inst:ColorPlane[PixelRGBA8,PlaneBlue]     = ColorPlane[PixelRGBA8,PlaneBlue]
    inst:ColorPlane[PixelRGBA8,PlaneGreen]    = ColorPlane[PixelRGBA8,PlaneGreen]
    inst:ColorPlane[PixelRGBA8,PlaneRed]      = ColorPlane[PixelRGBA8,PlaneRed]
    inst:ColorPlane[PixelCMYK16,PlaneBlack]   = ColorPlane[PixelCMYK16,PlaneBlack]
    inst:ColorPlane[PixelCMYK16,PlaneYellow]  = ColorPlane[PixelCMYK16,PlaneYellow]
    inst:ColorPlane[PixelCMYK16,PlaneMagenta] = ColorPlane[PixelCMYK16,PlaneMagenta]
    inst:ColorPlane[PixelCMYK16,PlaneCyan]    = ColorPlane[PixelCMYK16,PlaneCyan]
    inst:ColorPlane[PixelCMYK8,PlaneBlack]    = ColorPlane[PixelCMYK8,PlaneBlack]
    inst:ColorPlane[PixelCMYK8,PlaneYellow]   = ColorPlane[PixelCMYK8,PlaneYellow]
    inst:ColorPlane[PixelCMYK8,PlaneMagenta]  = ColorPlane[PixelCMYK8,PlaneMagenta]
    inst:ColorPlane[PixelCMYK8,PlaneCyan]     = ColorPlane[PixelCMYK8,PlaneCyan]
    inst:ColorPlane[PixelYCbCr8,PlaneCb]      = ColorPlane[PixelYCbCr8,PlaneCb]
    inst:ColorPlane[PixelYCbCr8,PlaneCr]      = ColorPlane[PixelYCbCr8,PlaneCr]
    inst:ColorPlane[PixelYCbCr8,PlaneLuma]    = ColorPlane[PixelYCbCr8,PlaneLuma]
    inst:ColorPlane[PixelRGBF,PlaneBlue]      = ColorPlane[PixelRGBF,PlaneBlue]
    inst:ColorPlane[PixelRGBF,PlaneGreen]     = ColorPlane[PixelRGBF,PlaneGreen]
    inst:ColorPlane[PixelRGBF,PlaneRed]       = ColorPlane[PixelRGBF,PlaneRed]
    inst:ColorPlane[PixelRGB16,PlaneBlue]     = ColorPlane[PixelRGB16,PlaneBlue]
    inst:ColorPlane[PixelRGB16,PlaneGreen]    = ColorPlane[PixelRGB16,PlaneGreen]
    inst:ColorPlane[PixelRGB16,PlaneRed]      = ColorPlane[PixelRGB16,PlaneRed]
    inst:ColorPlane[PixelRGB8,PlaneBlue]      = ColorPlane[PixelRGB8,PlaneBlue]
    inst:ColorPlane[PixelRGB8,PlaneGreen]     = ColorPlane[PixelRGB8,PlaneGreen]
    inst:ColorPlane[PixelRGB8,PlaneRed]       = ColorPlane[PixelRGB8,PlaneRed]
    inst:ColorPlane[PixelYA16,PlaneLuma]      = ColorPlane[PixelYA16,PlaneLuma]
    inst:ColorPlane[PixelYA16,PlaneAlpha]     = ColorPlane[PixelYA16,PlaneAlpha]
    inst:ColorPlane[PixelYA8,PlaneLuma]       = ColorPlane[PixelYA8,PlaneLuma]
    inst:ColorPlane[PixelYA8,PlaneAlpha]      = ColorPlane[PixelYA8,PlaneAlpha]

    inst:Eq[Image[A]]     = Eq[Image[A]]
    inst:NFData[Image[A]] = NFData[Image[A]]

    inst:NFData[MutableImage[S,A]] = NFData[MutableImage[S,A]]

    inst:Eq[DynamicImage]     = Eq[DynamicImage]
    inst:NFData[DynamicImage] = NFData[DynamicImage]
