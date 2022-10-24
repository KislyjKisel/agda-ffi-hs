{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Png-Instanced where

open import Ffi.Hs.Codec.Picture.Png

instance
    inst:PngSavable[PixelRGBA16] = PngSavable[PixelRGBA16]
    inst:PngSavable[PixelRGBA8]  = PngSavable[PixelRGBA8] 
    inst:PngSavable[PixelRGB16]  = PngSavable[PixelRGB16] 
    inst:PngSavable[PixelRGB8]   = PngSavable[PixelRGB8]  
    inst:PngSavable[PixelYA16]   = PngSavable[PixelYA16]  
    inst:PngSavable[PixelYA8]    = PngSavable[PixelYA8]   
    inst:PngSavable[Pixel16]     = PngSavable[Pixel16]    
    inst:PngSavable[Pixel8]      = PngSavable[Pixel8]     

    inst:PngPaletteSaveable[PixelRGBA8] = PngPaletteSaveable[PixelRGBA8]
    inst:PngPaletteSaveable[PixelRGB8]  = PngPaletteSaveable[PixelRGB8] 
