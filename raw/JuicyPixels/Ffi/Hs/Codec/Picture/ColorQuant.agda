{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.ColorQuant where

open import Agda.Builtin.Bool          using (Bool)
open import Agda.Builtin.List          using (List)
open import Ffi.Hs.Codec.Picture.Gif   using (GifDisposalMethod; GifDelay; GifFrame)
open import Ffi.Hs.Codec.Picture.Types
open import Ffi.Hs.Data.Int            using (Int)
open import Ffi.Hs.Data.Tuple          using (Tuple2)

{-# FOREIGN GHC
import qualified Codec.Picture.ColorQuant
#-}

data PaletteCreationMethod : Set where
    MedianMeanCut : PaletteCreationMethod
    Uniform       : PaletteCreationMethod

{-# COMPILE GHC PaletteCreationMethod = data Codec.Picture.ColorQuant.PaletteCreationMethod
    ( Codec.Picture.ColorQuant.MedianMeanCut
    | Codec.Picture.ColorQuant.Uniform
    ) #-}

record PaletteOptions : Set where
    constructor mkPaletteOptions
    field
        paletteCreationMethod : PaletteCreationMethod
        enableImageDithering  : Bool
        paletteColorCount     : Int

{-# COMPILE GHC PaletteOptions = data Codec.Picture.ColorQuant.PaletteOptions (Codec.Picture.ColorQuant.PaletteOptions) #-}

postulate
    palettize             : PaletteOptions → Image PixelRGB8 → Tuple2 (Image Pixel8) Palette
    palettizeWithAlpha    : List (Tuple2 GifDelay (Image PixelRGBA8)) → GifDisposalMethod → List GifFrame
    defaultPaletteOptions : PaletteOptions

{-# COMPILE GHC palettize             = Codec.Picture.ColorQuant.palettize             #-}
{-# COMPILE GHC palettizeWithAlpha    = Codec.Picture.ColorQuant.palettizeWithAlpha    #-}
{-# COMPILE GHC defaultPaletteOptions = Codec.Picture.ColorQuant.defaultPaletteOptions #-}
