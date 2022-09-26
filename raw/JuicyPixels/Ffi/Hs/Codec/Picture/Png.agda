{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Png where

open import Agda.Primitive
open import Ffi.Hs.Codec.Picture.Types
open import Ffi.Hs.Data.ByteString as BS using ()
open import Ffi.Hs.Data.ByteString.Lazy as LBS using ()
open import Ffi.Hs.Codec.Picture.Metadata using (Metadatas)

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    PngSavable : Set aℓ → Set aℓ
    encodePng             : ⦃ PngSavable A ⦄ → Image A → LBS.ByteString
    encodePngWithMetadata : ⦃ PngSavable A ⦄ → Metadatas → Image A → LBS.ByteString

    PngSavable[PixelRGBA16] : PngSavable PixelRGBA16
    PngSavable[PixelRGBA8]  : PngSavable PixelRGBA8
    PngSavable[PixelRGB16]  : PngSavable PixelRGB16
    PngSavable[PixelRGB8]   : PngSavable PixelRGB8
    PngSavable[PixelYA16]   : PngSavable PixelYA16
    PngSavable[PixelYA8]    : PngSavable PixelYA8
    PngSavable[Pixel16]     : PngSavable Pixel16
    PngSavable[Pixel8]      : PngSavable Pixel8

    PngPaletteSaveable : Set aℓ → Set aℓ
    encodePalettedPng             : ⦃ PngPaletteSaveable A ⦄ → Image A → Image Pixel8 → Either String LBS.ByteString
    encodePalettedPngWithMetadata : ⦃ PngPaletteSaveable A ⦄ → Metadatas → Image A → Image Pixel8 → Either String LBS.ByteString

    PngPaletteSaveable[PixelRGBA8] : PngPaletteSaveable PixelRGBA8
    PngPaletteSaveable[PixelRGB8]  : PngPaletteSaveable PixelRGB8

    decodePng                       : BS.ByteString → Either String DynamicImage
    decodePngWithMetadata           : BS.ByteString → Either String (Tuple2 DynamicImage Metadatas)
    -- todo: decodePngWithPaletteAndMetadata : ByteString → Either String (Tuple2 PalettedImage Metadatas)
    writePng                        : ⦃ PngSavable A ⦄ → FilePath → Image A → IO ⊤
    encodeDynamicPng                : DynamicImage → Either String LBS.ByteString
    writeDynamicPng                 : FilePath → DynamicImage → IO (Either String Bool)
