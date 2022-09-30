{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Png where

open import Agda.Builtin.Bool                  using (Bool)
open import Agda.Builtin.Char                  using (Char)
open import Agda.Builtin.IO                    using (IO)
open import Agda.Builtin.List                  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit                  using (⊤)
open import Ffi.Hs.Codec.Picture.Metadata      using (Metadatas)
open import Ffi.Hs.Codec.Picture.Types
open import Ffi.Hs.Data.ByteString as BS       using ()
open import Ffi.Hs.Data.ByteString.Lazy as LBS using ()
open import Ffi.Hs.Data.Either                 using (Either)
open import Ffi.Hs.Data.Tuple                  using (Tuple2)

{-# FOREIGN GHC
import qualified Codec.Picture.Png
#-}

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

    encodePalettedPng             : ⦃ PngPaletteSaveable A ⦄ → Image A → Image Pixel8 → Either (List Char) LBS.ByteString
    encodePalettedPngWithMetadata : ⦃ PngPaletteSaveable A ⦄ → Metadatas → Image A → Image Pixel8 → Either (List Char) LBS.ByteString

    PngPaletteSaveable[PixelRGBA8] : PngPaletteSaveable PixelRGBA8
    PngPaletteSaveable[PixelRGB8]  : PngPaletteSaveable PixelRGB8


    decodePng                       : BS.ByteString → Either (List Char) DynamicImage
    decodePngWithMetadata           : BS.ByteString → Either (List Char) (Tuple2 DynamicImage Metadatas)
    decodePngWithPaletteAndMetadata : BS.ByteString → Either (List Char) (Tuple2 PalettedImage Metadatas)
    writePng                        : ⦃ PngSavable A ⦄ → List Char → Image A → IO ⊤
    encodeDynamicPng                : DynamicImage → Either (List Char) LBS.ByteString
    writeDynamicPng                 : List Char → DynamicImage → IO (Either (List Char) Bool)

{-# FOREIGN GHC data AgdaPngSavable aℓ a = Codec.Picture.Png.PngSavable a => AgdaPngSavable #-}
{-# COMPILE GHC PngSavable = type(0) AgdaPngSavable #-}

{-# COMPILE GHC encodePng             = \ aℓ a AgdaPngSaveable -> Codec.Picture.Png.encodePng             #-}
{-# COMPILE GHC encodePngWithMetadata = \ aℓ a AgdaPngSaveable -> Codec.Picture.Png.encodePngWithMetadata #-}

{-# COMPILE GHC PngSavable[PixelRGBA16] = AgdaPngSavable #-}
{-# COMPILE GHC PngSavable[PixelRGBA8]  = AgdaPngSavable #-}
{-# COMPILE GHC PngSavable[PixelRGB16]  = AgdaPngSavable #-}
{-# COMPILE GHC PngSavable[PixelRGB8]   = AgdaPngSavable #-}
{-# COMPILE GHC PngSavable[PixelYA16]   = AgdaPngSavable #-}
{-# COMPILE GHC PngSavable[PixelYA8]    = AgdaPngSavable #-}
{-# COMPILE GHC PngSavable[Pixel16]     = AgdaPngSavable #-}
{-# COMPILE GHC PngSavable[Pixel8]      = AgdaPngSavable #-}

{-# FOREIGN GHC data AgdaPngPaletteSaveable aℓ a = Codec.Picture.Png.PngPaletteSaveable a => AgdaPngPaletteSaveable #-}
{-# COMPILE GHC PngPaletteSaveable = type(0) AgdaPngPaletteSaveable #-}

{-# COMPILE GHC encodePalettedPng             = \ aℓ a AgdaPngPaletteSaveable -> Codec.Picture.Png.encodePalettedPng             #-}
{-# COMPILE GHC encodePalettedPngWithMetadata = \ aℓ a AgdaPngPaletteSaveable -> Codec.Picture.Png.encodePalettedPngWithMetadata #-}

{-# COMPILE GHC PngPaletteSaveable[PixelRGBA8] = AgdaPngPaletteSaveable #-}
{-# COMPILE GHC PngPaletteSaveable[PixelRGB8]  = AgdaPngPaletteSaveable #-}

{-# COMPILE GHC decodePng                       =                           Codec.Picture.Png.decodePng                       #-}
{-# COMPILE GHC decodePngWithMetadata           =                           Codec.Picture.Png.decodePngWithMetadata           #-}
{-# COMPILE GHC decodePngWithPaletteAndMetadata =                           Codec.Picture.Png.decodePngWithPaletteAndMetadata #-}
{-# COMPILE GHC writePng                        = \ aℓ a AgdaPngSaveable -> Codec.Picture.Png.writePng                        #-}
{-# COMPILE GHC encodeDynamicPng                =                           Codec.Picture.Png.encodeDynamicPng                #-}
{-# COMPILE GHC writeDynamicPng                 =                           Codec.Picture.Png.writeDynamicPng                 #-}
