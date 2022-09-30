{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Tiff where

open import Agda.Builtin.Char             using (Char)
open import Agda.Builtin.IO               using (IO)
open import Agda.Builtin.List             using (List)
open import Agda.Primitive                using (Level)
open import Ffi.Hs.-base.Unit             using (⊤)
open import Ffi.Hs.Codec.Picture.Metadata using (Metadatas)
open import Ffi.Hs.Codec.Picture.Types
open import Ffi.Hs.Data.ByteString        using (StrictByteString)
open import Ffi.Hs.Data.ByteString.Lazy   using (LazyByteString)
open import Ffi.Hs.Data.Either            using (Either)
open import Ffi.Hs.Data.Tuple             using (Tuple2)

{-# FOREIGN GHC
import qualified Codec.Picture.Tiff
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    TiffSaveable : Set aℓ → Set aℓ

    TiffSaveable[A]⇒Pixel[A] : ⦃ TiffSaveable A ⦄ → Pixel A

    TiffSaveable[PixelRGBA16] : TiffSaveable PixelRGBA16
    TiffSaveable[PixelRGBA8]  : TiffSaveable PixelRGBA8
    TiffSaveable[PixelCMYK16] : TiffSaveable PixelCMYK16
    TiffSaveable[PixelCMYK8]  : TiffSaveable PixelCMYK8
    TiffSaveable[PixelYCbCr8] : TiffSaveable PixelYCbCr8
    TiffSaveable[PixelRGB16]  : TiffSaveable PixelRGB16
    TiffSaveable[PixelRGB8]   : TiffSaveable PixelRGB8
    TiffSaveable[PixelYA16]   : TiffSaveable PixelYA16
    TiffSaveable[PixelYA8]    : TiffSaveable PixelYA8
    TiffSaveable[PixelF]      : TiffSaveable PixelF
    TiffSaveable[Pixel32]     : TiffSaveable Pixel32
    TiffSaveable[Pixel16]     : TiffSaveable Pixel16
    TiffSaveable[Pixel8]      : TiffSaveable Pixel8

    decodeTiff                       : StrictByteString → Either (List Char) DynamicImage
    decodeTiffWithMetadata           : StrictByteString → Either (List Char) (Tuple2 DynamicImage Metadatas)
    decodeTiffWithPaletteAndMetadata : StrictByteString → Either (List Char) (Tuple2 PalettedImage Metadatas)
    encodeTiff                       : ⦃ TiffSaveable A ⦄ → Image A → LazyByteString
    writeTiff                        : ⦃ TiffSaveable A ⦄ → List Char → Image A → IO ⊤

{-# FOREIGN GHC data AgdaTiffSaveable aℓ a = Codec.Picture.Tiff.TiffSaveable a => AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable = type(0) AgdaTiffSaveable #-}

{-# COMPILE GHC TiffSaveable[A]⇒Pixel[A] = \ aℓ a AgdaTiffSaveable -> AgdaPixel #-}

{-# COMPILE GHC TiffSaveable[PixelRGBA16] = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[PixelRGBA8]  = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[PixelCMYK16] = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[PixelCMYK8]  = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[PixelYCbCr8] = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[PixelRGB16]  = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[PixelRGB8]   = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[PixelYA16]   = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[PixelYA8]    = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[PixelF]      = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[Pixel32]     = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[Pixel16]     = AgdaTiffSaveable #-}
{-# COMPILE GHC TiffSaveable[Pixel8]      = AgdaTiffSaveable #-}

{-# COMPILE GHC decodeTiff                       =                            Codec.Picture.Tiff.decodeTiff                       #-}
{-# COMPILE GHC decodeTiffWithMetadata           =                            Codec.Picture.Tiff.decodeTiffWithMetadata           #-}
{-# COMPILE GHC decodeTiffWithPaletteAndMetadata =                            Codec.Picture.Tiff.decodeTiffWithPaletteAndMetadata #-}
{-# COMPILE GHC encodeTiff                       = \ aℓ a AgdaTiffSaveable -> Codec.Picture.Tiff.encodeTiff                       #-}
{-# COMPILE GHC writeTiff                        = \ aℓ a AgdaTiffSaveable -> Codec.Picture.Tiff.writeTiff                        #-}
