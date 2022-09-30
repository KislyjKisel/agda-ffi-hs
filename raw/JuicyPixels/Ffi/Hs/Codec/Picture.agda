{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture where

open import Agda.Builtin.Char             using (Char)
open import Agda.Builtin.IO               using (IO)
open import Agda.Builtin.List             using (List)
open import Ffi.Hs.-base.Unit             using (⊤)
open import Ffi.Hs.Codec.Picture.Metadata using (Metadatas)
open import Ffi.Hs.Data.ByteString        using (StrictByteString)
open import Ffi.Hs.Data.ByteString.Lazy   using (LazyByteString)
open import Ffi.Hs.Data.Either            using (Either)
open import Ffi.Hs.Data.Int               using (Int)
open import Ffi.Hs.Data.Tuple             using (Tuple2)
open import Ffi.Hs.Data.Type.Equality     using (_~_)
open import Ffi.Hs.Data.Word              using (Word8)
open import Ffi.Hs.Foreign.ForeignPtr     using (ForeignPtr)

{-# FOREIGN GHC
import qualified Codec.Picture
import MAlonzo.Code.Ffi.Hs.Data.Type.Equality (AgdaTypeEq(AgdaTypeEq))
#-}

open import Ffi.Hs.Codec.Picture.Types using (PalettedImage)

open Ffi.Hs.Codec.Picture.Types public
    using
    ( Image
    ; mkImage
    ; Eq[Image[A]]
    ; NFData[Image[A]]

    ; DynamicImage
    ; ImageY8
    ; ImageY16
    ; ImageY32
    ; ImageYF
    ; ImageYA8
    ; ImageYA16
    ; ImageRGB8
    ; ImageRGB16
    ; ImageRGBF
    ; ImageRGBA8
    ; ImageRGBA16
    ; ImageYCbCr8
    ; ImageCMYK8
    ; ImageCMYK16
    ; Eq[DynamicImage]
    ; NFData[DynamicImage]

    ; Palette

    ; Pixel
    ; PixelBaseComponent
    ; mixWith
    ; mixWithAlpha
    ; pixelOpacity
    ; componentCount
    ; colorMap
    ; pixelBaseIndex
    ; mutablePixelBaseIndex
    ; pixelAt
    ; readPixel
    ; writePixel
    ; unsafePixelAt
    ; unsafeReadPixel
    ; unsafeWritePixel

    ; Pixel[PixelRGBA16]
    ; PixelBaseComponent[PixelRGBA16]
    ; Pixel[PixelRGBA8]
    ; PixelBaseComponent[PixelRGBA8]
    ; Pixel[PixelCMYK16]
    ; PixelBaseComponent[PixelCMYK16]
    ; Pixel[PixelCMYK8]
    ; PixelBaseComponent[PixelCMYK8]
    ; Pixel[PixelYCbCr8]
    ; PixelBaseComponent[PixelYCbCr8]
    ; Pixel[PixelRGBF]
    ; PixelBaseComponent[PixelRGBF]
    ; Pixel[PixelRGB16]
    ; PixelBaseComponent[PixelRGB16]
    ; Pixel[PixelYCbCrK8]
    ; PixelBaseComponent[PixelYCbCrK8]
    ; Pixel[PixelRGB8]
    ; PixelBaseComponent[PixelRGB8]
    ; Pixel[PixelYA16]
    ; PixelBaseComponent[PixelYA16]

    ; Pixel8
    ; Pixel16
    ; Pixel32
    ; PixelF

    ; PixelYA8
    ; Eq[PixelYA8]
    ; Ord[PixelYA8]
    ; Show[PixelYA8]

    ; PixelYA16
    ; Eq[PixelYA16]
    ; Ord[PixelYA16]
    ; Show[PixelYA16]

    ; PixelRGB8
    ; Eq[PixelRGB8]
    ; Ord[PixelRGB8]
    ; Show[PixelRGB8]

    ; PixelRGB16
    ; Eq[PixelRGB16]
    ; Ord[PixelRGB16]
    ; Show[PixelRGB16]

    ; PixelRGBF
    ; Eq[PixelRGBF]
    ; Ord[PixelRGBF]
    ; Show[PixelRGBF]

    ; PixelRGBA8
    ; Eq[PixelRGBA8]
    ; Ord[PixelRGBA8]
    ; Show[PixelRGBA8]

    ; PixelRGBA16
    ; Eq[PixelRGBA16]
    ; Ord[PixelRGBA16]
    ; Show[PixelRGBA16]

    ; PixelYCbCr8
    ; Eq[PixelYCbCr8]
    ; Ord[PixelYCbCr8]
    ; Show[PixelYCbCr8]

    ; PixelCMYK8
    ; Eq[PixelCMYK8]
    ; Ord[PixelCMYK8]
    ; Show[PixelCMYK8]

    ; PixelCMYK16
    ; Eq[PixelCMYK16]
    ; Ord[PixelCMYK16]
    ; Show[PixelCMYK16]

    -- todo: ; imagePixels
    -- todo: ; imageIPixels

    ; palettedToTrueColor
    ; withImage
    ; generateFoldImage
    ; generateImage
    ; dynamicPixelMap
    ; dynamicMap
    ; pixelMap
    )

postulate
    readImage                         : List Char → IO (Either (List Char) DynamicImage)
    readImageWithMetadata             : List Char → IO (Either (List Char) (Tuple2 DynamicImage Metadatas))
    decodeImage                       : StrictByteString → Either (List Char) DynamicImage
    decodeImageWithMetadata           : StrictByteString → Either (List Char) (Tuple2 DynamicImage Metadatas)
    decodeImageWithPaletteAndMetadata : StrictByteString → Either (List Char) (Tuple2 PalettedImage Metadatas)

{-# COMPILE GHC readImage                         = Codec.Picture.readImage                         #-}
{-# COMPILE GHC readImageWithMetadata             = Codec.Picture.readImageWithMetadata             #-}
{-# COMPILE GHC decodeImage                       = Codec.Picture.decodeImage                       #-}
{-# COMPILE GHC decodeImageWithMetadata           = Codec.Picture.decodeImageWithMetadata           #-}
{-# COMPILE GHC decodeImageWithPaletteAndMetadata = Codec.Picture.decodeImageWithPaletteAndMetadata #-}


postulate
    convertRGB8  : DynamicImage → Image PixelRGB8
    convertRGB16 : DynamicImage → Image PixelRGB16
    convertRGBA8 : DynamicImage → Image PixelRGBA8

{-# COMPILE GHC convertRGB8  = Codec.Picture.convertRGB8  #-}
{-# COMPILE GHC convertRGB16 = Codec.Picture.convertRGB16 #-}
{-# COMPILE GHC convertRGBA8 = Codec.Picture.convertRGBA8 #-}


postulate
    saveBmpImage      : List Char → DynamicImage → IO ⊤
    saveJpgImage      : Int → List Char → DynamicImage → IO ⊤
    saveGifImage      : List Char → DynamicImage → Either (List Char) (IO ⊤)
    savePngImage      : List Char → DynamicImage → IO ⊤
    saveTiffImage     : List Char → DynamicImage → IO ⊤
    saveRadianceImage : List Char → DynamicImage → IO ⊤

{-# COMPILE GHC saveBmpImage      = Codec.Picture.saveBmpImage      #-}
{-# COMPILE GHC saveJpgImage      = Codec.Picture.saveJpgImage      #-}
{-# COMPILE GHC saveGifImage      = Codec.Picture.saveGifImage      #-}
{-# COMPILE GHC savePngImage      = Codec.Picture.savePngImage      #-}
{-# COMPILE GHC saveTiffImage     = Codec.Picture.saveTiffImage     #-}
{-# COMPILE GHC saveRadianceImage = Codec.Picture.saveRadianceImage #-}


open import Ffi.Hs.Codec.Picture.Bitmap public
    using
    ( BmpEncodable
    ; BmpEncodable[PixelRGBA8]
    ; BmpEncodable[PixelRGB8]
    ; BmpEncodable[Pixel8]

    ; writeBitmap
    ; encodeBitmap
    ; decodeBitmap
    ; encodeDynamicBitmap
    ; writeDynamicBitmap
    )

postulate
    readBitmap : List Char → IO (Either (List Char) DynamicImage)

{-# COMPILE GHC readBitmap = Codec.Picture.readBitmap #-}


open import Ffi.Hs.Codec.Picture.Gif public
    using
    ( GifDelay

    ; GifLooping
    ; LoopingNever
    ; LoopingForever
    ; LoopingRepeat

    ; decodeGif
    ; decodeGifImages
    ; encodeGifImage
    ; writeGifImage
    ; encodeGifImageWithPalette
    ; writeGifImageWithPalette
    ; encodeGifImages
    ; writeGifImages
    )

postulate
    readGif                    : List Char → IO (Either (List Char) DynamicImage)
    readGifImages              : List Char → IO (Either (List Char) (List DynamicImage))
    encodeColorReducedGifImage : Image PixelRGB8 → Either (List Char) LazyByteString
    writeColorReducedGifImage  : List Char → Image PixelRGB8 → Either (List Char) (IO ⊤)
    encodeGifAnimation         : GifDelay → GifLooping → List (Image PixelRGB8) → Either (List Char) LazyByteString
    writeGifAnimation          : List Char → GifDelay → GifLooping → List (Image PixelRGB8) → Either (List Char) (IO ⊤)

{-# COMPILE GHC readGif                    = Codec.Picture.readGif                    #-}
{-# COMPILE GHC readGifImages              = Codec.Picture.readGifImages              #-}
{-# COMPILE GHC encodeColorReducedGifImage = Codec.Picture.encodeColorReducedGifImage #-}
{-# COMPILE GHC writeColorReducedGifImage  = Codec.Picture.writeColorReducedGifImage  #-}
{-# COMPILE GHC encodeGifAnimation         = Codec.Picture.encodeGifAnimation         #-}
{-# COMPILE GHC writeGifAnimation          = Codec.Picture.writeGifAnimation          #-}


open import Ffi.Hs.Codec.Picture.Jpg public
    using
    ( decodeJpeg
    ; encodeJpeg
    ; encodeJpegAtQuality
    )

postulate
    readJpeg : List Char → IO (Either (List Char) DynamicImage)

{-# COMPILE GHC readJpeg = Codec.Picture.readJpeg #-}


open import Ffi.Hs.Codec.Picture.Png public
    using
    ( PngSavable
    ; PngSavable[PixelRGBA16]
    ; PngSavable[PixelRGBA8]
    ; PngSavable[PixelRGB16]
    ; PngSavable[PixelRGB8]
    ; PngSavable[PixelYA16]
    ; PngSavable[PixelYA8]
    ; PngSavable[Pixel16]
    ; PngSavable[Pixel8]

    ; encodePng
    ; encodePngWithMetadata
    ; decodePng
    ; writePng
    ; encodePalettedPng
    ; encodeDynamicPng
    ; writeDynamicPng
    )

postulate
    readPng : List Char → IO (Either (List Char) DynamicImage)

{-# COMPILE GHC readPng = Codec.Picture.readPng #-}


open import Ffi.Hs.Codec.Picture.Tga public
    using
    ( TgaSaveable
    ; TgaSaveable[PixelRGBA8]
    ; TgaSaveable[PixelRGB8]
    ; TgaSaveable[Pixel8]
    ; decodeTga
    ; encodeTga
    ; writeTga
    )

postulate
    readTga : List Char → IO (Either (List Char) DynamicImage)

{-# COMPILE GHC readTga = Codec.Picture.readTga #-}


open import Ffi.Hs.Codec.Picture.Tiff public
    using
    ( TiffSaveable
    ; TiffSaveable[A]⇒Pixel[A]
    ; TiffSaveable[PixelRGBA16]
    ; TiffSaveable[PixelRGBA8]
    ; TiffSaveable[PixelCMYK16]
    ; TiffSaveable[PixelCMYK8]
    ; TiffSaveable[PixelYCbCr8]
    ; TiffSaveable[PixelRGB16]
    ; TiffSaveable[PixelRGB8]
    ; TiffSaveable[PixelYA16]
    ; TiffSaveable[PixelYA8]
    ; TiffSaveable[PixelF]
    ; TiffSaveable[Pixel32]
    ; TiffSaveable[Pixel16]
    ; TiffSaveable[Pixel8]

    ; decodeTiff
    ; encodeTiff
    ; writeTiff
    )

postulate
    readTiff : List Char → IO (Either (List Char) DynamicImage)

{-# COMPILE GHC readTiff = Codec.Picture.readTiff #-}


open import Ffi.Hs.Codec.Picture.HDR public
    using
    ( decodeHDR
    ; encodeHDR
    ; writeHDR
    )

postulate
    readHDR : List Char → IO (Either (List Char) DynamicImage)

{-# COMPILE GHC readHDR = Codec.Picture.readHDR #-}


open import Ffi.Hs.Codec.Picture.ColorQuant public
    using
    ( PaletteCreationMethod
    ; MedianMeanCut
    ; Uniform

    ; PaletteOptions
    ; mkPaletteOptions

    ; palettize
    )

postulate
    imageFromUnsafePtr : ∀{A : Set} → ⦃ Pixel A ⦄ → ⦃ PixelBaseComponent A ~ Word8 ⦄ → Int → Int → ForeignPtr Word8 → Image A

{-# COMPILE GHC imageFromUnsafePtr = \ a AgdaPixel AgdaTypeEq -> Codec.Picture.imageFromUnsafePtr #-}
