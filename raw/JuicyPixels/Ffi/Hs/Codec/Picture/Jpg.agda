{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Jpg where

open import Agda.Builtin.Char             using (Char)
open import Agda.Builtin.List             using (List)
open import Agda.Primitive                using (Level)
open import Ffi.Hs.Codec.Picture.Metadata
open import Ffi.Hs.Codec.Picture.Types
open import Ffi.Hs.Data.ByteString        using (StrictByteString)
open import Ffi.Hs.Data.ByteString.Lazy   using (LazyByteString)
open import Ffi.Hs.Data.Either            using (Either)
open import Ffi.Hs.Data.Tuple             using (Tuple2)
open import Ffi.Hs.Data.Type.Equality     using (_~_)
open import Ffi.Hs.Data.Word              using (Word8)

{-# FOREIGN GHC
import qualified Codec.Picture.Jpg
import MAlonzo.Code.Ffi.Hs.Data.Type.Equality (AgdaTypeEq(AgdaTypeEq))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    JpgEncodable : Set aℓ → Set aℓ

    JpgEncodable[A]⇒Pixel[A]                    : ⦃ JpgEncodable A ⦄ → Pixel A
    JpgEncodable[A]⇒PixelBaseComponent[A]~Word8 : ⦃ JpgEncodable A ⦄ → PixelBaseComponent A ~ Word8

    JpgEncodable[PixelCMYK8]  : JpgEncodable PixelCMYK8
    JpgEncodable[PixelYCbCr8] : JpgEncodable PixelYCbCr8
    JpgEncodable[PixelRGB8]   : JpgEncodable PixelRGB8
    JpgEncodable[Pixel8]      : JpgEncodable Pixel8

    decodeJpeg                            : StrictByteString → Either (List Char) DynamicImage
    decodeJpegWithMetadata                : StrictByteString → Either (List Char) (Tuple2 DynamicImage Metadatas)
    encodeJpegAtQuality                   : Word8 → Image PixelYCbCr8 → LazyByteString
    encodeJpegAtQualityWithMetadata       : Word8 → Metadatas → Image PixelYCbCr8 → LazyByteString
    encodeDirectJpegAtQualityWithMetadata : ⦃ JpgEncodable A ⦄ → Word8 → Metadatas → Image A → LazyByteString
    encodeJpeg                            : Image PixelYCbCr8 → LazyByteString

{-# FOREIGN GHC data AgdaJpgEncodable aℓ a = Codec.Picture.Jpg.JpgEncodable a => AgdaJpgEncodable #-}
{-# COMPILE GHC JpgEncodable = type(0) AgdaJpgEncodable #-}

{-# COMPILE GHC JpgEncodable[A]⇒Pixel[A]                     = \ aℓ a AgdaJpgEncodable -> AgdaPixel  #-}
{-# COMPILE GHC JpgEncodable[A]⇒PixelBaseComponent[A]~Word8  = \ aℓ a AgdaJpgEncodable -> AgdaTypeEq #-}

{-# COMPILE GHC JpgEncodable[PixelCMYK8]  = AgdaJpgEncodable #-}
{-# COMPILE GHC JpgEncodable[PixelYCbCr8] = AgdaJpgEncodable #-}
{-# COMPILE GHC JpgEncodable[PixelRGB8]   = AgdaJpgEncodable #-}
{-# COMPILE GHC JpgEncodable[Pixel8]      = AgdaJpgEncodable #-}

{-# COMPILE GHC decodeJpeg                            =                            Codec.Picture.Jpg.decodeJpeg                            #-}
{-# COMPILE GHC decodeJpegWithMetadata                =                            Codec.Picture.Jpg.decodeJpegWithMetadata                #-}
{-# COMPILE GHC encodeJpegAtQuality                   =                            Codec.Picture.Jpg.encodeJpegAtQuality                   #-}
{-# COMPILE GHC encodeJpegAtQualityWithMetadata       =                            Codec.Picture.Jpg.encodeJpegAtQualityWithMetadata       #-}
{-# COMPILE GHC encodeDirectJpegAtQualityWithMetadata = \ aℓ a AgdaJpgEncodable -> Codec.Picture.Jpg.encodeDirectJpegAtQualityWithMetadata #-}
{-# COMPILE GHC encodeJpeg                            =                            Codec.Picture.Jpg.encodeJpeg                            #-}
