{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Bitmap where

open import Agda.Builtin.Bool             using (Bool)
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
import qualified Codec.Picture.Bitmap
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


postulate
    BmpEncodable : Set aℓ → Set aℓ

    BmpEncodable[PixelRGBA8] : BmpEncodable PixelRGBA8
    BmpEncodable[PixelRGB8]  : BmpEncodable PixelRGB8
    BmpEncodable[Pixel8]     : BmpEncodable Pixel8

    writeBitmap                        : ⦃ BmpEncodable A ⦄ → List Char → Image A → IO ⊤
    encodeBitmap                       : ⦃ BmpEncodable A ⦄ → Image A → LazyByteString
    encodeBitmapWithMetadata           : ⦃ BmpEncodable A ⦄ → Metadatas → Image A → LazyByteString
    decodeBitmap                       : StrictByteString → Either (List Char) DynamicImage
    decodeBitmapWithMetadata           : StrictByteString → Either (List Char) (Tuple2 DynamicImage Metadatas)
    decodeBitmapWithPaletteAndMetadata : StrictByteString → Either (List Char) (Tuple2 PalettedImage Metadatas)
    encodeDynamicBitmap                : DynamicImage → Either (List Char) LazyByteString
    -- todo: (idk) encodeBitmapWithPaletteAndMetadata : ⦃ BmpEncodable A ⦄ → Metadatas → BmpPalette → Image A → LazyByteString
    writeDynamicBitmap                 : List Char → DynamicImage → IO (Either (List Char) Bool)

{-# FOREIGN GHC data AgdaBmpEncodable aℓ a = Codec.Picture.Bitmap.BmpEncodable a => AgdaBmpEncodable #-}
{-# COMPILE GHC BmpEncodable = type(0) AgdaBmpEncodable #-}

{-# COMPILE GHC BmpEncodable[PixelRGBA8] = AgdaBmpEncodable #-}
{-# COMPILE GHC BmpEncodable[PixelRGB8]  = AgdaBmpEncodable #-}
{-# COMPILE GHC BmpEncodable[Pixel8]     = AgdaBmpEncodable #-}

{-# COMPILE GHC writeBitmap                        = \ aℓ a AgdaBmpEncodable -> Codec.Picture.Bitmap.writeBitmap                        #-}
{-# COMPILE GHC encodeBitmap                       = \ aℓ a AgdaBmpEncodable -> Codec.Picture.Bitmap.encodeBitmap                       #-}
{-# COMPILE GHC encodeBitmapWithMetadata           = \ aℓ a AgdaBmpEncodable -> Codec.Picture.Bitmap.encodeBitmapWithMetadata           #-}
{-# COMPILE GHC decodeBitmap                       =                            Codec.Picture.Bitmap.decodeBitmap                       #-}
{-# COMPILE GHC decodeBitmapWithMetadata           =                            Codec.Picture.Bitmap.decodeBitmapWithMetadata           #-}
{-# COMPILE GHC decodeBitmapWithPaletteAndMetadata =                            Codec.Picture.Bitmap.decodeBitmapWithPaletteAndMetadata #-}
{-# COMPILE GHC encodeDynamicBitmap                =                            Codec.Picture.Bitmap.encodeDynamicBitmap                #-}
{-# COMPILE GHC writeDynamicBitmap                 =                            Codec.Picture.Bitmap.writeDynamicBitmap                 #-}
