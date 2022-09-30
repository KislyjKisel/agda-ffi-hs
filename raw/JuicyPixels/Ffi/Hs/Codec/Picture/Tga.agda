{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Tga where

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
import qualified Codec.Picture.Tga
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    TgaSaveable : Set aℓ → Set aℓ

    TgaSaveable[PixelRGBA8] : TgaSaveable PixelRGBA8
    TgaSaveable[PixelRGB8]  : TgaSaveable PixelRGB8
    TgaSaveable[Pixel8]     : TgaSaveable Pixel8

    decodeTga                       : StrictByteString → Either (List Char) DynamicImage
    decodeTgaWithMetadata           : StrictByteString → Either (List Char) (Tuple2 DynamicImage Metadatas)
    decodeTgaWithPaletteAndMetadata : StrictByteString → Either (List Char) (Tuple2 PalettedImage Metadatas)
    encodeTga                       : ⦃ TgaSaveable A ⦄ → Image A → LazyByteString
    writeTga                        : ⦃ TgaSaveable A ⦄ → List Char → Image A → IO ⊤

{-# FOREIGN GHC data AgdaTgaSaveable aℓ a = Codec.Picture.Tga.TgaSaveable a => AgdaTgaSaveable #-}
{-# COMPILE GHC TgaSaveable = type(0) AgdaTgaSaveable #-}

{-# COMPILE GHC TgaSaveable[PixelRGBA8] = AgdaTgaSaveable #-}
{-# COMPILE GHC TgaSaveable[PixelRGB8]  = AgdaTgaSaveable #-}
{-# COMPILE GHC TgaSaveable[Pixel8]     = AgdaTgaSaveable #-}

{-# COMPILE GHC decodeTga                       =                           Codec.Picture.Tga.decodeTga                       #-}
{-# COMPILE GHC decodeTgaWithMetadata           =                           Codec.Picture.Tga.decodeTgaWithMetadata           #-}
{-# COMPILE GHC decodeTgaWithPaletteAndMetadata =                           Codec.Picture.Tga.decodeTgaWithPaletteAndMetadata #-}
{-# COMPILE GHC encodeTga                       = \ aℓ a AgdaTgaSaveable -> Codec.Picture.Tga.encodeTga                       #-}
{-# COMPILE GHC writeTga                        = \ aℓ a AgdaTgaSaveable -> Codec.Picture.Tga.writeTga                        #-}
