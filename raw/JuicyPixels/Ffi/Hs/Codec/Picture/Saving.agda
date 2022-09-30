{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Saving where

open import Agda.Builtin.Char           using (Char)
open import Agda.Builtin.List           using (List)
open import Ffi.Hs.Codec.Picture.Types  using (DynamicImage)
open import Ffi.Hs.Data.ByteString.Lazy using (ByteString)
open import Ffi.Hs.Data.Either          using (Either)
open import Ffi.Hs.Data.Int             using (Int)

{-# FOREIGN GHC
import qualified Codec.Picture.Saving
#-}

postulate
    imageToJpg      : Int → DynamicImage → ByteString
    imageToPng      : DynamicImage → ByteString
    imageToGif      : DynamicImage → Either (List Char) ByteString
    imageToBitmap   : DynamicImage → ByteString
    imageToTiff     : DynamicImage → ByteString
    imageToRadiance : DynamicImage → ByteString
    imageToTga      : DynamicImage → ByteString

{-# COMPILE GHC imageToJpg      = Codec.Picture.Saving.imageToJpg      #-}
{-# COMPILE GHC imageToPng      = Codec.Picture.Saving.imageToPng      #-}
{-# COMPILE GHC imageToGif      = Codec.Picture.Saving.imageToGif      #-}
{-# COMPILE GHC imageToBitmap   = Codec.Picture.Saving.imageToBitmap   #-}
{-# COMPILE GHC imageToTiff     = Codec.Picture.Saving.imageToTiff     #-}
{-# COMPILE GHC imageToRadiance = Codec.Picture.Saving.imageToRadiance #-}
{-# COMPILE GHC imageToTga      = Codec.Picture.Saving.imageToTga      #-}
