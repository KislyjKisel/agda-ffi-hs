{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Metadata.Exif-Instanced where

open import Ffi.Hs.Codec.Picture.Metadata.Exif

instance
    inst:Eq[ExifTag]     = Eq[ExifTag]
    inst:Ord[ExifTag]    = Ord[ExifTag]
    inst:Show[ExifTag]   = Show[ExifTag]
    inst:NFData[ExifTag] = NFData[ExifTag]

    inst:Show[ExifData]   = Show[ExifData]
    inst:NFData[ExifData] = NFData[ExifData]
