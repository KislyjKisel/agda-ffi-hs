{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.HDR where

open import Agda.Builtin.Char             using (Char)
open import Agda.Builtin.IO               using (IO)
open import Agda.Builtin.List             using (List)
open import Ffi.Hs.-base.Unit             using (⊤)
open import Ffi.Hs.Codec.Picture.Metadata using (Metadatas)
open import Ffi.Hs.Codec.Picture.Types
open import Ffi.Hs.Data.ByteString        using (StrictByteString)
open import Ffi.Hs.Data.ByteString.Lazy   using (LazyByteString)
open import Ffi.Hs.Data.Either            using (Either)
open import Ffi.Hs.Data.Tuple             using (Tuple2)

{-# FOREIGN GHC
import qualified Codec.Picture.HDR
#-}

postulate
    decodeHDR             : StrictByteString → Either (List Char) DynamicImage
    decodeHDRWithMetadata : StrictByteString → Either (List Char) (Tuple2 DynamicImage Metadatas)
    encodeHDR             : Image PixelRGBF → LazyByteString
    encodeRawHDR          : Image PixelRGBF → LazyByteString
    encodeRLENewStyleHDR  : Image PixelRGBF → LazyByteString
    writeHDR              : List Char → Image PixelRGBF → IO ⊤
    writeRLENewStyleHDR   : List Char → Image PixelRGBF → IO ⊤

{-# COMPILE GHC decodeHDR             = Codec.Picture.HDR.decodeHDR             #-}
{-# COMPILE GHC decodeHDRWithMetadata = Codec.Picture.HDR.decodeHDRWithMetadata #-}
{-# COMPILE GHC encodeHDR             = Codec.Picture.HDR.encodeHDR             #-}
{-# COMPILE GHC encodeRawHDR          = Codec.Picture.HDR.encodeRawHDR          #-}
{-# COMPILE GHC encodeRLENewStyleHDR  = Codec.Picture.HDR.encodeRLENewStyleHDR  #-}
{-# COMPILE GHC writeHDR              = Codec.Picture.HDR.writeHDR              #-}
{-# COMPILE GHC writeRLENewStyleHDR   = Codec.Picture.HDR.writeRLENewStyleHDR   #-}
