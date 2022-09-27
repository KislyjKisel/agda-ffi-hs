{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Metadata.Exif where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.List      using (List)
open import Ffi.Hs.-base.Class     using (Eq; Ord; Show)
open import Ffi.Hs.Control.DeepSeq using (NFData)
open import Ffi.Hs.Data.ByteString using (ByteString)
open import Ffi.Hs.Data.Int        using (Int32)
open import Ffi.Hs.Data.Tuple      using (Tuple2)
open import Ffi.Hs.Data.Vector     using (Vector)
open import Ffi.Hs.Data.Word       using (Word16; Word32)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Codec.Picture.Metadata.Exif
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData(AgdaNFData))
#-}

data ExifTag : Set where
    TagPhotometricInterpretation   : ExifTag
    TagCompression                 : ExifTag
    TagImageWidth                  : ExifTag
    TagImageLength                 : ExifTag
    TagXResolution                 : ExifTag
    TagYResolution                 : ExifTag
    TagResolutionUnit              : ExifTag
    TagRowPerStrip                 : ExifTag
    TagStripByteCounts             : ExifTag
    TagStripOffsets                : ExifTag
    TagBitsPerSample               : ExifTag
    TagColorMap                    : ExifTag
    TagTileWidth                   : ExifTag
    TagTileLength                  : ExifTag
    TagTileOffset                  : ExifTag
    TagTileByteCount               : ExifTag
    TagSamplesPerPixel             : ExifTag
    TagArtist                      : ExifTag
    TagDocumentName                : ExifTag
    TagSoftware                    : ExifTag
    TagPlanarConfiguration         : ExifTag
    TagOrientation                 : ExifTag
    TagSampleFormat                : ExifTag
    TagInkSet                      : ExifTag
    TagSubfileType                 : ExifTag
    TagFillOrder                   : ExifTag
    TagYCbCrCoeff                  : ExifTag
    TagYCbCrSubsampling            : ExifTag
    TagYCbCrPositioning            : ExifTag
    TagReferenceBlackWhite         : ExifTag
    TagXPosition                   : ExifTag
    TagYPosition                   : ExifTag
    TagExtraSample                 : ExifTag
    TagImageDescription            : ExifTag
    TagPredictor                   : ExifTag
    TagCopyright                   : ExifTag
    TagMake                        : ExifTag
    TagModel                       : ExifTag
    TagDateTime                    : ExifTag
    TagGPSInfo                     : ExifTag
    TagLightSource                 : ExifTag
    TagFlash                       : ExifTag
    TagJpegProc                    : ExifTag
    TagJPEGInterchangeFormat       : ExifTag
    TagJPEGInterchangeFormatLength : ExifTag
    TagJPEGRestartInterval         : ExifTag
    TagJPEGLosslessPredictors      : ExifTag
    TagJPEGPointTransforms         : ExifTag
    TagJPEGQTables                 : ExifTag
    TagJPEGDCTables                : ExifTag
    TagJPEGACTables                : ExifTag
    TagExifOffset                  : ExifTag
    TagUnknown                     : Word16 → ExifTag

{-# COMPILE GHC ExifTag = data Codec.Picture.Metadata.Exif.ExifTag
    ( Codec.Picture.Metadata.Exif.TagPhotometricInterpretation
    | Codec.Picture.Metadata.Exif.TagCompression
    | Codec.Picture.Metadata.Exif.TagImageWidth
    | Codec.Picture.Metadata.Exif.TagImageLength
    | Codec.Picture.Metadata.Exif.TagXResolution
    | Codec.Picture.Metadata.Exif.TagYResolution
    | Codec.Picture.Metadata.Exif.TagResolutionUnit
    | Codec.Picture.Metadata.Exif.TagRowPerStrip
    | Codec.Picture.Metadata.Exif.TagStripByteCounts
    | Codec.Picture.Metadata.Exif.TagStripOffsets
    | Codec.Picture.Metadata.Exif.TagBitsPerSample
    | Codec.Picture.Metadata.Exif.TagColorMap
    | Codec.Picture.Metadata.Exif.TagTileWidth
    | Codec.Picture.Metadata.Exif.TagTileLength
    | Codec.Picture.Metadata.Exif.TagTileOffset
    | Codec.Picture.Metadata.Exif.TagTileByteCount
    | Codec.Picture.Metadata.Exif.TagSamplesPerPixel
    | Codec.Picture.Metadata.Exif.TagArtist
    | Codec.Picture.Metadata.Exif.TagDocumentName
    | Codec.Picture.Metadata.Exif.TagSoftware
    | Codec.Picture.Metadata.Exif.TagPlanarConfiguration
    | Codec.Picture.Metadata.Exif.TagOrientation
    | Codec.Picture.Metadata.Exif.TagSampleFormat
    | Codec.Picture.Metadata.Exif.TagInkSet
    | Codec.Picture.Metadata.Exif.TagSubfileType
    | Codec.Picture.Metadata.Exif.TagFillOrder
    | Codec.Picture.Metadata.Exif.TagYCbCrCoeff
    | Codec.Picture.Metadata.Exif.TagYCbCrSubsampling
    | Codec.Picture.Metadata.Exif.TagYCbCrPositioning
    | Codec.Picture.Metadata.Exif.TagReferenceBlackWhite
    | Codec.Picture.Metadata.Exif.TagXPosition
    | Codec.Picture.Metadata.Exif.TagYPosition
    | Codec.Picture.Metadata.Exif.TagExtraSample
    | Codec.Picture.Metadata.Exif.TagImageDescription
    | Codec.Picture.Metadata.Exif.TagPredictor
    | Codec.Picture.Metadata.Exif.TagCopyright
    | Codec.Picture.Metadata.Exif.TagMake
    | Codec.Picture.Metadata.Exif.TagModel
    | Codec.Picture.Metadata.Exif.TagDateTime
    | Codec.Picture.Metadata.Exif.TagGPSInfo
    | Codec.Picture.Metadata.Exif.TagLightSource
    | Codec.Picture.Metadata.Exif.TagFlash
    | Codec.Picture.Metadata.Exif.TagJpegProc
    | Codec.Picture.Metadata.Exif.TagJPEGInterchangeFormat
    | Codec.Picture.Metadata.Exif.TagJPEGInterchangeFormatLength
    | Codec.Picture.Metadata.Exif.TagJPEGRestartInterval
    | Codec.Picture.Metadata.Exif.TagJPEGLosslessPredictors
    | Codec.Picture.Metadata.Exif.TagJPEGPointTransforms
    | Codec.Picture.Metadata.Exif.TagJPEGQTables
    | Codec.Picture.Metadata.Exif.TagJPEGDCTables
    | Codec.Picture.Metadata.Exif.TagJPEGACTables
    | Codec.Picture.Metadata.Exif.TagExifOffset
    | Codec.Picture.Metadata.Exif.TagUnknown
    ) #-}

postulate
    Eq[ExifTag]     : Eq ExifTag
    Ord[ExifTag]    : Ord ExifTag
    Show[ExifTag]   : Show ExifTag
    NFData[ExifTag] : NFData ExifTag

{-# COMPILE GHC Eq[ExifTag]     = AgdaEq     #-}
{-# COMPILE GHC Ord[ExifTag]    = AgdaOrd    #-}
{-# COMPILE GHC Show[ExifTag]   = AgdaShow   #-}
{-# COMPILE GHC NFData[ExifTag] = AgdaNFData #-}

data ExifData : Set where
    ExifNone           : ExifData
    ExifLong           : Word32 → ExifData
    ExifShort          : Word16 → ExifData
    ExifString         : ByteString → ExifData
    ExifUndefined      : ByteString → ExifData
    ExifShorts         : Vector Word16 → ExifData
    ExifLongs          : Vector Word32 → ExifData
    ExifRational       : Word32 → Word32 → ExifData
    ExifSignedRational : Int32 → Int32 → ExifData
    ExifIFD            : List (Tuple2 ExifTag ExifData) → ExifData

{-# COMPILE GHC ExifData = data Codec.Picture.Metadata.Exif.ExifData
    ( Codec.Picture.Metadata.Exif.ExifNone
    | Codec.Picture.Metadata.Exif.ExifLong
    | Codec.Picture.Metadata.Exif.ExifShort
    | Codec.Picture.Metadata.Exif.ExifString
    | Codec.Picture.Metadata.Exif.ExifUndefined
    | Codec.Picture.Metadata.Exif.ExifShorts
    | Codec.Picture.Metadata.Exif.ExifLongs
    | Codec.Picture.Metadata.Exif.ExifRational
    | Codec.Picture.Metadata.Exif.ExifSignedRational
    | Codec.Picture.Metadata.Exif.ExifIFD
    ) #-}

postulate
    Show[ExifData] : Show ExifData
    NFData[ExifData] : NFData ExifData

{-# COMPILE GHC Show[ExifData]   = AgdaShow   #-}
{-# COMPILE GHC NFData[ExifData] = AgdaNFData #-}

postulate
    tagOfWord16 : Word16 → ExifTag
    word16OfTag : ExifTag → Word16
    isInIFD0    : ExifTag → Bool

{-# COMPILE GHC tagOfWord16 = Codec.Picture.Metadata.Exif.tagOfWord16 #-}
{-# COMPILE GHC word16OfTag = Codec.Picture.Metadata.Exif.word16OfTag #-}
{-# COMPILE GHC isInIFD0    = Codec.Picture.Metadata.Exif.isInIFD0    #-}
