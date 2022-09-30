{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Metadata where

open import Agda.Builtin.Char                  using (Char)
open import Agda.Builtin.List                  using (List)
open import Agda.Builtin.Maybe                 using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Codec.Picture.Metadata.Exif using (ExifTag; ExifData)
open import Ffi.Hs.Control.DeepSeq             using (NFData)
open import Ffi.Hs.Data.ByteString             using (ByteString)
open import Ffi.Hs.Data.Int                    using (Int)
open import Ffi.Hs.Data.Tuple                  using (Tuple2)
open import Ffi.Hs.Data.Word                   using (Word)
open import Ffi.Hs.GHC.Float                   using (Double)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Codec.Picture.Metadata
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Metadatas : Set

    Show[Metadatas]      : Show Metadatas
    Semigroup[Metadatas] : Semigroup Metadatas
    Monoid[Metadatas]    : Monoid Metadatas
    NFData[Metadatas]    : NFData Metadatas

{-# COMPILE GHC Metadatas = type Codec.Picture.Metadata.Metadatas #-}

{-# COMPILE GHC Show[Metadatas]      = AgdaShow      #-}
{-# COMPILE GHC Semigroup[Metadatas] = AgdaSemigroup #-}
{-# COMPILE GHC Monoid[Metadatas]    = AgdaMonoid    #-}
{-# COMPILE GHC NFData[Metadatas]    = AgdaNFData    #-}


data Value : Set where
    mkInt    : Int → Value
    mkDouble : Double → Value
    mkString : List Char → Value

{-# COMPILE GHC Value = data Codec.Picture.Metadata.Value
    ( Codec.Picture.Metadata.Int
    | Codec.Picture.Metadata.Double
    | Codec.Picture.Metadata.String
    ) #-}

postulate
    Eq[Value]     : Eq Value
    Show[Value]   : Show Value
    NFData[Value] : NFData Value

{-# COMPILE GHC Eq[Value]     = AgdaEq     #-}
{-# COMPILE GHC Show[Value]   = AgdaShow   #-}
{-# COMPILE GHC NFData[Value] = AgdaNFData #-}


data SourceFormat : Set where
    SourceJpeg   : SourceFormat
    SourceGif    : SourceFormat
    SourceBitmap : SourceFormat
    SourceTiff   : SourceFormat
    SourcePng    : SourceFormat
    SourceHDR    : SourceFormat
    SourceTGA    : SourceFormat

{-# COMPILE GHC SourceFormat = data Codec.Picture.Metadata.SourceFormat
    ( Codec.Picture.Metadata.SourceJpeg
    | Codec.Picture.Metadata.SourceGif
    | Codec.Picture.Metadata.SourceBitmap
    | Codec.Picture.Metadata.SourceTiff
    | Codec.Picture.Metadata.SourcePng
    | Codec.Picture.Metadata.SourceHDR
    | Codec.Picture.Metadata.SourceTGA
    ) #-}

postulate
    Eq[SourceFormat]     : Eq SourceFormat
    Show[SourceFormat]   : Show SourceFormat
    NFData[SourceFormat] : NFData SourceFormat

{-# COMPILE GHC Eq[SourceFormat]     = AgdaEq     #-}
{-# COMPILE GHC Show[SourceFormat]   = AgdaShow   #-}
{-# COMPILE GHC NFData[SourceFormat] = AgdaNFData #-}


data ColorSpace : Set where
    SRGB                    : ColorSpace
    WindowsBitmapColorSpace : ByteString → ColorSpace
    ICCProfile              : ByteString → ColorSpace

{-# COMPILE GHC ColorSpace = data Codec.Picture.Metadata.ColorSpace
    ( Codec.Picture.Metadata.SRGB
    | Codec.Picture.Metadata.WindowsBitmapColorSpace
    | Codec.Picture.Metadata.ICCProfile
    )  #-}

postulate
    Eq[ColorSpace]     : Eq ColorSpace
    Show[ColorSpace]   : Show ColorSpace
    NFData[ColorSpace] : NFData ColorSpace

{-# COMPILE GHC Eq[ColorSpace]     = AgdaEq     #-}
{-# COMPILE GHC Show[ColorSpace]   = AgdaShow   #-}
{-# COMPILE GHC NFData[ColorSpace] = AgdaNFData #-}


data Keys : Set → Set₁ where
    Gamma       : Keys Double
    mkColorSpace  : Keys ColorSpace
    Format      : Keys SourceFormat
    DpiX        : Keys Word
    DpiY        : Keys Word
    Width       : Keys Word
    Height      : Keys Word
    Title       : Keys (List Char)
    Description : Keys (List Char)
    Author      : Keys (List Char)
    Copyright   : Keys (List Char)
    Software    : Keys (List Char)
    Comment     : Keys (List Char)
    Disclaimer  : Keys (List Char)
    Source      : Keys (List Char)
    Warning     : Keys (List Char)
    Exif        : ExifTag → Keys ExifData
    Unknown     : List Char → Keys Value

{-# COMPILE GHC Keys = data Codec.Picture.Metadata.Keys
    ( Codec.Picture.Metadata.Gamma
    | Codec.Picture.Metadata.ColorSpace
    | Codec.Picture.Metadata.Format
    | Codec.Picture.Metadata.DpiX
    | Codec.Picture.Metadata.DpiY
    | Codec.Picture.Metadata.Width
    | Codec.Picture.Metadata.Height
    | Codec.Picture.Metadata.Title
    | Codec.Picture.Metadata.Description
    | Codec.Picture.Metadata.Author
    | Codec.Picture.Metadata.Copyright
    | Codec.Picture.Metadata.Software
    | Codec.Picture.Metadata.Comment
    | Codec.Picture.Metadata.Disclaimer
    | Codec.Picture.Metadata.Source
    | Codec.Picture.Metadata.Warning
    | Codec.Picture.Metadata.Exif
    | Codec.Picture.Metadata.Unknown
    ) #-}

postulate
    Eq[Keys[A]]   : Eq (Keys A)
    Show[Keys[A]] : Show (Keys A)

{-# COMPILE GHC Eq[Keys[A]]   = AgdaEq   #-}
{-# COMPILE GHC Show[Keys[A]] = AgdaShow #-}


postulate
    Elem : (Set → Set₁) → Set
    _:=>_ : ∀{K} → ⦃ Show A ⦄ → ⦃ NFData A ⦄ → K A → A → Elem K

    Show[Elem[Keys]]   : Show (Elem Keys)
    NFData[Elem[Keys]] : NFData (Elem Keys)

{-# COMPILE GHC Elem = type(0) Codec.Picture.Metadata.Elem #-}
{-# COMPILE GHC _:=>_ = \ k a AgdaShow AgdaNFData -> (Codec.Picture.Metadata.:=>) #-}

{-# COMPILE GHC Show[Elem[Keys]]   = AgdaShow   #-}
{-# COMPILE GHC NFData[Elem[Keys]] = AgdaNFData #-}


postulate
    lookup    : Keys A → Metadatas → Maybe A
    empty     : Metadatas
    insert    : ⦃ Show A ⦄ → ⦃ NFData A ⦄ → Keys A → A → Metadatas → Metadatas
    delete    : Keys A → Metadatas → Metadatas
    singleton : ⦃ Show A ⦄ → ⦃ NFData A ⦄ → Keys A → A → Metadatas

    foldl'  : (A → Elem Keys → A) → A → Metadatas → A
    foldMap : ⦃ Monoid A ⦄ → (Elem Keys → A) → Metadatas → A

    mkDpiMetadata    : Word → Metadatas
    mkSizeMetadata   : ⦃ Integral A ⦄ → A → A → Metadatas
    basicMetadata    : ∀{sℓ} {Size : Set sℓ} → ⦃ Integral Size ⦄ → SourceFormat → Size → Size → Metadatas
    simpleMetadata   : ∀{sℓ dℓ} {Size : Set sℓ} {Dpi : Set dℓ} → ⦃ Integral Size ⦄ → ⦃ Integral Dpi ⦄ → SourceFormat → Size → Size → Dpi → Dpi → Metadatas
    extractExifMetas : Metadatas → List (Tuple2 ExifTag ExifData)

    dotsPerMeterToDotPerInch      : Word → Word
    dotPerInchToDotsPerMeter      : Word → Word
    dotsPerCentiMeterToDotPerInch : Word → Word

{-# COMPILE GHC lookup    = \ a                     -> Codec.Picture.Metadata.lookup    #-}
{-# COMPILE GHC empty     =                            Codec.Picture.Metadata.empty     #-}
{-# COMPILE GHC insert    = \ a AgdaShow AgdaNFData -> Codec.Picture.Metadata.insert    #-}
{-# COMPILE GHC delete    = \ a                     -> Codec.Picture.Metadata.delete    #-}
{-# COMPILE GHC singleton = \ a AgdaShow AgdaNFData -> Codec.Picture.Metadata.singleton #-}

{-# COMPILE GHC foldl'  = \ aℓ a            -> Codec.Picture.Metadata.foldl'  #-}
{-# COMPILE GHC foldMap = \ aℓ a AgdaMonoid -> Codec.Picture.Metadata.foldMap #-}

{-# COMPILE GHC mkDpiMetadata    =                                                    Codec.Picture.Metadata.mkDpiMetadata    #-}
{-# COMPILE GHC mkSizeMetadata   = \ aℓ a AgdaIntegral                             -> Codec.Picture.Metadata.mkSizeMetadata   #-}
{-# COMPILE GHC basicMetadata    = \ sizeℓ size AgdaIntegral                       -> Codec.Picture.Metadata.basicMetadata    #-}
{-# COMPILE GHC simpleMetadata   = \ sizeℓ dpiℓ size dpi AgdaIntegral AgdaIntegral -> Codec.Picture.Metadata.simpleMetadata   #-}
{-# COMPILE GHC extractExifMetas =                                                    Codec.Picture.Metadata.extractExifMetas #-}

{-# COMPILE GHC dotsPerMeterToDotPerInch      = Codec.Picture.Metadata.dotsPerMeterToDotPerInch      #-}
{-# COMPILE GHC dotPerInchToDotsPerMeter      = Codec.Picture.Metadata.dotPerInchToDotsPerMeter      #-}
{-# COMPILE GHC dotsPerCentiMeterToDotPerInch = Codec.Picture.Metadata.dotsPerCentiMeterToDotPerInch #-}
