{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Metadata where

open import Ffi.Hs.-base.Class using (Eq; Show; Semigroup; Monoid)
open import Ffi.Hs.Control.DeepSeq using (NFData)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Ffi.Hs.Data.Word using (Word)
open import Ffi.Hs.Codec.Picture.Metadata.Exif using (ExifTag; ExifData)
open import Ffi.Hs.GHC.Float using (Double)
open import Ffi.Hs.Data.ByteString using (ByteString)
open import Ffi.Hs.Data.Int using (Int)
open import Agda.Primitive

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

data Value : Set where
    mkInt    : Int → Value
    mkDouble : Double → Value
    mkString : List Char → Value

postulate
    Eq[Value]     : Eq Value
    Show[Value]   : Show Value
    NFData[Value] : NFData Value


data SourceFormat : Set where
    SourceJpeg : SourceFormat
    SourceGif : SourceFormat
    SourceBitmap : SourceFormat
    SourceTiff : SourceFormat
    SourcePng : SourceFormat
    SourceHDR : SourceFormat
    SourceTGA : SourceFormat

postulate
    Eq[SourceFormat]     : Eq SourceFormat
    Show[SourceFormat]   : Show SourceFormat
    NFData[SourceFormat] : NFData SourceFormat

data ColorSpace : Set where
    SRGB : ColorSpace
    WindowsBitmapColorSpace : ByteString → ColorSpace
    ICCProfile : ByteString → ColorSpace

postulate
    Eq[ColorSpace]     : Eq ColorSpace
    Show[ColorSpace]   : Show ColorSpace
    NFData[ColorSpace] : NFData ColorSpace

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

postulate
    Eq[Keys[A]]   : Eq (Keys A)
    Show[Keys[A]] : Show (Keys A)

postulate
    Elem : (Set → Set₁) → Set
    _:=>_ : ∀{K} → ⦃ Show A ⦄ → ⦃ NFData A ⦄ → K A → A → Elem K

    Show[Elem[Keys]]   : Show (Elem Keys)
    NFData[Elem[Keys]] : NFData (Elem Keys)
