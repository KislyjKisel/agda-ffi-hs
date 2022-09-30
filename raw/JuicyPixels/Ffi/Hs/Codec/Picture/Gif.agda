{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Gif where

open import Agda.Builtin.Char             using (Char)
open import Agda.Builtin.IO               using (IO)
open import Agda.Builtin.List             using (List)
open import Agda.Builtin.Maybe            using (Maybe)
open import Ffi.Hs.-base.Unit             using (⊤)
open import Ffi.Hs.Codec.Picture.Metadata using (Metadatas)
open import Ffi.Hs.Codec.Picture.Types
open import Ffi.Hs.Data.ByteString        using (StrictByteString)
open import Ffi.Hs.Data.ByteString.Lazy   using (LazyByteString)
open import Ffi.Hs.Data.Either            using (Either)
open import Ffi.Hs.Data.Int               using (Int)
open import Ffi.Hs.Data.Tuple             using (Tuple2; Tuple3)
open import Ffi.Hs.Data.Word              using (Word8; Word16)

{-# FOREIGN GHC
import qualified Codec.Picture.Gif
#-}


GifDelay : Set
GifDelay = Int


postulate
    decodeGif                       : StrictByteString → Either (List Char) DynamicImage
    decodeGifWithMetadata           : StrictByteString → Either (List Char) (Tuple2 DynamicImage Metadatas)
    decodeGifWithPaletteAndMetadata : StrictByteString → Either (List Char) (Tuple2 PalettedImage Metadatas)
    decodeGifImages                 : StrictByteString → Either (List Char) (List DynamicImage)
    getDelaysGifImages              : StrictByteString → Either (List Char) (List GifDelay)

{-# COMPILE GHC decodeGif                       = Codec.Picture.Gif.decodeGif                       #-}
{-# COMPILE GHC decodeGifWithMetadata           = Codec.Picture.Gif.decodeGifWithMetadata           #-}
{-# COMPILE GHC decodeGifWithPaletteAndMetadata = Codec.Picture.Gif.decodeGifWithPaletteAndMetadata #-}
{-# COMPILE GHC decodeGifImages                 = Codec.Picture.Gif.decodeGifImages                 #-}
{-# COMPILE GHC getDelaysGifImages              = Codec.Picture.Gif.getDelaysGifImages              #-}


data GifDisposalMethod : Set where
    DisposalAny               : GifDisposalMethod
    DisposalDoNot             : GifDisposalMethod
    DisposalRestoreBackground : GifDisposalMethod
    DisposalRestorePrevious   : GifDisposalMethod
    DisposalUnknown           : Word8 → GifDisposalMethod

{-# COMPILE GHC GifDisposalMethod = data Codec.Picture.Gif.GifDisposalMethod
    ( Codec.Picture.Gif.DisposalAny
    | Codec.Picture.Gif.DisposalDoNot
    | Codec.Picture.Gif.DisposalRestoreBackground
    | Codec.Picture.Gif.DisposalRestorePrevious
    | Codec.Picture.Gif.DisposalUnknown
    ) #-}


record GifFrame : Set where
    constructor mkGifFrame
    field
        gfXOffset     : Int
        gfYOffset     : Int
        gfPalette     : Maybe Palette
        gfTransparent : Maybe Int
        gfDelay       : GifDelay
        gfDisposal    : GifDisposalMethod
        gfPixels      : Image Pixel8

{-# COMPILE GHC GifFrame = data Codec.Picture.Gif.GifFrame (Codec.Picture.Gif.GifFrame) #-}


data GifLooping : Set where
    LoopingNever   : GifLooping
    LoopingForever : GifLooping
    LoopingRepeat  : Word16 → GifLooping

{-# COMPILE GHC GifLooping = data Codec.Picture.Gif.GifLooping
    ( Codec.Picture.Gif.LoopingNever
    | Codec.Picture.Gif.LoopingForever
    | Codec.Picture.Gif.LoopingRepeat
    ) #-}


record GifEncode : Set where
    constructor mkGifEncode
    field
        geWidth      : Int
        geHeight     : Int
        gePalette    : Maybe Palette
        geBackground : Maybe Int
        geLooping    : GifLooping
        geFrames     : List GifFrame

{-# COMPILE GHC GifEncode = data Codec.Picture.Gif.GifEncode (Codec.Picture.Gif.GifEncode) #-}


postulate
    encodeGifImage            : Image Pixel8 → LazyByteString
    encodeGifImageWithPalette : Image Pixel8 → Palette → Either (List Char) LazyByteString
    encodeGifImages           : GifLooping → List (Tuple3 Palette GifDelay (Image Pixel8)) → Either (List Char) LazyByteString
    encodeComplexGifImage     : GifEncode → Either (List Char) LazyByteString
    writeGifImage             : List Char → Image Pixel8 → IO ⊤
    writeGifImageWithPalette  : List Char → Image Pixel8 → Palette → Either (List Char) (IO ⊤)
    writeGifImages            : List Char → GifLooping → List (Tuple3 Palette GifDelay (Image Pixel8)) → Either (List Char) (IO ⊤)
    writeComplexGifImage      : List Char → GifEncode → Either (List Char) (IO ⊤)
    greyPalette               : Palette

{-# COMPILE GHC encodeGifImage            = Codec.Picture.Gif.encodeGifImage            #-}
{-# COMPILE GHC encodeGifImageWithPalette = Codec.Picture.Gif.encodeGifImageWithPalette #-}
{-# COMPILE GHC encodeGifImages           = Codec.Picture.Gif.encodeGifImages           #-}
{-# COMPILE GHC encodeComplexGifImage     = Codec.Picture.Gif.encodeComplexGifImage     #-}
{-# COMPILE GHC writeGifImage             = Codec.Picture.Gif.writeGifImage             #-}
{-# COMPILE GHC writeGifImageWithPalette  = Codec.Picture.Gif.writeGifImageWithPalette  #-}
{-# COMPILE GHC writeGifImages            = Codec.Picture.Gif.writeGifImages            #-}
{-# COMPILE GHC writeComplexGifImage      = Codec.Picture.Gif.writeComplexGifImage      #-}
{-# COMPILE GHC greyPalette               = Codec.Picture.Gif.greyPalette               #-}
