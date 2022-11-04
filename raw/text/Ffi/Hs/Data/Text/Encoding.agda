{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Text.Encoding where

open import Agda.Builtin.String             using () renaming (String to Text)
open import Ffi.Hs.-base.Class              using (Show)
open import Ffi.Hs.Data.Binary.Builder      using (Builder)
open import Ffi.Hs.Data.ByteString          using (ByteString)
open import Ffi.Hs.Data.Either              using (Either)
open import Ffi.Hs.Data.Text.Encoding.Error using (UnicodeException; OnDecodeError)

{-# FOREIGN GHC
import qualified Data.Text.Encoding
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaShow(AgdaShow))
#-}

data Decoding : Set where
    Some : Text → ByteString → (ByteString → Decoding) → Decoding

{-# COMPILE GHC Decoding = data Data.Text.Encoding.Decoding
    ( Data.Text.Encoding.Some
    ) #-}

postulate
    Show[Decoding] : Show Decoding

{-# COMPILE GHC Show[Decoding] = AgdaShow #-}

postulate
    decodeLatin1             : ByteString → Text
    decodeUtf8Lenient        : ByteString → Text
    decodeUtf8'              : ByteString → Either UnicodeException Text
    decodeUtf8With           : OnDecodeError → ByteString → Text
    decodeUtf16LEWith        : OnDecodeError → ByteString → Text
    decodeUtf16BEWith        : OnDecodeError → ByteString → Text
    decodeUtf32LEWith        : OnDecodeError → ByteString → Text
    decodeUtf32BEWith        : OnDecodeError → ByteString → Text
    streamDecodeUtf8With     : OnDecodeError → ByteString → Decoding
    decodeASCII              : ByteString → Text
    decodeUtf8               : ByteString → Text
    decodeUtf16LE            : ByteString → Text
    decodeUtf16BE            : ByteString → Text
    decodeUtf32LE            : ByteString → Text
    decodeUtf32BE            : ByteString → Text
    streamDecodeUtf8         : ByteString → Decoding
    encodeUtf8               : Text → ByteString
    encodeUtf16LE            : Text → ByteString
    encodeUtf16BE            : Text → ByteString
    encodeUtf32LE            : Text → ByteString
    encodeUtf32BE            : Text → ByteString
    encodeUtf8Builder        : Text → Builder
    -- todo: encodeUtf8BuilderEscaped : BoundedPrim Word8 → Text → Builder

{-# COMPILE GHC decodeLatin1         = Data.Text.Encoding.decodeLatin1         #-}
{-# COMPILE GHC decodeUtf8Lenient    = Data.Text.Encoding.decodeUtf8Lenient    #-}
{-# COMPILE GHC decodeUtf8'          = Data.Text.Encoding.decodeUtf8'          #-}
{-# COMPILE GHC decodeUtf8With       = Data.Text.Encoding.decodeUtf8With       #-}
{-# COMPILE GHC decodeUtf16LEWith    = Data.Text.Encoding.decodeUtf16LEWith    #-}
{-# COMPILE GHC decodeUtf16BEWith    = Data.Text.Encoding.decodeUtf16BEWith    #-}
{-# COMPILE GHC decodeUtf32LEWith    = Data.Text.Encoding.decodeUtf32LEWith    #-}
{-# COMPILE GHC decodeUtf32BEWith    = Data.Text.Encoding.decodeUtf32BEWith    #-}
{-# COMPILE GHC streamDecodeUtf8With = Data.Text.Encoding.streamDecodeUtf8With #-}
{-# COMPILE GHC decodeASCII          = Data.Text.Encoding.decodeASCII          #-}
{-# COMPILE GHC decodeUtf8           = Data.Text.Encoding.decodeUtf8           #-}
{-# COMPILE GHC decodeUtf16LE        = Data.Text.Encoding.decodeUtf16LE        #-}
{-# COMPILE GHC decodeUtf16BE        = Data.Text.Encoding.decodeUtf16BE        #-}
{-# COMPILE GHC decodeUtf32LE        = Data.Text.Encoding.decodeUtf32LE        #-}
{-# COMPILE GHC decodeUtf32BE        = Data.Text.Encoding.decodeUtf32BE        #-}
{-# COMPILE GHC streamDecodeUtf8     = Data.Text.Encoding.streamDecodeUtf8     #-}
{-# COMPILE GHC encodeUtf8           = Data.Text.Encoding.encodeUtf8           #-}
{-# COMPILE GHC encodeUtf16LE        = Data.Text.Encoding.encodeUtf16LE        #-}
{-# COMPILE GHC encodeUtf16BE        = Data.Text.Encoding.encodeUtf16BE        #-}
{-# COMPILE GHC encodeUtf32LE        = Data.Text.Encoding.encodeUtf32LE        #-}
{-# COMPILE GHC encodeUtf32BE        = Data.Text.Encoding.encodeUtf32BE        #-}
{-# COMPILE GHC encodeUtf8Builder    = Data.Text.Encoding.encodeUtf8Builder    #-}
