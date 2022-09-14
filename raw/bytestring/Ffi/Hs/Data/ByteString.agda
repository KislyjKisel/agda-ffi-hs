{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.ByteString where

open import Ffi.Hs.Data.Word using (Word8)
open import Agda.Builtin.List using (List)
open import Ffi.Hs.System.IO using (IO; FilePath)

postulate
    ByteString : Set

StrictByteString : Set
StrictByteString = ByteString

postulate
    empty : ByteString
    singleton : Word8 → ByteString
    pack : List Word8 → ByteString
    unpack : ByteString → List Word8
    fromStrict : ByteString → ByteString
    toStrict : ByteString → ByteString
    fromFilePath : FilePath → IO ByteString
    toFilePath : ByteString → IO FilePath

postulate
    cons   : Word8 → ByteString → ByteString
    snoc   : ByteString → Word8 → ByteString
    append : ByteString → ByteString → ByteString
    head   : ⦃ HasCallStack ⦄ → ByteString → Word8
    uncons : ByteString → Maybe (Tuple2 Word8 ByteString)
    last   : ⦃ HasCallStack ⦄ → ByteString → Word8
    tail   : ⦃ HasCallStack ⦄ → ByteString → ByteString
    init   : ⦃ HasCallStack ⦄ → ByteString → ByteString
    null   : ByteString → Bool
    length : ByteString → Int