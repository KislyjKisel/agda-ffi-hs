{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.IO.Buffer where

open import Agda.Builtin.Bool         using (Bool)
open import Agda.Builtin.Char         using (Char)
open import Agda.Builtin.IO           using (IO)
open import Agda.Builtin.Unit         using (⊤)
open import Agda.Primitive
open import Ffi.Hs.-base.Class        using (Eq)
open import Ffi.Hs.Data.Int           using (Int)
open import Ffi.Hs.Data.String        using (String)
open import Ffi.Hs.Data.Tuple         using (Tuple2)
open import Ffi.Hs.Data.Word          using (Word8; Word64)
open import Ffi.Hs.Foreign.ForeignPtr using (ForeignPtr)
open import Ffi.Hs.Foreign.Ptr        using (Ptr)

{-# FOREIGN GHC
import qualified GHC.IO.Buffer
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaEq(AgdaEq))
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ


RawBuffer : Set aℓ → Set aℓ
RawBuffer = ForeignPtr

CharBufElem : Set
CharBufElem = Char

RawCharBuffer : Set
RawCharBuffer = RawBuffer CharBufElem


data BufferState : Set where
    ReadBuffer  : BufferState
    WriteBuffer : BufferState

{-# COMPILE GHC BufferState = data GHC.IO.Buffer.BufferState
    ( GHC.IO.Buffer.ReadBuffer
    | GHC.IO.Buffer.WriteBuffer
    ) #-}

postulate
    Eq[BufferState] : Eq BufferState

{-# COMPILE GHC Eq[BufferState] = AgdaEq #-}


record Buffer (A : Set aℓ) : Set aℓ where
    constructor mkBuffer
    field
        bufRaw    : RawBuffer A
        bufState  : BufferState
        bufSize   : Int
        bufOffset : Word64
        bufL      : Int
        bufR      : Int

open Buffer public

{-# COMPILE GHC Buffer = data GHC.IO.Buffer.Buffer (GHC.IO.Buffer.Buffer) #-}


CharBuffer : Set
CharBuffer = Buffer Char


postulate
    newByteBuffer : Int → BufferState → IO (Buffer Word8)
    newCharBuffer : Int → BufferState → IO CharBuffer
    newBuffer     : Int → Int → BufferState → IO (Buffer A)
    emptyBuffer   : RawBuffer A → Int → BufferState → Buffer A

    bufferRemove       : Int → Buffer A → Buffer A
    bufferAdd          : Int → Buffer A → Buffer A
    slideContents      : Buffer Word8 → IO (Buffer Word8)
    bufferAdjustL      : Int → Buffer A → Buffer A
    bufferAddOffset    : Int → Buffer A → Buffer A
    bufferAdjustOffset : Word64 → Buffer A → Buffer A

    isEmptyBuffer    : Buffer A → Bool
    isFullBuffer     : Buffer A → Bool
    isFullCharBuffer : Buffer A → Bool
    isWriteBuffer    : Buffer A → Bool
    bufferElems      : Buffer A → Int
    bufferAvailable  : Buffer A → Int
    bufferOffset     : Buffer A → Word64
    summaryBuffer    : Buffer A → String

    withBuffer    : Buffer A → (Ptr A → IO B) → IO B
    withRawBuffer : RawBuffer A → (Ptr A → IO B) → IO B

    checkBuffer : Buffer A → IO ⊤

    readWord8Buf    : RawBuffer Word8 → Int → IO Word8
    writeWord8Buf   : RawBuffer Word8 → Int → Word8 → IO ⊤
    peekCharBuf     : RawCharBuffer → Int → IO Char
    readCharBuf     : RawCharBuffer → Int → IO (Tuple2 Char Int)
    writeCharBuf    : RawCharBuffer → Int → Char → IO Int
    readCharBufPtr  : Ptr CharBufElem → Int → IO (Tuple2 Char Int)
    writeCharBufPtr : Ptr CharBufElem → Int → Char → IO Int
    charSize        : Int

{-# COMPILE GHC newByteBuffer =           GHC.IO.Buffer.newByteBuffer #-}
{-# COMPILE GHC newCharBuffer =           GHC.IO.Buffer.newCharBuffer #-}
{-# COMPILE GHC newBuffer     = \ aℓ a -> GHC.IO.Buffer.newBuffer     #-}
{-# COMPILE GHC emptyBuffer   = \ aℓ a -> GHC.IO.Buffer.emptyBuffer   #-}

{-# COMPILE GHC bufferRemove       = \ aℓ a -> GHC.IO.Buffer.bufferRemove       #-}
{-# COMPILE GHC bufferAdd          = \ aℓ a -> GHC.IO.Buffer.bufferAdd          #-}
{-# COMPILE GHC slideContents      =           GHC.IO.Buffer.slideContents      #-}
{-# COMPILE GHC bufferAdjustL      = \ aℓ a -> GHC.IO.Buffer.bufferAdjustL      #-}
{-# COMPILE GHC bufferAddOffset    = \ aℓ a -> GHC.IO.Buffer.bufferAddOffset    #-}
{-# COMPILE GHC bufferAdjustOffset = \ aℓ a -> GHC.IO.Buffer.bufferAdjustOffset #-}

{-# COMPILE GHC isEmptyBuffer    = \ aℓ a -> GHC.IO.Buffer.isEmptyBuffer    #-}
{-# COMPILE GHC isFullBuffer     = \ aℓ a -> GHC.IO.Buffer.isFullBuffer     #-}
{-# COMPILE GHC isFullCharBuffer = \ aℓ a -> GHC.IO.Buffer.isFullCharBuffer #-}
{-# COMPILE GHC isWriteBuffer    = \ aℓ a -> GHC.IO.Buffer.isWriteBuffer    #-}
{-# COMPILE GHC bufferElems      = \ aℓ a -> GHC.IO.Buffer.bufferElems      #-}
{-# COMPILE GHC bufferAvailable  = \ aℓ a -> GHC.IO.Buffer.bufferAvailable  #-}
{-# COMPILE GHC bufferOffset     = \ aℓ a -> GHC.IO.Buffer.bufferOffset     #-}
{-# COMPILE GHC summaryBuffer    = \ aℓ a -> GHC.IO.Buffer.summaryBuffer    #-}

{-# COMPILE GHC withBuffer    = \ aℓ a bℓ b -> GHC.IO.Buffer.withBuffer    #-}
{-# COMPILE GHC withRawBuffer = \ aℓ a bℓ b -> GHC.IO.Buffer.withRawBuffer #-}

{-# COMPILE GHC checkBuffer = \ aℓ a -> GHC.IO.Buffer.checkBuffer #-}

{-# COMPILE GHC readWord8Buf    = GHC.IO.Buffer.readWord8Buf    #-}
{-# COMPILE GHC writeWord8Buf   = GHC.IO.Buffer.writeWord8Buf   #-}
{-# COMPILE GHC peekCharBuf     = GHC.IO.Buffer.peekCharBuf     #-}
{-# COMPILE GHC readCharBuf     = GHC.IO.Buffer.readCharBuf     #-}
{-# COMPILE GHC writeCharBuf    = GHC.IO.Buffer.writeCharBuf    #-}
{-# COMPILE GHC readCharBufPtr  = GHC.IO.Buffer.readCharBufPtr  #-}
{-# COMPILE GHC writeCharBufPtr = GHC.IO.Buffer.writeCharBufPtr #-}
{-# COMPILE GHC charSize        = GHC.IO.Buffer.charSize        #-}
