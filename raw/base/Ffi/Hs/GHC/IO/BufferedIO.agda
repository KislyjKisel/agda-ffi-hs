{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.IO.BufferedIO where

open import Agda.Builtin.IO      using (IO)
open import Agda.Builtin.Maybe   using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.Data.Int      using (Int)
open import Ffi.Hs.Data.Tuple    using (Tuple2)
open import Ffi.Hs.Data.Word     using (Word8)
open import Ffi.Hs.GHC.IO.Buffer using (Buffer; BufferState)
open import Ffi.Hs.GHC.IO.Device using (RawIO)

{-# FOREIGN GHC
import qualified GHC.IO.BufferedIO
import MAlonzo.Code.Ffi.Hs.GHC.IO.Device (AgdaRawIO(AgdaRawIO))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


postulate
    BufferedIO : Set aℓ → Set aℓ

    newBuffer         : ⦃ BufferedIO A ⦄ → A → BufferState → IO (Buffer Word8)
    fillReadBuffer    : ⦃ BufferedIO A ⦄ → A → Buffer Word8 → IO (Tuple2 Int (Buffer Word8))
    fillReadBuffer0   : ⦃ BufferedIO A ⦄ → A → Buffer Word8 → IO (Tuple2 (Maybe Int) (Buffer Word8))
    emptyWriteBuffer  : ⦃ BufferedIO A ⦄ → A → Buffer Word8 → IO (Buffer Word8)
    flushWriteBuffer  : ⦃ BufferedIO A ⦄ → A → Buffer Word8 → IO (Buffer Word8) 
    flushWriteBuffer0 : ⦃ BufferedIO A ⦄ → A → Buffer Word8 → IO (Tuple2 Int (Buffer Word8))

    readBuf             : ⦃ RawIO A ⦄ → A → Buffer Word8 → IO (Tuple2 Int (Buffer Word8))
    readBufNonBlocking  : ⦃ RawIO A ⦄ → A → Buffer Word8 → IO (Tuple2 (Maybe Int) (Buffer Word8))
    writeBuf            : ⦃ RawIO A ⦄ → A → Buffer Word8 → IO (Buffer Word8)
    writeBufNonBlocking : ⦃ RawIO A ⦄ → A → Buffer Word8 → IO (Tuple2 Int (Buffer Word8))

{-# FOREIGN GHC data AgdaBufferedIO aℓ a = GHC.IO.BufferedIO a => AgdaBufferedIO #-}
{-# COMPILE GHC BufferedIO = type(0) AgdaBufferedIO #-}

{-# COMPILE GHC newBuffer         = \ aℓ a AgdaBufferedIO -> GHC.IO.BufferedIO.newBuffer         #-}
{-# COMPILE GHC fillReadBuffer    = \ aℓ a AgdaBufferedIO -> GHC.IO.BufferedIO.fillReadBuffer    #-}
{-# COMPILE GHC fillReadBuffer0   = \ aℓ a AgdaBufferedIO -> GHC.IO.BufferedIO.fillReadBuffer0   #-}
{-# COMPILE GHC emptyWriteBuffer  = \ aℓ a AgdaBufferedIO -> GHC.IO.BufferedIO.emptyWriteBuffer  #-}
{-# COMPILE GHC flushWriteBuffer  = \ aℓ a AgdaBufferedIO -> GHC.IO.BufferedIO.flushWriteBuffer  #-}
{-# COMPILE GHC flushWriteBuffer0 = \ aℓ a AgdaBufferedIO -> GHC.IO.BufferedIO.flushWriteBuffer0 #-}

{-# COMPILE GHC readBuf             = \ aℓ a AgdaRawIO -> GHC.IO.BufferedIO.readBuf             #-}
{-# COMPILE GHC readBufNonBlocking  = \ aℓ a AgdaRawIO -> GHC.IO.BufferedIO.readBufNonBlocking  #-}
{-# COMPILE GHC writeBuf            = \ aℓ a AgdaRawIO -> GHC.IO.BufferedIO.writeBuf            #-}
{-# COMPILE GHC writeBufNonBlocking = \ aℓ a AgdaRawIO -> GHC.IO.BufferedIO.writeBufNonBlocking #-}
