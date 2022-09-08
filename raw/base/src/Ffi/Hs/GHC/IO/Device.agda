{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.IO.Device where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Int   using () renaming (Int to Integer)
open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Eq; Ord; Read; Show; Enum; Ix)
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Word   using (Word8; Word64)
open import Ffi.Hs.Foreign.Ptr using (Ptr)

{-# FOREIGN GHC
import qualified GHC.IO.Device
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaRead, AgdaShow, AgdaEq
    , AgdaOrd, AgdaEnum, AgdaIx
    )
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    RawIO : Set aℓ → Set aℓ
    read             : ⦃ RawIO A ⦄ → A → Ptr Word8 → Word64 → Int → IO Int
    readNonBlocking  : ⦃ RawIO A ⦄ → A → Ptr Word8 → Word64 → Int → IO (Maybe Int)
    write            : ⦃ RawIO A ⦄ → A → Ptr Word8 → Word64 → Int → IO (⊤ {lzero})
    writeNonBlocking : ⦃ RawIO A ⦄ → A → Ptr Word8 → Word64 → Int → IO Int

{-# FOREIGN GHC data AgdaRawIO aℓ a = GHC.IO.Device.RawIO a => AgdaRawIO #-}
{-# COMPILE GHC RawIO = type(0) AgdaRawIO #-}

{-# COMPILE GHC read             = \ aℓ a AgdaRawIO -> GHC.IO.Device.read             #-}
{-# COMPILE GHC readNonBlocking  = \ aℓ a AgdaRawIO -> GHC.IO.Device.readNonBlocking  #-}
{-# COMPILE GHC write            = \ aℓ a AgdaRawIO -> GHC.IO.Device.write            #-}
{-# COMPILE GHC writeNonBlocking = \ aℓ a AgdaRawIO -> GHC.IO.Device.writeNonBlocking #-}

data IODeviceType : Set where
    Directory   : IODeviceType
    Stream      : IODeviceType
    RegularFile : IODeviceType
    RawDevice   : IODeviceType

{-# COMPILE GHC IODeviceType = data GHC.IO.Device.IODeviceType (GHC.IO.Device.Directory | GHC.IO.Device.Stream | GHC.IO.Device.RegularFile | GHC.IO.Device.RawDevice) #-}

postulate
    Eq[IODeviceType] : Eq IODeviceType

{-# COMPILE GHC Eq[IODeviceType] = AgdaEq #-}

data SeekMode : Set where
    AbsoluteSeek : SeekMode
    RelativeSeek : SeekMode
    SeekFromEnd  : SeekMode

{-# COMPILE GHC SeekMode = data System.IO.SeekMode (System.IO.AbsoluteSeek | System.IO.RelativeSeek | System.IO.SeekFromEnd) #-}

postulate
    Enum[SeekMode] : Enum SeekMode
    Ix[SeekMode]   : Ix SeekMode
    Read[SeekMode] : Read SeekMode
    Show[SeekMode] : Show SeekMode
    Eq[SeekMode]   : Eq SeekMode
    Ord[SeekMode]  : Ord SeekMode

{-# COMPILE GHC Enum[SeekMode] = AgdaEnum #-}
{-# COMPILE GHC Ix[SeekMode]   = AgdaIx   #-}
{-# COMPILE GHC Read[SeekMode] = AgdaRead #-}
{-# COMPILE GHC Show[SeekMode] = AgdaShow #-}
{-# COMPILE GHC Eq[SeekMode]   = AgdaEq   #-}
{-# COMPILE GHC Ord[SeekMode]  = AgdaOrd  #-}

postulate
    IODevice : Set aℓ → Set aℓ
    ready      : ⦃ IODevice A ⦄ → A → Bool → Int → IO Bool
    close      : ⦃ IODevice A ⦄ → A → IO (⊤ {lzero})
    isTerminal : ⦃ IODevice A ⦄ → A → IO Bool
    isSeekable : ⦃ IODevice A ⦄ → A → IO Bool
    seek       : ⦃ IODevice A ⦄ → A → SeekMode → Integer → IO Integer
    tell       : ⦃ IODevice A ⦄ → A → IO Integer
    getSize    : ⦃ IODevice A ⦄ → A → IO Integer
    setSize    : ⦃ IODevice A ⦄ → A → Integer → IO (⊤ {lzero})
    setEcho    : ⦃ IODevice A ⦄ → A → Bool → IO (⊤ {lzero})
    getEcho    : ⦃ IODevice A ⦄ → A → IO Bool
    setRaw     : ⦃ IODevice A ⦄ → A → Bool → IO (⊤ {lzero})
    devType    : ⦃ IODevice A ⦄ → A → IO IODeviceType
    dup        : ⦃ IODevice A ⦄ → A → IO A
    dup2       : ⦃ IODevice A ⦄ → A → A → IO A

{-# FOREIGN GHC data AgdaIODevice aℓ a = GHC.IO.Device.IODevice a => AgdaIODevice #-}
{-# COMPILE GHC IODevice = type(0) AgdaIODevice #-}

{-# COMPILE GHC ready      = \ aℓ a AgdaIODevice -> GHC.IO.Device.ready      #-}
{-# COMPILE GHC close      = \ aℓ a AgdaIODevice -> GHC.IO.Device.close      #-}
{-# COMPILE GHC isTerminal = \ aℓ a AgdaIODevice -> GHC.IO.Device.isTerminal #-}
{-# COMPILE GHC isSeekable = \ aℓ a AgdaIODevice -> GHC.IO.Device.isSeekable #-}
{-# COMPILE GHC seek       = \ aℓ a AgdaIODevice -> GHC.IO.Device.seek       #-}
{-# COMPILE GHC tell       = \ aℓ a AgdaIODevice -> GHC.IO.Device.tell       #-}
{-# COMPILE GHC getSize    = \ aℓ a AgdaIODevice -> GHC.IO.Device.getSize    #-}
{-# COMPILE GHC setSize    = \ aℓ a AgdaIODevice -> GHC.IO.Device.setSize    #-}
{-# COMPILE GHC setEcho    = \ aℓ a AgdaIODevice -> GHC.IO.Device.setEcho    #-}
{-# COMPILE GHC getEcho    = \ aℓ a AgdaIODevice -> GHC.IO.Device.getEcho    #-}
{-# COMPILE GHC setRaw     = \ aℓ a AgdaIODevice -> GHC.IO.Device.setRaw     #-}
{-# COMPILE GHC devType    = \ aℓ a AgdaIODevice -> GHC.IO.Device.devType    #-}
{-# COMPILE GHC dup        = \ aℓ a AgdaIODevice -> GHC.IO.Device.dup        #-}
{-# COMPILE GHC dup2       = \ aℓ a AgdaIODevice -> GHC.IO.Device.dup2       #-}
