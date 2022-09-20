{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.IO.FD where

open import Agda.Builtin.Bool         using (Bool)
open import Agda.Builtin.Char         using (Char)
open import Agda.Builtin.List         using (List)
open import Agda.Builtin.Maybe        using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class        using (Show)
open import Ffi.Hs.-base.Unit         using (⊤; ⊤′)
open import Ffi.Hs.Data.Int           using (Int)
open import Ffi.Hs.Data.Tuple         using (Tuple2; Tuple3)
open import Ffi.Hs.Data.Word          using (Word8)
open import Ffi.Hs.Foreign.C.Types    using (CInt; CSize)
open import Ffi.Hs.Foreign.Ptr        using (Ptr)
open import Ffi.Hs.GHC.IO.BufferedIO  using (BufferedIO)
open import Ffi.Hs.GHC.IO.Device      using (RawIO; IODevice; IODeviceType)
open import Ffi.Hs.System.IO          using (IO; FilePath; IOMode)
open import Ffi.Hs.System.Posix.Types using (CDev; CIno)

{-# FOREIGN GHC
import qualified GHC.IO.FD
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.GHC.IO.Device     (AgdaRawIO, AgdaIODevice)
import MAlonzo.Code.Ffi.Hs.GHC.IO.BufferedIO (AgdaBufferedIO)
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ

record FD : Set where
    constructor mkFD′
    field
        fdFD            : CInt
        fdIsNonBlocking : Int

{-# COMPILE GHC FD = data GHC.IO.FD.FD (GHC.IO.FD.FD) #-}

postulate
    openFileWith : FilePath → IOMode → Bool → (FD → IODeviceType → IO A) →
                   ((∀{xℓ}{X : Set xℓ} → IO X → IO X) → A → IO B) → IO B

    mkFD : CInt → IOMode → Maybe (Tuple3 IODeviceType CDev CIno) →
           Bool → Bool → IO (Tuple2 FD IODeviceType)

    openFile                : FilePath → IOMode → Bool → IO (Tuple2 FD IODeviceType)
    release                 : FD → IO ⊤
    setNonBlockingMode      : FD → Bool → IO FD
    readRawBufferPtr        : List Char → FD → Ptr Word8 → Int → CSize → IO Int
    readRawBufferPtrNoBlock : List Char → FD → Ptr Word8 → Int → CSize → IO Int
    writeRawBufferPtr       : List Char → FD → Ptr Word8 → Int → CSize → IO CInt

    stdin  : FD
    stdout : FD
    stderr : FD

{-# COMPILE GHC openFileWith =
    \ aℓ a bℓ b fp m b a1 a2 -> GHC.IO.FD.openFileWith fp m b a1 (\ f -> a2 (f () ())) #-}

{-# COMPILE GHC mkFD                    = GHC.IO.FD.mkFD #-}
{-# COMPILE GHC openFile                = GHC.IO.FD.openFile #-}
{-# COMPILE GHC release                 = GHC.IO.FD.release #-}
{-# COMPILE GHC setNonBlockingMode      = GHC.IO.FD.setNonBlockingMode #-}
{-# COMPILE GHC readRawBufferPtr        = GHC.IO.FD.readRawBufferPtr #-}
{-# COMPILE GHC readRawBufferPtrNoBlock = GHC.IO.FD.readRawBufferPtrNoBlock #-}
{-# COMPILE GHC writeRawBufferPtr       = GHC.IO.FD.writeRawBufferPtr #-}

{-# COMPILE GHC stdin  = GHC.IO.FD.stdin  #-}
{-# COMPILE GHC stdout = GHC.IO.FD.stdout #-}
{-# COMPILE GHC stderr = GHC.IO.FD.stderr #-}

postulate
    BufferedIO[FD] : BufferedIO FD
    IODevice[FD]   : IODevice FD
    RawIO[FD]      : RawIO FD
    Show[FD]       : Show FD

{-# COMPILE GHC BufferedIO[FD] = AgdaBufferedIO #-}
{-# COMPILE GHC IODevice[FD]   = AgdaIODevice   #-}
{-# COMPILE GHC RawIO[FD]      = AgdaRawIO      #-}
{-# COMPILE GHC Show[FD]       = AgdaShow       #-}
