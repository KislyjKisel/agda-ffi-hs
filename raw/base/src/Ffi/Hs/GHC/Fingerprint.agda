{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Fingerprint where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Word   using (Word8)
open import Ffi.Hs.Foreign.Ptr using (Ptr)

open import Ffi.Hs.GHC.Fingerprint.Type public

{-# FOREIGN GHC
import qualified GHC.Fingerprint
#-}

postulate
    fingerprint0 : Fingerprint
    fingerprintData : Ptr Word8 → Int → IO Fingerprint
    fingerprintString : List Char → Fingerprint
    fingerprintFingerprints : List Fingerprint → Fingerprint
    getFileHash : List Char → IO Fingerprint

{-# COMPILE GHC fingerprint0            = GHC.Fingerprint.fingerprint0            #-}
{-# COMPILE GHC fingerprintData         = GHC.Fingerprint.fingerprintData         #-}
{-# COMPILE GHC fingerprintString       = GHC.Fingerprint.fingerprintString       #-}
{-# COMPILE GHC fingerprintFingerprints = GHC.Fingerprint.fingerprintFingerprints #-}
{-# COMPILE GHC getFileHash             = GHC.Fingerprint.getFileHash             #-}
