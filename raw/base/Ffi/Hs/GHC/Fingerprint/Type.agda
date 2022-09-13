{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Fingerprint.Type where

open import Ffi.Hs.-base.Class using (Storable; Show; Eq; Ord)
open import Ffi.Hs.Data.Word   using (Word64)

{-# FOREIGN GHC
import qualified GHC.Fingerprint.Type
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

data Fingerprint : Set where
    mkFingerprint : Word64 → Word64 → Fingerprint

{-# COMPILE GHC Fingerprint = data GHC.Fingerprint.Type.Fingerprint (GHC.Fingerprint.Type.Fingerprint) #-}

postulate
    Storable[Fingerprint] : Storable Fingerprint
    Show[Fingerprint]     : Show Fingerprint
    Eq[Fingerprint]       : Eq Fingerprint
    Ord[Fingerprint]      : Ord Fingerprint

{-# COMPILE GHC Storable[Fingerprint] = AgdaStorable #-}
{-# COMPILE GHC Show[Fingerprint]     = AgdaShow     #-}
{-# COMPILE GHC Eq[Fingerprint]       = AgdaEq       #-}
{-# COMPILE GHC Ord[Fingerprint]      = AgdaOrd      #-}
