{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Fingerprint where

postulate
    Fingerprint : Set

{-# FOREIGN GHC import qualified GHC.Fingerprint.Type #-}
{-# COMPILE GHC Fingerprint = GHC.Fingerprint.Type.Fingerprint #-}
