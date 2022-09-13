{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Timeout where

open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Exception; Show; Eq)
open import Ffi.Hs.Data.Int    using (Int)

{-# FOREIGN GHC
import qualified System.Timeout
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Timeout : Set
    timeout : Int → IO A → IO (Maybe A)

    Exception[Timeout] : Exception Timeout
    Show[Timeout]      : Show Timeout
    Eq[Timeout]        : Eq Timeout

{-# COMPILE GHC Timeout = type System.Timeout.Timeout #-}
{-# COMPILE GHC timeout = \ aℓ a -> System.Timeout.timeout #-}

{-# COMPILE GHC Exception[Timeout] = AgdaException #-}
{-# COMPILE GHC Show[Timeout]      = AgdaShow      #-}
{-# COMPILE GHC Eq[Timeout]        = AgdaEq        #-}
