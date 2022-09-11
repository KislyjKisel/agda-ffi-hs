{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Unique where

open import Agda.Builtin.IO    using (IO)
open import Ffi.Hs.-base.Class using (Eq; Ord)
open import Ffi.Hs.Data.Int    using (Int)

{-# FOREIGN GHC
import qualified Data.Unique
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaEq, AgdaOrd
    )
#-}

postulate
    Unique : Set

    Eq[Unique]  : Eq Unique
    Ord[Unique] : Ord Unique

    newUnique  : IO Unique
    hashUnique : Unique → Int

{-# COMPILE GHC Unique = type Data.Unique.Unique #-}

{-# COMPILE GHC Eq[Unique]  = AgdaEq  #-}
{-# COMPILE GHC Ord[Unique] = AgdaOrd #-}

{-# COMPILE GHC newUnique  = Data.Unique.newUnique  #-}
{-# COMPILE GHC hashUnique = Data.Unique.hashUnique #-}
