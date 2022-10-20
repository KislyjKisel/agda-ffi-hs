{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Unique.Really where

open import Agda.Builtin.IO      using (IO)
open import Ffi.Hs.-base.Class   using (Eq)
open import Ffi.Hs.Data.Hashable using (Hashable)
open import Ffi.Hs.Data.Int      using (Int)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Unique.Really
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Hashable (AgdaHashable(AgdaHashable))
#-}

postulate
    Unique : Set

    Eq[Unique]       : Eq Unique
    Hashable[Unique] : Hashable Unique

    newUnique  : IO Unique
    hashUnique : Unique → Int

{-# COMPILE GHC Unique = type Data.Unique.Really.Unique #-}

{-# COMPILE GHC Eq[Unique]       = AgdaEq       #-}
{-# COMPILE GHC Hashable[Unique] = AgdaHashable #-}

{-# COMPILE GHC newUnique  = Data.Unique.Really.newUnique  #-}
{-# COMPILE GHC hashUnique = Data.Unique.Really.hashUnique #-}
