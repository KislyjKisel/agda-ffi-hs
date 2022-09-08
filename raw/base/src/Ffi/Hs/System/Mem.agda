{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Mem where

open import Agda.Builtin.IO   using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤)
open import Ffi.Hs.Data.Int   using (Int64)

{-# FOREIGN GHC
import qualified System.Mem
#-}

postulate
    performGC      : IO (⊤ {lzero})
    performMajorGC : IO (⊤ {lzero})
    performMinorGC : IO (⊤ {lzero})

    setAllocationCounter   : Int64 → IO (⊤ {lzero})
    getAllocationCounter   : IO Int64
    enableAllocationLimit  : IO (⊤ {lzero})
    disableAllocationLimit : IO (⊤ {lzero})

{-# COMPILE GHC performGC      = System.Mem.performGC      #-}
{-# COMPILE GHC performMajorGC = System.Mem.performMajorGC #-}
{-# COMPILE GHC performMinorGC = System.Mem.performMinorGC #-}

{-# COMPILE GHC setAllocationCounter   = System.Mem.setAllocationCounter   #-}
{-# COMPILE GHC getAllocationCounter   = System.Mem.getAllocationCounter   #-}
{-# COMPILE GHC enableAllocationLimit  = System.Mem.enableAllocationLimit  #-}
{-# COMPILE GHC disableAllocationLimit = System.Mem.disableAllocationLimit #-}
