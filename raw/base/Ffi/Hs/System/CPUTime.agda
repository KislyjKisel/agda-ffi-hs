{-# OPTIONS --without-K #-}

module Ffi.Hs.System.CPUTime where

open import Agda.Builtin.Int using () renaming (Int to Integer)
open import Agda.Builtin.IO  using (IO)

{-# FOREIGN GHC
import qualified System.CPUTime
#-}

postulate
    getCPUTime       : IO Integer
    cpuTimePrecision : Integer

{-# COMPILE GHC getCPUTime       = System.CPUTime.getCPUTime       #-}
{-# COMPILE GHC cpuTimePrecision = System.CPUTime.cpuTimePrecision #-}
