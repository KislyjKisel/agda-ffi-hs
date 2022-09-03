{-# OPTIONS --without-K #-}

module Ffi.Hs.System.CPUTime where

open import Agda.Builtin.Int using () renaming (Int to Integer)
open import Agda.Builtin.IO  using (IO)

postulate
    getCPUTime       : IO Integer
    cpuTimePrecision : Integer

{-# FOREIGN GHC import qualified System.CPUTime #-}
{-# COMPILE GHC getCPUTime       = System.CPUTime.getCPUTime       #-}
{-# COMPILE GHC cpuTimePrecision = System.CPUTime.cpuTimePrecision #-}
