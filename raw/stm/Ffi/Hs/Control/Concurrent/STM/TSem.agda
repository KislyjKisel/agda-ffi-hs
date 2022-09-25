{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.STM.TSem where

open import Agda.Builtin.Int         using () renaming (Int to Integer)
open import Ffi.Hs.-base.Class       using (Eq)
open import Ffi.Hs.-base.Unit        using (⊤)
open import Ffi.Hs.Control.Monad.STM using (STM)
open import Ffi.Hs.Numeric.Natural   using (Natural)

{-# FOREIGN GHC
import qualified Control.Concurrent.STM.TSem
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaEq(AgdaEq))
#-}

postulate
    TSem : Set
    Eq[TSem] : Eq TSem

    newTSem     : Integer → STM TSem
    waitTSem    : TSem → STM ⊤
    signalTSem  : TSem → STM ⊤
    signalTSemN : Natural → TSem → STM ⊤

{-# COMPILE GHC TSem = type Control.Concurrent.STM.TSem.TSem #-}

{-# COMPILE GHC Eq[TSem] = AgdaEq #-}

{-# COMPILE GHC newTSem     = Control.Concurrent.STM.TSem.newTSem     #-}
{-# COMPILE GHC waitTSem    = Control.Concurrent.STM.TSem.waitTSem    #-}
{-# COMPILE GHC signalTSem  = Control.Concurrent.STM.TSem.signalTSem  #-}
{-# COMPILE GHC signalTSemN = Control.Concurrent.STM.TSem.signalTSemN #-}
