{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.QSem where

open import Agda.Builtin.IO   using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤; ⊤′)
open import Ffi.Hs.Data.Int   using (Int)

postulate
    QSem : Set
    newQSem    : Int → IO QSem
    waitQSem   : QSem → IO ⊤
    signalQSem : QSem → IO ⊤

{-# FOREIGN GHC import qualified Control.Concurrent.QSem as AgdaHsQSem #-}
{-# COMPILE GHC QSem       = AgdaHsQSem.QSem       #-}
{-# COMPILE GHC newQSem    = AgdaHsQSem.newQSem    #-}
{-# COMPILE GHC waitQSem   = AgdaHsQSem.waitQSem   #-}
{-# COMPILE GHC signalQSem = AgdaHsQSem.signalQSem #-}
