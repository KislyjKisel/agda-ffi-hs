{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.QSem where

open import Agda.Builtin.IO   using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤)
open import Ffi.Hs.Data.Int   using (Int)

{-# FOREIGN GHC
import qualified Control.Concurrent.QSem
#-}

postulate
    QSem : Set
    newQSem    : Int → IO QSem
    waitQSem   : QSem → IO ⊤
    signalQSem : QSem → IO ⊤

{-# COMPILE GHC QSem = type Control.Concurrent.QSem.QSem #-}
{-# COMPILE GHC newQSem    = Control.Concurrent.QSem.newQSem    #-}
{-# COMPILE GHC waitQSem   = Control.Concurrent.QSem.waitQSem   #-}
{-# COMPILE GHC signalQSem = Control.Concurrent.QSem.signalQSem #-}
