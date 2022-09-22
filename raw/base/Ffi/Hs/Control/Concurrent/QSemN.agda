{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.QSemN where

open import Agda.Builtin.IO   using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤)
open import Ffi.Hs.Data.Int   using (Int)

{-# FOREIGN GHC
import qualified Control.Concurrent.QSemN
#-}

postulate
    QSemN : Set
    newQSemN    : Int → IO QSemN
    waitQSemN   : QSemN → Int → IO ⊤
    signalQSemN : QSemN → Int → IO ⊤

{-# COMPILE GHC QSemN = type Control.Concurrent.QSemN.QSemN #-}
{-# COMPILE GHC newQSemN    = Control.Concurrent.QSemN.newQSemN    #-}
{-# COMPILE GHC waitQSemN   = Control.Concurrent.QSemN.waitQSemN   #-}
{-# COMPILE GHC signalQSemN = Control.Concurrent.QSemN.signalQSemN #-}
