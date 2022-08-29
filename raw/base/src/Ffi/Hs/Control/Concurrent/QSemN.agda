{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.QSemN where

open import Agda.Builtin.IO   using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤)
open import Ffi.Hs.Data.Int   using (Int)

postulate
    QSemN : Set
    newQSemN    : Int → IO QSemN
    waitQSemN   : QSemN → Int → IO (⊤ {lzero})
    signalQSemN : QSemN → Int → IO (⊤ {lzero})

{-# FOREIGN GHC import qualified Control.Concurrent.QSemN as AgdaHsQSemN #-}
{-# COMPILE GHC QSemN       = AgdaHsQSemN.QSemN       #-}
{-# COMPILE GHC newQSemN    = AgdaHsQSemN.newQSemN    #-}
{-# COMPILE GHC waitQSemN   = AgdaHsQSemN.waitQSemN   #-}
{-# COMPILE GHC signalQSemN = AgdaHsQSemN.signalQSemN #-}
