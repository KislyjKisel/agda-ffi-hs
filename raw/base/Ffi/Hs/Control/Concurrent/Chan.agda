{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Concurrent.Chan where

open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Eq)
open import Ffi.Hs.-base.Unit  using (⊤; ⊤′)

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Chan : Set aℓ → Set aℓ

    newChan   : IO (Chan A)
    writeChan : Chan A → A → IO ⊤
    readChan  : Chan A → IO A
    dupChan   : Chan A → IO (Chan A)

    getChanContents : Chan A → IO (List A)
    writeList2Chan  : Chan A → List A → IO ⊤

{-# FOREIGN GHC import qualified Control.Concurrent.Chan as AgdaHsChan #-}

{-# FOREIGN GHC type AgdaChan aℓ = AgdaHsChan.Chan #-}
{-# COMPILE GHC Chan = type(1) AgdaChan #-}

{-# COMPILE GHC newChan   = \ aℓ a -> AgdaHsChan.newChan   #-}
{-# COMPILE GHC writeChan = \ aℓ a -> AgdaHsChan.writeChan #-}
{-# COMPILE GHC readChan  = \ aℓ a -> AgdaHsChan.readChan  #-}
{-# COMPILE GHC dupChan   = \ aℓ a -> AgdaHsChan.dupChan   #-}

{-# COMPILE GHC getChanContents = \ aℓ a -> AgdaHsChan.getChanContents #-}
{-# COMPILE GHC writeList2Chan  = \ aℓ a -> AgdaHsChan.writeList2Chan  #-}

postulate
    Eq[Chan[A]] : Eq (Chan A)

{-# FOREIGN GHC import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries #-}
{-# COMPILE GHC Eq[Chan[A]] = \ aℓ a -> AgdaEq #-}
