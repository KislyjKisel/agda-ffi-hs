{-# OPTIONS --without-K #-}

module Ffi.Hs.System.IO where

open import Agda.Builtin.IO public
    using (IO)

postulate
    Handle : Set