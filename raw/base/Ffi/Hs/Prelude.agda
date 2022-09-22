{-# OPTIONS --without-K #-}

module Ffi.Hs.Prelude where

open import Agda.Primitive

open import Ffi.Hs.Data.Bool public
    using (Bool; True; False; not; otherwise; _&&_; _||_)

open import Ffi.Hs.Data.List public
    using (List)

open import Ffi.Hs.Data.Maybe public
    using (Maybe; Just; Nothing; maybe)

-- todo: Prelude
