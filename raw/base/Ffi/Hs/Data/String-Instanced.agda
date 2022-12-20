{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.String-Instanced where

open import Ffi.Hs.Data.String

instance
    inst:IsString[String]     = IsString[String]
    inst:Lit-FromText[String] = Lit-FromText[String]
