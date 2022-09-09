{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.String where

open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Primitive

{-# FOREIGN GHC
import qualified Data.String
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

String : Set
String = List Char

postulate
    IsString : Set aℓ → Set aℓ
    fromString : ⦃ IsString A ⦄ → String → A

    lines   : String → List String
    words   : String → List String
    unlines : List String → String
    unwords : List String → String

{-# FOREIGN GHC data AgdaIsString aℓ a = IsString a => AgdaIsString #-}
{-# COMPILE GHC IsString = type(0) AgdaIsString #-}
{-# COMPILE GHC fromString = \ aℓ a AgdaIsString -> Data.String.fromString #-}

{-# COMPILE GHC lines   = Data.String.lines   #-}
{-# COMPILE GHC words   = Data.String.words   #-}
{-# COMPILE GHC unlines = Data.String.unlines #-}
{-# COMPILE GHC unwords = Data.String.unwords #-}

postulate
    IsString[String] : IsString String

{-# COMPILE GHC IsString[String] = AgdaIsString #-}
