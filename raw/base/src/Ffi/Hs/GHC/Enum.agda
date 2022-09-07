{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Enum where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Show)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

open Ffi.Hs.-base.Class public
    using (Bounded; Enum)

{-# FOREIGN GHC
import qualified GHC.Enum
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaShow, AgdaBounded, AgdaEnum )
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    minBound : ⦃ Bounded A ⦄ → A
    maxBound : ⦃ Bounded A ⦄ → A

    succ           : ⦃ Enum A ⦄ → A → A
    pred           : ⦃ Enum A ⦄ → A → A
    toEnum         : ⦃ Enum A ⦄ → Int → A
    fromEnum       : ⦃ Enum A ⦄ → A → Int
    enumFrom       : ⦃ Enum A ⦄ → A → List A
    enumFromThen   : ⦃ Enum A ⦄ → A → A → List A
    enumFromTo     : ⦃ Enum A ⦄ → A → A → List A
    enumFromThenTo : ⦃ Enum A ⦄ → A → A → A → List A

    boundedEnumFrom     : ⦃ Enum A ⦄ → ⦃ Bounded A ⦄ → A → List A
    boundedEnumFromThen : ⦃ Enum A ⦄ → ⦃ Bounded A ⦄ → A → A → List A
    toEnumError         : ⦃ Show A ⦄ → List Char → Int → Tuple2 A A → B
    fromEnumError       : ⦃ Show A ⦄ → List Char → A → B
    succError           : List Char → A
    predError           : List Char → A

{-# COMPILE GHC minBound = \ aℓ a AgdaBounded -> GHC.Enum.minBound #-}
{-# COMPILE GHC maxBound = \ aℓ a AgdaBounded -> GHC.Enum.maxBound #-}

{-# COMPILE GHC succ           = \ aℓ a AgdaEnum -> GHC.Enum.succ           #-}
{-# COMPILE GHC pred           = \ aℓ a AgdaEnum -> GHC.Enum.pred           #-}
{-# COMPILE GHC toEnum         = \ aℓ a AgdaEnum -> GHC.Enum.toEnum         #-}
{-# COMPILE GHC fromEnum       = \ aℓ a AgdaEnum -> GHC.Enum.fromEnum       #-}
{-# COMPILE GHC enumFrom       = \ aℓ a AgdaEnum -> GHC.Enum.enumFrom       #-}
{-# COMPILE GHC enumFromThen   = \ aℓ a AgdaEnum -> GHC.Enum.enumFromThen   #-}
{-# COMPILE GHC enumFromTo     = \ aℓ a AgdaEnum -> GHC.Enum.enumFromTo     #-}
{-# COMPILE GHC enumFromThenTo = \ aℓ a AgdaEnum -> GHC.Enum.enumFromThenTo #-}

{-# COMPILE GHC boundedEnumFrom     = \ aℓ a AgdaEnum AgdaBounded -> GHC.Enum.boundedEnumFrom     #-}
{-# COMPILE GHC boundedEnumFromThen = \ aℓ a AgdaEnum AgdaBounded -> GHC.Enum.boundedEnumFromThen #-}
{-# COMPILE GHC toEnumError         = \ aℓ a bℓ b AgdaShow        -> GHC.Enum.toEnumError         #-}
{-# COMPILE GHC fromEnumError       = \ aℓ a bℓ b AgdaShow        -> GHC.Enum.fromEnumError       #-}
{-# COMPILE GHC succError           = \ aℓ a                      -> GHC.Enum.succError           #-}
{-# COMPILE GHC predError           = \ aℓ a                      -> GHC.Enum.predError           #-}
