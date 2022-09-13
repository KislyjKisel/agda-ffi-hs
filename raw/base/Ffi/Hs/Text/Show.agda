{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.Show where

open import Agda.Builtin.Bool using (Bool)
open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Primitive
open import Ffi.Hs.Data.Int   using (Int)

open import Ffi.Hs.-base.Class public
    using (Show)

private
    variable
        aℓ : Level
        A : Set aℓ

ShowS : Set
ShowS = List Char → List Char

postulate
    showsPrec : ⦃ Show A ⦄ → Int → A → ShowS
    show      : ⦃ Show A ⦄ → A → List Char
    showList  : ⦃ Show A ⦄ → List A → ShowS

    shows        : ⦃ Show A ⦄ → A → ShowS
    showChar     : Char → ShowS
    showString   : List Char → ShowS
    showParen    : Bool → ShowS → ShowS
    showListWith : (A → ShowS) → List A → ShowS

{-# FOREIGN GHC
import qualified Text.Show
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

{-# COMPILE GHC showsPrec = \ aℓ a AgdaShow -> Text.Show.showsPrec #-}
{-# COMPILE GHC show      = \ aℓ a AgdaShow -> Text.Show.show      #-}
{-# COMPILE GHC showList  = \ aℓ a AgdaShow -> Text.Show.showList  #-}

{-# COMPILE GHC shows        = \ aℓ a AgdaShow -> Text.Show.shows        #-}
{-# COMPILE GHC showChar     =                    Text.Show.showChar     #-}
{-# COMPILE GHC showString   =                    Text.Show.showString   #-}
{-# COMPILE GHC showParen    =                    Text.Show.showParen    #-}
{-# COMPILE GHC showListWith = \ aℓ a ->          Text.Show.showListWith #-}
