{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.IsList where

open import Agda.Builtin.List using (List)
open import Agda.Primitive

{-# FOREIGN GHC
import qualified GHC.IsList
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    IsList   : Set aℓ → Set aℓ
    Item     : (A : Set aℓ) → ⦃ IsList A ⦄ → Set
    fromList : ⦃ _ : IsList A ⦄ → List (Item A) → A
    toList   : ⦃ _ : IsList A ⦄ → A → List (Item A)

{-# FOREIGN GHC data AgdaIsList aℓ a = GHC.IsList.IsList a => AgdaIsList #-}
{-# COMPILE GHC IsList = type(0) AgdaIsList #-}

{-# FOREIGN GHC type AgdaItem aℓ a isl = GHC.IsList.Item a #-}
{-# COMPILE GHC Item = type(3) AgdaItem #-}

{-# COMPILE GHC fromList = \ aℓ a AgdaIsList -> GHC.IsList.fromList #-}
{-# COMPILE GHC toList   = \ aℓ a AgdaIsList -> GHC.IsList.toList   #-}
