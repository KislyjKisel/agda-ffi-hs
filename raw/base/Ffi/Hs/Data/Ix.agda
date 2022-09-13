{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Ix where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Ord)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

open Ffi.Hs.-base.Class public
    using (Ix)

{-# FOREIGN GHC
import qualified Data.Ix
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    range     : ⦃ Ix A ⦄ → Tuple2 A A → List A
    index     : ⦃ Ix A ⦄ → Tuple2 A A → A → Int
    inRange   : ⦃ Ix A ⦄ → Tuple2 A A → A → Bool
    rangeSize : ⦃ Ix A ⦄ → Tuple2 A A → Int

{-# COMPILE GHC range     = \ aℓ a AgdaIx -> Data.Ix.range     #-}
{-# COMPILE GHC index     = \ aℓ a AgdaIx -> Data.Ix.index     #-}
{-# COMPILE GHC inRange   = \ aℓ a AgdaIx -> Data.Ix.inRange   #-}
{-# COMPILE GHC rangeSize = \ aℓ a AgdaIx -> Data.Ix.rangeSize #-}

postulate
    Ix[A]⇒Ord[A] : ⦃ Ix A ⦄ → Ord A

{-# COMPILE GHC Ix[A]⇒Ord[A] = \ aℓ a AgdaIx -> AgdaOrd #-}
