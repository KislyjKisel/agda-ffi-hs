{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.IArray where

open import Agda.Builtin.List using (List)
open import Agda.Primitive
open import Ffi.Hs.Data.Tuple using (Tuple2)

open import Ffi.Hs.Data.Ix public

open import Ffi.Hs.Data.Array.Base public
    using (Array)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Array.IArray
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        iℓ eℓ aℓ : Level
        I J : Set iℓ
        E E' : Set eℓ
        A : Set iℓ → Set eℓ → Set aℓ

postulate
    IArray : (Set iℓ → Set eℓ → Set aℓ) → Set eℓ → Set (iℓ ⊔ eℓ ⊔ aℓ)

{-# FOREIGN GHC data AgdaIArray iℓ eℓ aℓ a e = Data.Array.IArray.IArray a e => AgdaIArray #-}
{-# COMPILE GHC IArray = type(0) AgdaIArray #-}

postulate
    array      : ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → Tuple2 I I → List (Tuple2 I E) → A I E
    listArray  : ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → Tuple2 I I → List E → A I E
    accumArray : ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → (E → E' → E) → E → Tuple2 I I → List (Tuple2 I E') → A I E

    _!_     : ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → A I E → I → E
    bounds  : ⦃ IArray A E ⦄ → A I E → Tuple2 I I
    indices : ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → A I E → List I
    elems   : ⦃ IArray A E ⦄ → A I E → List E
    assocs  : ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → A I E → List (Tuple2 I E)

    _//_  : ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → A I E → List (Tuple2 I E) → A I E
    accum : ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → (E → E' → E) → A I E → List (Tuple2 I E') → A I E
    amap  : ⦃ IArray A E' ⦄ → ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → (E' → E) → A I E' → A I E
    ixmap : ⦃ IArray A E ⦄ → ⦃ Ix I ⦄ → ⦃ Ix J ⦄ → Tuple2 I I → (I → J) → A J E → A I E

{-# COMPILE GHC array      = \ iℓ eℓ a e i AgdaIArray AgdaIx        -> Data.Array.IArray.array      #-}
{-# COMPILE GHC listArray  = \ iℓ eℓ a e i AgdaIArray AgdaIx        -> Data.Array.IArray.listArray  #-}
{-# COMPILE GHC accumArray = \ iℓ eℓ a e i e'ℓ e' AgdaIArray AgdaIx -> Data.Array.IArray.accumArray #-}

{-# COMPILE GHC _!_     = \ iℓ eℓ a e i AgdaIArray AgdaIx -> (Data.Array.IArray.!)     #-}
{-# COMPILE GHC bounds  = \ iℓ eℓ a e i AgdaIArray        -> Data.Array.IArray.bounds  #-}
{-# COMPILE GHC indices = \ iℓ eℓ a e i AgdaIArray AgdaIx -> Data.Array.IArray.indices #-}
{-# COMPILE GHC elems   = \ iℓ eℓ a e i AgdaIArray        -> Data.Array.IArray.elems   #-}
{-# COMPILE GHC assocs  = \ iℓ eℓ a e i AgdaIArray AgdaIx -> Data.Array.IArray.assocs  #-}

{-# COMPILE GHC _//_  = \ iℓ eℓ a e i AgdaIArray AgdaIx               -> (Data.Array.IArray.//)  #-}
{-# COMPILE GHC accum = \ iℓ eℓ a e i e'ℓ e' AgdaIArray AgdaIx        -> Data.Array.IArray.accum #-}
{-# COMPILE GHC amap  = \ iℓ eℓ a e e' i AgdaIArray AgdaIArray AgdaIx -> Data.Array.IArray.amap  #-}
{-# COMPILE GHC ixmap = \ iℓ eℓ a i j e AgdaIArray AgdaIx AgdaIx      -> Data.Array.IArray.ixmap #-}
