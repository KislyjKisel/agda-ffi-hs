{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.MArray where

open import Agda.Builtin.List        using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Monad)
open import Ffi.Hs.-base.Unit        using (⊤)
open import Ffi.Hs.Data.Array.IArray using (IArray)
open import Ffi.Hs.Data.Tuple        using (Tuple2)

open import Ffi.Hs.Data.Ix public

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Array.MArray
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Array.IArray (AgdaIArray)
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ → Set aℓ → Set aℓ
        B : Set aℓ → Set aℓ → Set bℓ
        E E' I J : Set aℓ
        M : Set aℓ → Set aℓ

postulate
    MArray : (Set aℓ → Set aℓ → Set aℓ) → Set aℓ → (Set aℓ → Set aℓ) → Set aℓ
    MArray[A,E,M]⇒Monad[M] : ⦃ MArray A E M ⦄ → Monad M

    newArray     : ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → Tuple2 I I → E → M (A I E)
    newArray-    : ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → Tuple2 I I → M (A I E)
    newListArray : ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → Tuple2 I I → List E → M (A I E)

    readArray  : ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → A I E → I → M E
    writeArray : ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → A I E → I → E → M ⊤

    mapArray   : ⦃ MArray A E' M ⦄ → ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → (E' → E) → A I E' → M (A I E)
    mapIndices : ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → ⦃ Ix J ⦄ → Tuple2 I I → (I → J) → A J E → M (A I E)

    getBounds : ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → A I E → M (Tuple2 I I)
    getElems  : ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → A I E → M (List E)
    getAssocs : ⦃ MArray A E M ⦄ → ⦃ Ix I ⦄ → A I E → M (List (Tuple2 I E))

    freeze : ⦃ Ix I ⦄ → ⦃ MArray A E M ⦄ → ⦃ IArray B E ⦄ → A I E → M (B I E)
    thaw   : ⦃ Ix I ⦄ → ⦃ IArray A E ⦄ → ⦃ MArray B E M ⦄ → A I E → M (B I E)

{-# FOREIGN GHC data AgdaMArray aℓ a e m = Data.Array.MArray.MArray a e m => AgdaMArray #-}
{-# COMPILE GHC MArray = type(0) AgdaMArray #-}

{-# COMPILE GHC MArray[A,E,M]⇒Monad[M] = \ aℓ a e m AgdaMArray -> AgdaMonad #-}

{-# COMPILE GHC newArray     = \ aℓ a e m i AgdaMArray AgdaIx -> Data.Array.MArray.newArray     #-}
{-# COMPILE GHC newArray-    = \ aℓ a e m i AgdaMArray AgdaIx -> Data.Array.MArray.newArray_    #-}
{-# COMPILE GHC newListArray = \ aℓ a e m i AgdaMArray AgdaIx -> Data.Array.MArray.newListArray #-}

{-# COMPILE GHC readArray  = \ aℓ a e m i AgdaMArray AgdaIx -> Data.Array.MArray.readArray  #-}
{-# COMPILE GHC writeArray = \ aℓ a e m i AgdaMArray AgdaIx -> Data.Array.MArray.writeArray #-}

{-# COMPILE GHC mapArray   = \ aℓ a e' m e i AgdaMArray AgdaMArray AgdaIx -> Data.Array.MArray.mapArray   #-}
{-# COMPILE GHC mapIndices = \ aℓ a e m i j AgdaMArray AgdaIx AgdaIx      -> Data.Array.MArray.mapIndices #-}

{-# COMPILE GHC getBounds = \ aℓ a e m i AgdaMArray AgdaIx -> Data.Array.MArray.getBounds #-}
{-# COMPILE GHC getElems  = \ aℓ a e m i AgdaMArray AgdaIx -> Data.Array.MArray.getElems  #-}
{-# COMPILE GHC getAssocs = \ aℓ a e m i AgdaMArray AgdaIx -> Data.Array.MArray.getAssocs #-}

{-# COMPILE GHC freeze = \ aℓ i a e m b AgdaIx AgdaMArray AgdaIArray    -> Data.Array.MArray.freeze #-}
{-# COMPILE GHC thaw   = \ bℓ i aℓ a e b m AgdaIx AgdaIArray AgdaMArray -> Data.Array.MArray.thaw   #-}
