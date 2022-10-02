{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array where

open import Agda.Builtin.List        using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Array.IArray using (IArray)
open import Ffi.Hs.Data.Tuple        using (Tuple2)

open import Ffi.Hs.Data.Ix public

open import Ffi.Hs.Data.Array.Base public
    using (Array)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Array
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Array.IArray (AgdaIArray)
#-}

private
    variable
        iℓ eℓ aℓ : Level
        I J : Set iℓ
        E : Set eℓ
        A : Set aℓ

infixl 9 _!_ _//_

postulate
    array      : ⦃ Ix I ⦄ → Tuple2 I I → List (Tuple2 I E) → Array I E
    listArray  : ⦃ Ix I ⦄ → Tuple2 I I → List E → Array I E
    accumArray : ⦃ Ix I ⦄ → (E → A → E) → E → Tuple2 I I → List (Tuple2 I A) → Array I E

    _!_     : ⦃ Ix I ⦄ → Array I E → I → E
    bounds  : Array I E → Tuple2 I I
    indices : ⦃ Ix I ⦄ → Array I E → List I
    elems   : Array I E → List E
    assocs  : ⦃ Ix I ⦄ → Array I E → List (Tuple2 I E)

    _//_  : ⦃ Ix I ⦄ → Array I E → List (Tuple2 I E) → Array I E
    accum : ⦃ Ix I ⦄ → (E → A → E) → Array I E → List (Tuple2 I A) → Array I E
    ixmap : ⦃ Ix I ⦄ → ⦃ Ix J ⦄ → Tuple2 I I → (I → J) → Array J E → Array I E

{-# COMPILE GHC array      = \ iℓ i eℓ e AgdaIx      -> Data.Array.array      #-}
{-# COMPILE GHC listArray  = \ iℓ i eℓ e AgdaIx      -> Data.Array.listArray  #-}
{-# COMPILE GHC accumArray = \ iℓ i eℓ e aℓ a AgdaIx -> Data.Array.accumArray #-}

{-# COMPILE GHC _!_     = \ iℓ i eℓ e AgdaIx -> (Data.Array.!)     #-}
{-# COMPILE GHC bounds  = \ iℓ i eℓ e        -> Data.Array.bounds  #-}
{-# COMPILE GHC indices = \ iℓ i eℓ e AgdaIx -> Data.Array.indices #-}
{-# COMPILE GHC elems   = \ iℓ i eℓ e        -> Data.Array.elems   #-}
{-# COMPILE GHC assocs  = \ iℓ i eℓ e AgdaIx -> Data.Array.assocs  #-}

{-# COMPILE GHC _//_  = \ iℓ i eℓ e AgdaIx             -> (Data.Array.//)  #-}
{-# COMPILE GHC accum = \ iℓ i eℓ e aℓ a AgdaIx        -> Data.Array.accum #-}
{-# COMPILE GHC ixmap = \ iℓ i j eℓ e AgdaIx AgdaIx -> Data.Array.ixmap #-}

postulate
    IArray[Array,E]       : IArray (Array {iℓ}) E
    Functor[Array[I]]     : Functor (Array {eℓ} {eℓ} I)
    Foldable[Array[I]]    : Foldable (Array {eℓ = eℓ} I)
    Traversable[Array[I]] : {I : Set eℓ} → ⦃ Ix I ⦄ → Traversable (Array {eℓ = eℓ} I)
    Eq[Array[I,E]]        : ⦃ Ix I ⦄ → ⦃ Eq E ⦄ → Eq (Array I E)
    Ord[Array[I,E]]       : ⦃ Ix I ⦄ → ⦃ Ord E ⦄ → Ord (Array I E)
    Read[Array[I,E]]      : ⦃ Ix I ⦄ → ⦃ Read I ⦄ → ⦃ Read E ⦄ → Read (Array I E)
    Show[Array[I,E]]      : ⦃ Ix I ⦄ → ⦃ Show I ⦄ → ⦃ Show E ⦄ → Show (Array I E)

{-# COMPILE GHC IArray[Array,E]       = \ iℓ eℓ e                            -> AgdaIArray      #-}
{-# COMPILE GHC Functor[Array[I]]     = \ eℓ                                 -> AgdaFunctor     #-}
{-# COMPILE GHC Foldable[Array[I]]    = \ eℓ iℓ i                            -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Array[I]] = \ eℓ i AgdaIx                        -> AgdaTraversable #-}
{-# COMPILE GHC Eq[Array[I,E]]        = \ iℓ i eℓ e AgdaIx AgdaEq            -> AgdaEq          #-}
{-# COMPILE GHC Ord[Array[I,E]]       = \ iℓ i eℓ e AgdaIx AgdaOrd           -> AgdaOrd         #-}
{-# COMPILE GHC Read[Array[I,E]]      = \ iℓ i eℓ e AgdaIx AgdaRead AgdaRead -> AgdaRead        #-}
{-# COMPILE GHC Show[Array[I,E]]      = \ iℓ i eℓ e AgdaIx AgdaShow AgdaShow -> AgdaShow        #-}
