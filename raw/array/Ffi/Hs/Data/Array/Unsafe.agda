{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.Unsafe where

open import Agda.Builtin.IO            using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class         using (Ix)
open import Ffi.Hs.Control.Monad.ST    using (ST)
open import Ffi.Hs.Data.Array.IArray   using (IArray)
open import Ffi.Hs.Data.Array.IO       using (IOUArray)
open import Ffi.Hs.Data.Array.MArray   using (MArray)
open import Ffi.Hs.Data.Array.ST       using (STUArray)
open import Ffi.Hs.Data.Array.Storable using (StorableArray)
open import Ffi.Hs.Data.Tuple          using (Tuple2)
open import Ffi.Hs.Foreign.ForeignPtr  using (ForeignPtr)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Array.Unsafe
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.Array.IArray (AgdaIArray)
import MAlonzo.Code.Ffi.Hs.Data.Array.MArray (AgdaMArray)
#-}

private
    variable
        aℓ iℓ eℓ : Level
        E E' : Set eℓ
        I : Set iℓ
        S : Set
        A B : Set iℓ → Set eℓ → Set aℓ
        M : Set aℓ → Set aℓ

postulate
    castSTUArray                    : STUArray S I E → ST S (STUArray S I E')
    castIOUArray                    : IOUArray I E → IO (IOUArray I E')
    unsafeFreeze                    : ⦃ Ix I ⦄ → ⦃ MArray A E M ⦄ → ⦃ IArray B E ⦄ → A I E → M (B I E)
    unsafeThaw                      : ⦃ Ix I ⦄ → ⦃ IArray A E ⦄ → ⦃ MArray B E M ⦄ → A I E → M (B I E)
    unsafeForeignPtrToStorableArray : ⦃ Ix I ⦄ → ForeignPtr E → Tuple2 I I → IO (StorableArray I E)

{-# COMPILE GHC castSTUArray                    = \ s aℓ i e e'                                  -> Data.Array.Unsafe.castSTUArray                    #-}
{-# COMPILE GHC castIOUArray                    = \ aℓ i e e'                                    -> Data.Array.Unsafe.castIOUArray                    #-}
{-# COMPILE GHC unsafeFreeze                    = \ aℓ i e a b m AgdaIx AgdaMArray AgdaIArray    -> Data.Array.Unsafe.unsafeFreeze                    #-}
{-# COMPILE GHC unsafeThaw                      = \ aℓ bℓ i e a b m AgdaIx AgdaIArray AgdaMArray -> Data.Array.Unsafe.unsafeThaw                      #-}
{-# COMPILE GHC unsafeForeignPtrToStorableArray = \ iℓ i e AgdaIx                                -> Data.Array.Unsafe.unsafeForeignPtrToStorableArray #-}
