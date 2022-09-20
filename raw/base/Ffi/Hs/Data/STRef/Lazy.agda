{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.STRef.Lazy where

open import Agda.Primitive
open import Ffi.Hs.-base.Unit            using (⊤; ⊤′)
open import Ffi.Hs.Control.Monad.ST.Lazy using (ST)

{-# FOREIGN GHC
import qualified Data.STRef.Lazy
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        S : Set

postulate
    STRef        : Set → Set aℓ → Set aℓ
    newSTRef     : A → ST S (STRef S A)
    readSTRef    : STRef S A → ST S A
    writeSTRef   : STRef S A → A → ST S ⊤
    modifySTRef  : STRef S A → (A → A) → ST S ⊤

{-# FOREIGN GHC type AgdaSTRef aℓ = Data.STRef.STRef #-}
{-# COMPILE GHC STRef = type(1) AgdaSTRef #-}

{-# COMPILE GHC newSTRef     = \ aℓ a s -> Data.STRef.newSTRef     #-}
{-# COMPILE GHC readSTRef    = \ aℓ a s -> Data.STRef.readSTRef    #-}
{-# COMPILE GHC writeSTRef   = \ aℓ a s -> Data.STRef.writeSTRef   #-}
{-# COMPILE GHC modifySTRef  = \ aℓ a s -> Data.STRef.modifySTRef  #-}
