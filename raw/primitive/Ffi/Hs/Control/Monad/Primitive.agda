{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Primitive where

open import Ffi.Hs.Control.Monad.ST using () renaming (ST to StrictST)
open import Ffi.Hs.Control.Monad.ST.Lazy using () renaming (ST to LazyST)
open import Agda.Builtin.IO using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Monad)
open import Ffi.Hs.GHC.Exts    using (State#; Tuple2#)
open import Ffi.Hs.-base.Unit using (⊤)

{-# FOREIGN GHC
import qualified Control.Monad.Primitive
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        M : Set aℓ → Set aℓ
        S : Set

postulate
    PrimMonad : (Set aℓ → Set aℓ) → Set aℓ

    PrimState : (M : Set aℓ → Set aℓ) → ⦃ PrimMonad M ⦄ → Set

    primitiv  : ⦃ _ : PrimMonad M ⦄ → (State# (PrimState M) → Tuple2# (State# (PrimState M)) A) → M A
    primitiv- : ⦃ _ : PrimMonad M ⦄ → (State# (PrimState M) → State# (PrimState M)) → M ⊤

    PrimMonad[M]⇒Monad[M] : ⦃ PrimMonad M ⦄ → Monad M

{-# FOREIGN GHC data AgdaPrimMonad mℓ m = Control.Monad.Primitive.PrimMonad m => AgdaPrimMonad #-}
{-# COMPILE GHC PrimMonad = type(0) AgdaPrimMonad #-}

{-# FOREIGN GHC type AgdaPrimState mℓ m pm = Control.Monad.Primitive.PrimState m #-}
{-# COMPILE GHC PrimState = type(3) AgdaPrimState #-}

{-# COMPILE GHC primitiv  = \ mℓ m a AgdaPrimMonad -> Control.Monad.Primitive.primitive  #-}
{-# COMPILE GHC primitiv- = \ mℓ m   AgdaPrimMonad -> Control.Monad.Primitive.primitive_ #-}

{-# COMPILE GHC PrimMonad[M]⇒Monad[M] = \ mℓ m AgdaPrimMonad -> AgdaMonad #-}

postulate
    PrimMonad[IO]  : PrimMonad (IO {aℓ})
    PrimMonad[SST] : PrimMonad (StrictST {aℓ} S)
    PrimMonad[LST] : PrimMonad (LazyST {aℓ} S)

{-# COMPILE GHC PrimMonad[IO]  = \ aℓ   -> AgdaPrimMonad #-}
{-# COMPILE GHC PrimMonad[SST] = \ aℓ s -> AgdaPrimMonad #-}
{-# COMPILE GHC PrimMonad[LST] = \ aℓ s -> AgdaPrimMonad #-}

-- todo: instances for transformers

-- todo: PrimBase+instances, MonadPrim+i, MonadPrimBase+i
-- todo: funcs
