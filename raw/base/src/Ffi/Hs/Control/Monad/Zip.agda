{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Zip where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Monad)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

open Ffi.Hs.-base.Class public
    using (MonadZip)

{-# FOREIGN GHC
import qualified Control.Monad.Zip
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class(AgdaMonad, AgdaMonadZip)
#-}

private
    variable
        aℓ mℓ : Level
        A B C : Set aℓ
        M : Set mℓ → Set mℓ

postulate
    mzip     : ⦃ MonadZip M ⦄ → M A → M B → M (Tuple2 A B)
    mzipWith : ⦃ MonadZip M ⦄ → (A → B → C) → M A → M B → M C
    munzip   : ⦃ MonadZip M ⦄ → M (Tuple2 A B) → Tuple2 (M A) (M B)

{-# COMPILE GHC mzip     = \ mℓ m a b   AgdaMonadZip -> Control.Monad.Zip.mzip     #-}
{-# COMPILE GHC mzipWith = \ mℓ m a b c AgdaMonadZip -> Control.Monad.Zip.mzipWith #-}
{-# COMPILE GHC munzip   = \ mℓ m a b   AgdaMonadZip -> Control.Monad.Zip.munzip   #-}

postulate
    MonadZip[M]⇒Monad[M] : ⦃ MonadZip M ⦄ → Monad M

{-# COMPILE GHC MonadZip[M]⇒Monad[M] = \ mℓ m AgdaMonadZip -> AgdaMonad #-}
