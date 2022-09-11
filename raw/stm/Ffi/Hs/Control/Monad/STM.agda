{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.STM where

open import Agda.Builtin.Bool             using (Bool)
open import Agda.Builtin.IO               using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit             using (⊤)
open import Ffi.Hs.Control.Exception.Base using (Exception)

open import Ffi.Hs.-base.STM public
    using (STM)

{-# FOREIGN GHC
import qualified Control.Monad.STM
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A E : Set aℓ

postulate
    atomically : STM A → IO A
    retry      : STM A
    orElse     : STM A → STM A → STM A
    check      : Bool → STM (⊤ {lzero})
    throwSTM   : ⦃ Exception E ⦄ → E → STM A
    catchSTM   : ⦃ Exception E ⦄ → STM A → (E → STM A) → STM A

{-# COMPILE GHC atomically = \ aℓ a -> Control.Monad.STM.atomically #-}
{-# COMPILE GHC retry      = \ aℓ a -> Control.Monad.STM.retry      #-}
{-# COMPILE GHC orElse     = \ aℓ a -> Control.Monad.STM.orElse     #-}
{-# COMPILE GHC check      =           Control.Monad.STM.check      #-}
{-# COMPILE GHC throwSTM   = \ eℓ e aℓ a AgdaException -> Control.Monad.STM.throwSTM #-}
{-# COMPILE GHC catchSTM   = \ eℓ e aℓ a AgdaException -> Control.Monad.STM.catchSTM #-}

postulate
    Functor[STM]      : Functor {aℓ} STM
    Applicative[STM]  : Applicative {aℓ} STM
    Alternative[STM]  : Alternative {aℓ} STM
    Monad[STM]        : Monad {aℓ} STM
    MonadPlus[STM]    : MonadPlus {aℓ} STM
    MonadFix[STM]     : MonadFix {aℓ} STM
    Semigroup[STM[A]] : ⦃ Semigroup A ⦄ → Semigroup (STM A)
    Monoid[STM[A]]    : ⦃ Monoid A ⦄ → Monoid (STM A)

{-# COMPILE GHC Functor[STM]      = \ aℓ                 -> AgdaFunctor     #-}
{-# COMPILE GHC Applicative[STM]  = \ aℓ                 -> AgdaApplicative #-}
{-# COMPILE GHC Alternative[STM]  = \ aℓ                 -> AgdaAlternative #-}
{-# COMPILE GHC Monad[STM]        = \ aℓ                 -> AgdaMonad       #-}
{-# COMPILE GHC MonadPlus[STM]    = \ aℓ                 -> AgdaMonadPlus   #-}
{-# COMPILE GHC MonadFix[STM]     = \ aℓ                 -> AgdaMonadFix    #-}
{-# COMPILE GHC Semigroup[STM[A]] = \ aℓ a AgdaSemigroup -> AgdaSemigroup   #-}
{-# COMPILE GHC Monoid[STM[A]]    = \ aℓ a AgdaMonoid    -> AgdaMonoid      #-}
