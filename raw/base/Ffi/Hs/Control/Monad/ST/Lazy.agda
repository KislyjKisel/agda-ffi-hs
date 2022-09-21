{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.ST.Lazy where

open import Agda.Builtin.IO    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Functor; Applicative; Monad; MonadFix)
open import Ffi.Hs.GHC.Exts    using (RealWorld)
open import Ffi.Hs.Control.Monad.ST as Strict using ()

{-# FOREIGN GHC
import qualified Control.Monad.ST.Lazy
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        S : Set

postulate
    ST : Set → Set aℓ → Set aℓ
    runST  : ({S : Set} → ST S A) → A
    fixST  : (A → ST S A) → ST S A
    stToIO : ST RealWorld A → IO A

    strictToLazyST : Strict.ST S A → ST S A
    lazyToStrictST : ST S A → Strict.ST S A

{-# FOREIGN GHC type AgdaST aℓ = Control.Monad.ST.Lazy.ST #-}
{-# COMPILE GHC ST = type(1) AgdaST #-}
{-# COMPILE GHC runST  = \ aℓ a f -> Control.Monad.ST.Lazy.runST (f ()) #-}
{-# COMPILE GHC fixST  = \ aℓ a s -> Control.Monad.ST.Lazy.fixST        #-}
{-# COMPILE GHC stToIO = \ aℓ a   -> Control.Monad.ST.Lazy.stToIO       #-}

{-# COMPILE GHC strictToLazyST = \ s aℓ a -> Control.Monad.ST.Lazy.strictToLazyST #-}
{-# COMPILE GHC lazyToStrictST = \ s aℓ a -> Control.Monad.ST.Lazy.lazyToStrictST #-}

postulate
    Functor[ST[S]]     : Functor {aℓ} (ST S)
    Applicative[ST[S]] : Applicative {aℓ} (ST S)
    Monad[ST[S]]       : Monad {aℓ} (ST S)
    MonadFix[ST[S]]    : MonadFix {aℓ} (ST S)

{-# COMPILE GHC Functor[ST[S]]     = \ s aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Applicative[ST[S]] = \ s aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Monad[ST[S]]       = \ s aℓ -> AgdaMonad       #-}
{-# COMPILE GHC MonadFix[ST[S]]    = \ s aℓ -> AgdaMonadFix    #-}
