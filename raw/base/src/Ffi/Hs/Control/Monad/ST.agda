{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.ST where

open import Agda.Builtin.IO    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Show; Functor; Applicative; Monad; MonadFix; Semigroup; Monoid)
open import Ffi.Hs.GHC.Exts    using (RealWorld)

{-# FOREIGN GHC
import qualified Control.Monad.ST
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaShow, AgdaFunctor, AgdaApplicative, AgdaMonad
    , AgdaMonadFix, AgdaSemigroup, AgdaMonoid
    )
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

{-# FOREIGN GHC type AgdaST sℓ aℓ = Control.Monad.ST.ST #-}
{-# COMPILE GHC ST = type(2) AgdaST #-}
{-# COMPILE GHC runST  = \ aℓ a f -> Control.Monad.ST.runST (f ()) #-}
{-# COMPILE GHC fixST  = \ aℓ a s -> Control.Monad.ST.fixST        #-}
{-# COMPILE GHC stToIO = \ aℓ a   -> Control.Monad.ST.stToIO       #-}

postulate
    Show[ST[S,A]]      : Show (ST S A)
    Functor[ST[S]]     : Functor {aℓ} (ST S)
    Applicative[ST[S]] : Applicative {aℓ} (ST S)
    Monad[ST[S]]       : Monad {aℓ} (ST S)
    MonadFix[ST[S]]    : MonadFix {aℓ} (ST S)
    Semigroup[ST[S,A]] : ⦃ Semigroup A ⦄ → Semigroup (ST S A)
    Monoid[ST[S,A]]    : ⦃ Monoid A ⦄ → Monoid (ST S A)

{-# COMPILE GHC Show[ST[S,A]]      = \ s aℓ a               -> AgdaShow        #-}
{-# COMPILE GHC Functor[ST[S]]     = \ s aℓ                 -> AgdaFunctor     #-}
{-# COMPILE GHC Applicative[ST[S]] = \ s aℓ                 -> AgdaApplicative #-}
{-# COMPILE GHC Monad[ST[S]]       = \ s aℓ                 -> AgdaMonad       #-}
{-# COMPILE GHC MonadFix[ST[S]]    = \ s aℓ                 -> AgdaMonadFix    #-}
{-# COMPILE GHC Semigroup[ST[S,A]] = \ s aℓ a AgdaSemigroup -> AgdaSemigroup   #-}
{-# COMPILE GHC Monoid[ST[S,A]]    = \ s aℓ a AgdaMonoid    -> AgdaMonoid      #-}
