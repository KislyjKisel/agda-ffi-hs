{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Trans.Except where

open import Agda.Primitive                   using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Prelude             using (_$!_; case_of_)
open import Ffi.Hs.-base.Unit                using (⊤′)
open import Ffi.Hs.Control.Monad             using (return; _>>=_; _>>_; liftM)
open import Ffi.Hs.Control.Monad.Signatures  using (CallCC; Listen; Pass)
open import Ffi.Hs.Control.Monad.Trans.Class using (MonadTrans)
open import Ffi.Hs.Data.Either               using (Either; Left; Right; either; Functor[Either[A]])
open import Ffi.Hs.Data.Function             using (_$_; _∘_; flip; id)
open import Ffi.Hs.Data.Functor              using (fmap)
open import Ffi.Hs.Data.Functor.Identity     using (Identity; runIdentity; mkIdentity; Functor[Identity])
open import Ffi.Hs.Data.Tuple                using (mkTuple2)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Monad.Trans.Except
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.Monad.Trans.Class (AgdaMonadTrans(AgdaMonadTrans))
#-}

private
    variable
        ℓ : Level
        A B E E′ W : Set ℓ
        M N : Set ℓ → Set ℓ


data ExceptT (E : Set ℓ) (M : Set ℓ → Set ℓ) (A : Set ℓ) : Set ℓ where
    mkExceptT : M (Either E A) → ExceptT E M A

{-# FOREIGN GHC type AgdaExceptT ℓ = Control.Monad.Trans.Except.ExceptT #-}
{-# COMPILE GHC ExceptT = data(1) AgdaExceptT (Control.Monad.Trans.Except.ExceptT) #-}

postulate
    MonadTrans[ExceptT[E]]      : MonadTrans (ExceptT E)
    Monad[ExceptT[E,M]]         : ⦃ Monad M ⦄ → Monad (ExceptT E M)
    Functor[ExceptT[E,M]]       : ⦃ Functor M ⦄ → Functor (ExceptT E M)
    MonadFix[ExceptT[E,M]]      : ⦃ MonadFix M ⦄ → MonadFix (ExceptT E M)
    MonadFail[ExceptT[E,M]]     : ⦃ MonadFail M ⦄ → MonadFail (ExceptT E M)
    Applicative[ExceptT[E,M]]   : ⦃ Monad M ⦄ → Applicative (ExceptT E M)
    Foldable[ExceptT[E,M]]      : ⦃ Foldable M ⦄ → Foldable (ExceptT E M)
    Traversable[ExceptT[E,M]]   : ⦃ Traversable M ⦄ → Traversable (ExceptT E M)
    Contravariant[ExceptT[E,M]] : ⦃ Contravariant M ⦄ → Contravariant (ExceptT E M)
    Eq1[ExceptT[E,M]]           : ⦃ Eq E ⦄ → ⦃ Eq1 M ⦄ → Eq1 (ExceptT E M)
    Ord1[ExceptT[E,M]]          : ⦃ Ord E ⦄ → ⦃ Ord1 M ⦄ → Ord1 (ExceptT E M)
    Read1[ExceptT[E,M]]         : ⦃ Read E ⦄ → ⦃ Read1 M ⦄ → Read1 (ExceptT E M)
    Show1[ExceptT[E,M]]         : ⦃ Show E ⦄ → ⦃ Show1 M ⦄ → Show1 (ExceptT E M)
    MonadZip[ExceptT[E,M]]      : ⦃ MonadZip M ⦄ → MonadZip (ExceptT E M)
    MonadIO[ExceptT[E,M]]       : ⦃ MonadIO M ⦄ → MonadIO (ExceptT E M)
    Alternative[ExceptT[E,M]]   : ⦃ MonadPlus M ⦄ → ⦃ Monoid E ⦄ → Alternative (ExceptT E M)
    MonadPlus[ExceptT[E,M]]     : ⦃ MonadPlus M ⦄ → ⦃ Monoid E ⦄ → MonadPlus (ExceptT E M)
    Eq[ExceptT[E,M,A]]          : ⦃ Eq E ⦄ → ⦃ Eq1 M ⦄ → ⦃ Eq A ⦄ → Eq (ExceptT E M A)
    Ord[ExceptT[E,M,A]]         : ⦃ Ord E ⦄ → ⦃ Ord1 M ⦄ → ⦃ Ord A ⦄ → Ord (ExceptT E M A)
    Read[ExceptT[E,M,A]]        : ⦃ Read E ⦄ → ⦃ Read1 M ⦄ → ⦃ Read A ⦄ → Read (ExceptT E M A)
    Show[ExceptT[E,M,A]]        : ⦃ Show E ⦄ → ⦃ Show1 M ⦄ → ⦃ Show A ⦄ → Show (ExceptT E M A)

{-# COMPILE GHC MonadTrans[ExceptT[E]]      = \ eℓ e                                -> AgdaMonadTrans    #-}
{-# COMPILE GHC Monad[ExceptT[E,M]]         = \ mℓ m e AgdaMonad                    -> AgdaMonad         #-}
{-# COMPILE GHC Functor[ExceptT[E,M]]       = \ mℓ m e AgdaFunctor                  -> AgdaFunctor       #-}
{-# COMPILE GHC MonadFix[ExceptT[E,M]]      = \ mℓ m e AgdaMonadFix                 -> AgdaMonadFix      #-}
{-# COMPILE GHC MonadFail[ExceptT[E,M]]     = \ mℓ m e AgdaMonadFail                -> AgdaMonadFail     #-}
{-# COMPILE GHC Applicative[ExceptT[E,M]]   = \ mℓ m e AgdaMonad                    -> AgdaApplicative   #-}
{-# COMPILE GHC Foldable[ExceptT[E,M]]      = \ mℓ m e AgdaFoldable                 -> AgdaFoldable      #-}
{-# COMPILE GHC Traversable[ExceptT[E,M]]   = \ mℓ m e AgdaTraversable              -> AgdaTraversable   #-}
{-# COMPILE GHC Contravariant[ExceptT[E,M]] = \ mℓ m r AgdaContravariant            -> AgdaContravariant #-}
{-# COMPILE GHC Eq1[ExceptT[E,M]]           = \ mℓ m e AgdaEq AgdaEq1               -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[ExceptT[E,M]]          = \ mℓ m e AgdaOrd AgdaOrd1             -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[ExceptT[E,M]]         = \ mℓ m e AgdaRead AgdaRead1           -> AgdaRead1         #-}
{-# COMPILE GHC Show1[ExceptT[E,M]]         = \ mℓ m e AgdaShow AgdaShow1           -> AgdaShow1         #-}
{-# COMPILE GHC MonadZip[ExceptT[E,M]]      = \ mℓ m e AgdaMonadZip                 -> AgdaMonadZip      #-}
{-# COMPILE GHC MonadIO[ExceptT[E,M]]       = \ mℓ m r AgdaMonadIO                  -> AgdaMonadIO       #-}
{-# COMPILE GHC Alternative[ExceptT[E,M]]   = \ mℓ m r AgdaMonadPlus AgdaMonoid     -> AgdaAlternative   #-}
{-# COMPILE GHC MonadPlus[ExceptT[E,M]]     = \ mℓ m r AgdaMonadPlus AgdaMonoid     -> AgdaMonadPlus     #-}
{-# COMPILE GHC Eq[ExceptT[E,M,A]]          = \ ℓ e m a AgdaEq AgdaEq1 AgdaEq       -> AgdaEq            #-}
{-# COMPILE GHC Ord[ExceptT[E,M,A]]         = \ ℓ e m a AgdaOrd AgdaOrd1 AgdaOrd    -> AgdaOrd           #-}
{-# COMPILE GHC Read[ExceptT[E,M,A]]        = \ ℓ e m a AgdaRead AgdaRead1 AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Show[ExceptT[E,M,A]]        = \ ℓ e m a AgdaShow AgdaShow1 AgdaShow -> AgdaShow          #-}

runExceptT : ExceptT E M A → M (Either E A)
runExceptT (mkExceptT m) = m
{-# COMPILE GHC runExceptT = \ ℓ e m a -> Control.Monad.Trans.Except.runExceptT #-}

mapExceptT : (M (Either E A) → N (Either E′ B)) → ExceptT E M A → ExceptT E′ N B
mapExceptT f m = mkExceptT $ f (runExceptT m)
{-# COMPILE GHC mapExceptT = \ mℓ m e a nℓ n e' b -> Control.Monad.Trans.Except.mapExceptT #-}

withExceptT : ⦃ Functor M ⦄ → (E → E′) → ExceptT E M A → ExceptT E′ M A
withExceptT f = mapExceptT $ fmap $ either (Left ∘ f) Right
{-# COMPILE GHC withExceptT = \ ℓ e e' m a AgdaFunctor -> Control.Monad.Trans.Except.withExceptT #-}


Except : Set ℓ → Set ℓ → Set ℓ
Except E = ExceptT E Identity

except : ⦃ Monad M ⦄ → Either E A → ExceptT E M A
except m = mkExceptT (return m)
{-# COMPILE GHC except = \ ℓ m e a AgdaMonad -> Control.Monad.Trans.Except.except #-}

runExcept : Except E A → Either E A
runExcept (mkExceptT m) = runIdentity m
{-# COMPILE GHC runExcept = \ ℓ e a -> Control.Monad.Trans.Except.runExcept #-}

mapExcept : (Either E A → Either E′ B) → Except E A → Except E′ B
mapExcept f = mapExceptT (mkIdentity ∘ f ∘ runIdentity)
{-# COMPILE GHC mapExcept = \ aℓ e a bℓ e' b -> Control.Monad.Trans.Except.mapExcept #-}

withExcept : (E → E′) → Except E A → Except E′ A
withExcept = withExceptT ⦃ Functor[Identity] ⦄
{-# COMPILE GHC withExcept = \ ℓ e e' a -> Control.Monad.Trans.Except.withExcept #-}


throwE : ⦃ Monad M ⦄ → E → ExceptT E M A
throwE = mkExceptT ∘ return ∘ Left
{-# COMPILE GHC throwE = \ ℓ m e a AgdaMonad -> Control.Monad.Trans.Except.throwE #-}

catchE : ⦃ Monad M ⦄ → ExceptT E M A → (E → ExceptT E′ M A) → ExceptT E′ M A
catchE m h = mkExceptT $ do
    a ← runExceptT m
    case a of λ
        { (Left l)  → runExceptT (h l)
        ; (Right r) → return (Right r)
        }
{-# COMPILE GHC catchE = \ ℓ m e a e' AgdaMonad -> Control.Monad.Trans.Except.catchE #-}

handleE : ⦃ Monad M ⦄ → (E → ExceptT E′ M A) → ExceptT E M A → ExceptT E′ M A
handleE = flip catchE
{-# COMPILE GHC handleE = \ ℓ m e e' a AgdaMonad -> Control.Monad.Trans.Except.handleE #-}

tryE : ⦃ Monad M ⦄ → ExceptT E M A → ExceptT E M (Either E A)
tryE m = catchE (liftM Right m) (return ∘ Left)
    where instance _ = Monad[ExceptT[E,M]]

{-# COMPILE GHC tryE = \ ℓ m e a AgdaMonad -> Control.Monad.Trans.Except.tryE #-}

finallyE : ⦃ Monad M ⦄ → ExceptT E M A → ExceptT E M ⊤′ → ExceptT E M A
finallyE m closer = do
    res ← tryE m
    closer
    either throwE return res
    where instance _ = Monad[ExceptT[E,M]]
{-# COMPILE GHC finallyE = \ ℓ m e a AgdaMonad -> Control.Monad.Trans.Except.finallyE #-}


liftCallCC : CallCC M (Either E A) (Either E B) → CallCC (ExceptT E M) A B
liftCallCC callCC f = mkExceptT $
    callCC λ c →
    runExceptT (f (λ a → mkExceptT $ c (Right a)))
{-# COMPILE GHC liftCallCC = \ ℓ m e a b -> Control.Monad.Trans.Except.liftCallCC #-}

liftListen : ⦃ Monad M ⦄ → Listen W M (Either E A) → Listen W (ExceptT E M) A
liftListen listen = mapExceptT λ m → do
    (mkTuple2 a w) ← listen m
    return $! fmap (λ r → mkTuple2 r w) a
    where instance _ = Functor[Either[A]]
{-# COMPILE GHC liftListen = \ ℓ w m e a AgdaMonad -> Control.Monad.Trans.Except.liftListen #-}

liftPass : ⦃ Monad M ⦄ → Pass W M (Either E A) → Pass W (ExceptT E M) A
liftPass pass = mapExceptT λ m → pass $ do
    a ← m
    return $! case a of λ
        { (Left l)               → mkTuple2 (Left l) id
        ; (Right (mkTuple2 r f)) → mkTuple2 (Right r) f
        }
{-# COMPILE GHC liftPass = \ ℓ m w e a AgdaMonad -> Control.Monad.Trans.Except.liftPass #-}
