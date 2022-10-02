{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Trans.State.Strict where

open import Agda.Builtin.Strict              using (primForce)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level               using (Liftℓ; liftℓ)
open import Ffi.Hs.-base.Unit                using (⊤′; tt′)
open import Ffi.Hs.Control.Monad             using (_>>=_; return)
open import Ffi.Hs.Control.Monad.Signatures
open import Ffi.Hs.Control.Monad.Trans.Class using (MonadTrans)
open import Ffi.Hs.Data.Function             using (_$_; _∘_)
open import Ffi.Hs.Data.Functor.Identity     using (Identity; runIdentity; mkIdentity)
open import Ffi.Hs.Data.Tuple                using (Tuple2; mkTuple2; fst; snd)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Monad.Trans.State.Strict
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        sℓ aℓ : Level
        A B E S W : Set aℓ
        M N : Set aℓ → Set aℓ


record StateT (S : Set aℓ) (M : Set aℓ → Set aℓ) (A : Set aℓ) : Set aℓ where
    constructor mkStateT
    field
        runStateT : S → M (Tuple2 A S)

open StateT public

{-# FOREIGN GHC type AgdaStateT sℓ aℓ = Control.Monad.Trans.State.Strict.StateT #-}
{-# COMPILE GHC StateT = data(2) AgdaStateT (Control.Monad.Trans.State.Strict.StateT) #-}

postulate
    MonadTrans[StateT[S]]      : MonadTrans (StateT S)
    Monad[StateT[S,M]]         : ⦃ Monad M ⦄ → Monad (StateT S M)
    Functor[StateT[S,M]]       : ⦃ Functor M ⦄ → Functor (StateT S M)
    MonadFix[StateT[S,M]]      : ⦃ MonadFix M ⦄ → MonadFix (StateT S M)
    MonadFail[StateT[S,M]]     : ⦃ MonadFail M ⦄ → MonadFail (StateT S M)
    Applicative[StateT[S,M]]   : ⦃ Monad M ⦄ → Applicative (StateT S M)
    Contravariant[StateT[S,M]] : ⦃ Contravariant M ⦄ → Contravariant (StateT S M)
    MonadIO[StateT[S,M]]       : ⦃ MonadIO M ⦄ → MonadIO (StateT S M)
    Alternative[StateT[S,M]]   : ⦃ MonadPlus M ⦄ → Alternative (StateT S M)
    MonadPlus[StateT[S,M]]     : ⦃ MonadPlus M ⦄ → MonadPlus (StateT S M)

{-# COMPILE GHC MonadTrans[StateT[S]]      = \ sℓ s                     -> AgdaMonadTrans    #-}
{-# COMPILE GHC Monad[StateT[S,M]]         = \ mℓ m s AgdaMonad         -> AgdaMonad         #-}
{-# COMPILE GHC Functor[StateT[S,M]]       = \ mℓ m s AgdaFunctor       -> AgdaFunctor       #-}
{-# COMPILE GHC MonadFix[StateT[S,M]]      = \ mℓ m s AgdaMonadFix      -> AgdaMonadFix      #-}
{-# COMPILE GHC MonadFail[StateT[S,M]]     = \ mℓ m s AgdaMonadFail     -> AgdaMonadFail     #-}
{-# COMPILE GHC Applicative[StateT[S,M]]   = \ mℓ m s AgdaMonad         -> AgdaApplicative   #-}
{-# COMPILE GHC Contravariant[StateT[S,M]] = \ mℓ m s AgdaContravariant -> AgdaContravariant #-}
{-# COMPILE GHC MonadIO[StateT[S,M]]       = \ mℓ m s AgdaMonadIO       -> AgdaMonadIO       #-}
{-# COMPILE GHC Alternative[StateT[S,M]]   = \ mℓ m s AgdaMonadPlus     -> AgdaAlternative   #-}
{-# COMPILE GHC MonadPlus[StateT[S,M]]     = \ mℓ m s AgdaMonadPlus     -> AgdaMonadPlus     #-}


evalStateT : ⦃ Monad M ⦄ → StateT S M A → S → M A
evalStateT m s = runStateT m s >>= λ (mkTuple2 a _) → return a

{-# INLINE evalStateT #-}
{-# COMPILE GHC evalStateT = \ mℓ m s a AgdaMonad -> Control.Monad.Trans.State.Strict.evalStateT #-}

execStateT : ⦃ Monad M ⦄ → StateT S M A → S → M S
execStateT m s = runStateT m s >>= λ (mkTuple2 _ s') → return s'

{-# INLINE execStateT #-}
{-# COMPILE GHC execStateT = \ mℓ m s a AgdaMonad -> Control.Monad.Trans.State.Strict.execStateT #-}

mapStateT : (M (Tuple2 A S) → N (Tuple2 B S)) → StateT S M A → StateT S N B
mapStateT f m = mkStateT $ f ∘ runStateT m

{-# INLINE mapStateT #-}
{-# COMPILE GHC mapStateT = \ mℓ m a s n b -> Control.Monad.Trans.State.Strict.mapStateT #-}

withStateT : (S → S) → StateT S M A → StateT S M A
withStateT f m = mkStateT $ runStateT m ∘ f

{-# INLINE withStateT #-}
{-# COMPILE GHC withStateT = \ sℓ s m a -> Control.Monad.Trans.State.Strict.withStateT #-}


State : (S : Set sℓ) → Set sℓ → Set sℓ
State S = StateT S Identity

runState : State S A → S → Tuple2 A S
runState m = runIdentity ∘ runStateT m

{-# INLINE runState #-}
{-# COMPILE GHC runState = \ sℓ s a -> Control.Monad.Trans.State.Strict.runState #-}

evalState : State S A → S → A
evalState m s = fst $ runState m s

{-# INLINE evalState #-}
{-# COMPILE GHC evalState = \ sℓ s a -> Control.Monad.Trans.State.Strict.evalState #-}

execState : State S A → S → S
execState m s = snd $ runState m s

{-# INLINE execState #-}
{-# COMPILE GHC execState = \ sℓ s a -> Control.Monad.Trans.State.Strict.execState #-}

mapState : (Tuple2 A S → Tuple2 B S) → State S A → State S B
mapState f = mapStateT $ mkIdentity ∘ f ∘ runIdentity

{-# INLINE mapState #-}
{-# COMPILE GHC mapState = \ aℓ a s b -> Control.Monad.Trans.State.Strict.mapState #-}

withState : (S → S) → State S A → State S A
withState = withStateT

{-# INLINE withState #-}
{-# COMPILE GHC withState = \ sℓ s a -> Control.Monad.Trans.State.Strict.withState #-}


state : ⦃ Monad M ⦄ → (S → Tuple2 A S) → StateT S M A
state f = mkStateT $ return ∘ f

{-# INLINE state #-}
{-# COMPILE GHC state = \ mℓ m s a AgdaMonad -> Control.Monad.Trans.State.Strict.state #-}

get : ⦃ Monad M ⦄ → StateT S M S
get = state λ s → mkTuple2 s s

{-# INLINE get #-}
{-# COMPILE GHC get = \ mℓ m s AgdaMonad -> Control.Monad.Trans.State.Strict.get #-}

put : ⦃ Monad M ⦄ → S → StateT S M ⊤′
put s = state λ _ → mkTuple2 tt′ s

{-# INLINE put #-}
{-# COMPILE GHC put = \ mℓ m s AgdaMonad -> Control.Monad.Trans.State.Strict.put #-}

modify : ⦃ Monad M ⦄ → (S → S) → StateT S M ⊤′
modify f = state λ s → mkTuple2 tt′ (f s)

{-# INLINE modify #-}
{-# COMPILE GHC modify = \ mℓ m s AgdaMonad -> Control.Monad.Trans.State.Strict.modify #-}

modify' : ⦃ Monad M ⦄ → (S → S) → StateT S M ⊤′
modify' f = get >>= λ s → primForce (f s) put
    where
    instance _ = Monad[StateT[S,M]]

{-# INLINE modify' #-}
{-# COMPILE GHC modify' = \ mℓ m s AgdaMonad -> Control.Monad.Trans.State.Strict.modify' #-}

gets : ⦃ Monad M ⦄ → (S → A) → StateT S M A
gets f = state λ s → mkTuple2 (f s) s

{-# INLINE gets #-}
{-# COMPILE GHC gets = \ mℓ m s a AgdaMonad -> Control.Monad.Trans.State.Lazy.gets #-}


liftCallCC : CallCC M (Tuple2 A S) (Tuple2 B S) → CallCC (StateT S M) A B
liftCallCC callCC f = mkStateT λ s →
    callCC λ c →
    runStateT (f λ a → mkStateT λ _ → c (mkTuple2 a s)) s

{-# INLINE liftCallCC #-}
{-# COMPILE GHC liftCallCC = \ mℓ m a s b s -> Control.Monad.Trans.State.Strict.liftCallCC #-}

liftCallCC' : CallCC M (Tuple2 A S) (Tuple2 B S) → CallCC (StateT S M) A B
liftCallCC' callCC f = mkStateT λ s →
    callCC λ c →
    runStateT (f λ a → mkStateT λ s' → c $ mkTuple2 a s') s

{-# INLINE liftCallCC' #-}
{-# COMPILE GHC liftCallCC' = \ mℓ m a s b s -> Control.Monad.Trans.State.Strict.liftCallCC' #-}

liftCatch : Catch E M (Tuple2 A S) → Catch E (StateT S M) A
liftCatch catchE m h =
    mkStateT λ s → catchE (runStateT m s) (λ e → runStateT (h e) s)

{-# INLINE liftCatch #-}
{-# COMPILE GHC liftCatch = \ eℓ e m a s -> Control.Monad.Trans.State.Strict.liftCatch #-}

liftListen : ⦃ Monad M ⦄ → Listen W M (Tuple2 A S) → Listen W (StateT S M) A
liftListen listen m = mkStateT λ s →
    listen (runStateT m s) >>= λ (mkTuple2 (mkTuple2 a s') w) →
    return $ mkTuple2 (mkTuple2 a w) s'

{-# INLINE liftListen #-}
{-# COMPILE GHC liftListen = \ mℓ m w a s AgdaMonad -> Control.Monad.Trans.State.Strict.liftListen #-}

liftPass : ⦃ Monad M ⦄ → Pass W M (Tuple2 A S) → Pass W (StateT S M) A
liftPass pass m = mkStateT λ s → pass $
    runStateT m s >>= λ (mkTuple2 (mkTuple2 a f) s') →
    return $ mkTuple2 (mkTuple2 a s') f

{-# INLINE liftPass #-}
{-# COMPILE GHC liftPass = \ mℓ m w a s AgdaMonad -> Control.Monad.Trans.State.Strict.liftPass #-}
