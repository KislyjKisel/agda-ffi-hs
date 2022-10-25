{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Trans.Reader where

open import Agda.Primitive using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.Monad.Trans.Class using (MonadTrans)
open import Ffi.Hs.Data.Function             using (const; _$_; _∘_)
open import Ffi.Hs.Control.Monad.Signatures  using (CallCC; Catch)
open import Ffi.Hs.Control.Monad             using (return)
open import Ffi.Hs.Data.Functor.Identity     using (Identity; runIdentity; mkIdentity)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Monad.Trans.Reader
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.Monad.Trans.Class (AgdaMonadTrans(AgdaMonadTrans))
#-}

private
    variable
        ℓ : Level
        A B E R R′ : Set ℓ
        M N : Set ℓ → Set ℓ

record ReaderT (R : Set ℓ) (M : Set ℓ → Set ℓ) (A : Set ℓ) : Set ℓ where
    constructor mkReaderT
    field
        runReaderT : R → M A

open ReaderT public

{-# FOREIGN GHC type AgdaReaderT ℓ = Control.Monad.Trans.Reader.ReaderT #-}
{-# COMPILE GHC ReaderT = data(1) AgdaReaderT (Control.Monad.Trans.Reader.ReaderT) #-}

postulate
    MonadTrans[ReaderT[R]]      : MonadTrans (ReaderT R)
    Monad[ReaderT[R,M]]         : ⦃ Monad M ⦄ → Monad (ReaderT R M)
    Functor[ReaderT[R,M]]       : ⦃ Functor M ⦄ → Functor (ReaderT R M)
    MonadFix[ReaderT[R,M]]      : ⦃ MonadFix M ⦄ → MonadFix (ReaderT R M)
    MonadFail[ReaderT[R,M]]     : ⦃ MonadFail M ⦄ → MonadFail (ReaderT R M)
    Applicative[ReaderT[R,M]]   : ⦃ Monad M ⦄ → Applicative (ReaderT R M)
    Contravariant[ReaderT[R,M]] : ⦃ Contravariant M ⦄ → Contravariant (ReaderT R M)
    MonadIO[ReaderT[R,M]]       : ⦃ MonadIO M ⦄ → MonadIO (ReaderT R M)
    Alternative[ReaderT[R,M]]   : ⦃ MonadPlus M ⦄ → Alternative (ReaderT R M)
    MonadPlus[ReaderT[R,M]]     : ⦃ MonadPlus M ⦄ → MonadPlus (ReaderT R M)

{-# COMPILE GHC MonadTrans[ReaderT[R]]      = \ rℓ r                     -> AgdaMonadTrans    #-}
{-# COMPILE GHC Monad[ReaderT[R,M]]         = \ mℓ m r AgdaMonad         -> AgdaMonad         #-}
{-# COMPILE GHC Functor[ReaderT[R,M]]       = \ mℓ m r AgdaFunctor       -> AgdaFunctor       #-}
{-# COMPILE GHC MonadFix[ReaderT[R,M]]      = \ mℓ m r AgdaMonadFix      -> AgdaMonadFix      #-}
{-# COMPILE GHC MonadFail[ReaderT[R,M]]     = \ mℓ m r AgdaMonadFail     -> AgdaMonadFail     #-}
{-# COMPILE GHC Applicative[ReaderT[R,M]]   = \ mℓ m r AgdaMonad         -> AgdaApplicative   #-}
{-# COMPILE GHC Contravariant[ReaderT[R,M]] = \ mℓ m r AgdaContravariant -> AgdaContravariant #-}
{-# COMPILE GHC MonadIO[ReaderT[R,M]]       = \ mℓ m r AgdaMonadIO       -> AgdaMonadIO       #-}
{-# COMPILE GHC Alternative[ReaderT[R,M]]   = \ mℓ m r AgdaMonadPlus     -> AgdaAlternative   #-}
{-# COMPILE GHC MonadPlus[ReaderT[R,M]]     = \ mℓ m r AgdaMonadPlus     -> AgdaMonadPlus     #-}


mapReaderT : (M A → N B) → ReaderT R M A → ReaderT R N B
mapReaderT f m = mkReaderT $ f ∘ runReaderT m
{-# COMPILE GHC mapReaderT = \ ℓ m a n b r -> Control.Monad.Trans.Reader.runReaderT #-}

withReaderT : (R′ → R) → ReaderT R M A → ReaderT R′ M A
withReaderT f m = mkReaderT $ runReaderT m ∘ f
{-# COMPILE GHC withReaderT = \ ℓ r' r m a -> Control.Monad.Trans.Reader.withReaderT #-}


Reader : Set ℓ → Set ℓ → Set ℓ
Reader R = ReaderT R Identity

reader : ⦃ Monad M ⦄ → (R → A) → ReaderT R M A
reader f = mkReaderT $ return ∘ f
{-# COMPILE GHC reader = \ ℓ m r a -> Control.Monad.Trans.Reader.reader #-}

runReader : Reader R A → R → A
runReader m = runIdentity ∘ runReaderT m
{-# COMPILE GHC runReader = \ ℓ r a -> Control.Monad.Trans.Reader.runReader #-}

mapReader : (A → B) → Reader R A → Reader R B
mapReader f = mapReaderT $ mkIdentity ∘ f ∘ runIdentity
{-# COMPILE GHC mapReader = \ ℓ a b r -> Control.Monad.Trans.Reader.mapReader #-}

withReader : (R′ → R) → Reader R A → Reader R′ A
withReader = withReaderT
{-# COMPILE GHC withReader = \ ℓ r' r a -> Control.Monad.Trans.Reader.withReader #-}


ask : ⦃ Monad M ⦄ → ReaderT R M R
ask = mkReaderT return
{-# COMPILE GHC ask = \ ℓ m r AgdaMonad -> Control.Monad.Trans.Reader.ask #-}

local : (R → R) → ReaderT R M A → ReaderT R M A
local = withReaderT
{-# COMPILE GHC local = \ ℓ r m a -> Control.Monad.Trans.Reader.local #-}

asks : ⦃ Monad M ⦄ → (R → A) → ReaderT R M A
asks f = mkReaderT $ return ∘ f
{-# COMPILE GHC asks = \ ℓ m r a AgdaMonad -> Control.Monad.Trans.Reader.asks #-}


liftCallCC : CallCC M A B → CallCC (ReaderT R M) A B
liftCallCC callCC f = mkReaderT λ r →
    callCC λ c → runReaderT (f $ mkReaderT ∘ const ∘ c) r
{-# COMPILE GHC liftCallCC = \ ℓ m a b r -> Control.Monad.Trans.Reader.liftCallCC #-}

liftCatch : Catch E M A → Catch E (ReaderT R M) A
liftCatch f m h = mkReaderT λ r →
    f (runReaderT m r) λ e → runReaderT (h e) r
{-# COMPILE GHC liftCatch = \ ℓ e m a r -> Control.Monad.Trans.Reader.liftCatch #-}
