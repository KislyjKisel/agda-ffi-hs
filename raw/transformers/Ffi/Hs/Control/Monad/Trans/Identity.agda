{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Trans.Identity where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.Monad.Signatures  using (Catch; CallCC)
open import Ffi.Hs.Control.Monad.Trans.Class using (MonadTrans)
open import Ffi.Hs.Data.Function             using (_$_; _∘_)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Monad.Trans.Identity
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.Monad.Trans.Class (AgdaMonadTrans(AgdaMonadTrans))
#-}

private
    variable
        aℓ : Level
        A B E : Set aℓ
        M : Set aℓ → Set aℓ


record IdentityT (F : Set aℓ → Set aℓ) (A : Set aℓ) : Set aℓ where
    constructor mkIdentityT
    field
        runIdentityT : F A

{-# FOREIGN GHC type AgdaIdentityT aℓ = Control.Monad.Trans.Identity.IdentityT #-}
{-# COMPILE GHC IdentityT = data(1) AgdaIdentityT (Control.Monad.Trans.Identity.IdentityT) #-}

open IdentityT public

postulate
    MonadTrans[IdentityT]       : MonadTrans {aℓ} IdentityT
    Monad[IdentityT[M]]         : ⦃ Monad M ⦄ → Monad (IdentityT M)
    Functor[IdentityT[M]]       : ⦃ Functor M ⦄ → Functor (IdentityT M)
    MonadFix[IdentityT[M]]      : ⦃ MonadFix M ⦄ → MonadFix (IdentityT M)
    MonadFail[IdentityT[M]]     : ⦃ MonadFail M ⦄ → MonadFail (IdentityT M)
    Applicative[IdentityT[M]]   : ⦃ Applicative M ⦄ → Applicative (IdentityT M)
    Foldable[IdentityT[M]]      : ⦃ Foldable M ⦄ → Foldable (IdentityT M)
    Traversable[IdentityT[M]]   : ⦃ Traversable M ⦄ → Traversable (IdentityT M)
    Contravariant[IdentityT[M]] : ⦃ Contravariant M ⦄ → Contravariant (IdentityT M)
    Eq1[IdentityT[M]]           : ⦃ Eq1 M ⦄ → Eq1 (IdentityT M)
    Ord1[IdentityT[M]]          : ⦃ Ord1 M ⦄ → Ord1 (IdentityT M)
    Read1[IdentityT[M]]         : ⦃ Read1 M ⦄ → Read1 (IdentityT M)
    Show1[IdentityT[M]]         : ⦃ Show1 M ⦄ → Show1 (IdentityT M)
    MonadZip[IdentityT[M]]      : ⦃ MonadZip M ⦄ → MonadZip (IdentityT M)
    MonadIO[IdentityT[M]]       : ⦃ MonadIO M ⦄ → MonadIO (IdentityT M)
    Alternative[IdentityT[M]]   : ⦃ Alternative M ⦄ → Alternative (IdentityT M)
    MonadPlus[IdentityT[M]]     : ⦃ MonadPlus M ⦄ → MonadPlus (IdentityT M)
    Eq[IdentityT[M,A]]          : ⦃ Eq1 M ⦄ → ⦃ Eq A ⦄ → Eq (IdentityT M A)
    Ord[IdentityT[M,A]]         : ⦃ Ord1 M ⦄ → ⦃ Ord A ⦄ → Ord (IdentityT M A)
    Read[IdentityT[M,A]]        : ⦃ Read1 M ⦄ → ⦃ Read A ⦄ → Read (IdentityT M A)
    Show[IdentityT[M,A]]        : ⦃ Show1 M ⦄ → ⦃ Show A ⦄ → Show (IdentityT M A)

{-# COMPILE GHC MonadTrans[IdentityT]       = \ aℓ                        -> AgdaMonadTrans    #-}
{-# COMPILE GHC Monad[IdentityT[M]]         = \ mℓ m AgdaMonad            -> AgdaMonad         #-}
{-# COMPILE GHC Functor[IdentityT[M]]       = \ mℓ m AgdaFunctor          -> AgdaFunctor       #-}
{-# COMPILE GHC MonadFix[IdentityT[M]]      = \ mℓ m AgdaMonadFix         -> AgdaMonadFix      #-}
{-# COMPILE GHC MonadFail[IdentityT[M]]     = \ mℓ m AgdaMonadFail        -> AgdaMonadFail     #-}
{-# COMPILE GHC Applicative[IdentityT[M]]   = \ mℓ m AgdaApplicative      -> AgdaApplicative   #-}
{-# COMPILE GHC Foldable[IdentityT[M]]      = \ mℓ m AgdaFoldable         -> AgdaFoldable      #-}
{-# COMPILE GHC Traversable[IdentityT[M]]   = \ mℓ m AgdaTraversable      -> AgdaTraversable   #-}
{-# COMPILE GHC Contravariant[IdentityT[M]] = \ mℓ m AgdaContravariant    -> AgdaContravariant #-}
{-# COMPILE GHC Eq1[IdentityT[M]]           = \ mℓ m AgdaEq1              -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[IdentityT[M]]          = \ mℓ m AgdaOrd1             -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[IdentityT[M]]         = \ mℓ m AgdaRead1            -> AgdaRead1         #-}
{-# COMPILE GHC Show1[IdentityT[M]]         = \ mℓ m AgdaShow1            -> AgdaShow1         #-}
{-# COMPILE GHC MonadZip[IdentityT[M]]      = \ mℓ m AgdaMonadZip         -> AgdaMonadZip      #-}
{-# COMPILE GHC MonadIO[IdentityT[M]]       = \ mℓ m AgdaMonadIO          -> AgdaMonadIO       #-}
{-# COMPILE GHC Alternative[IdentityT[M]]   = \ mℓ m AgdaAlternative      -> AgdaAlternative   #-}
{-# COMPILE GHC MonadPlus[IdentityT[M]]     = \ mℓ m AgdaMonadPlus        -> AgdaMonadPlus     #-}
{-# COMPILE GHC Eq[IdentityT[M,A]]          = \ mℓ m a AgdaEq1 AgdaEq     -> AgdaEq            #-}
{-# COMPILE GHC Ord[IdentityT[M,A]]         = \ mℓ m a AgdaOrd1 AgdaOrd   -> AgdaOrd           #-}
{-# COMPILE GHC Read[IdentityT[M,A]]        = \ mℓ m a AgdaRead1 AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Show[IdentityT[M,A]]        = \ mℓ m a AgdaShow1 AgdaShow -> AgdaShow          #-}


mapIdentityT : ∀{mℓ nℓ} {M : Set mℓ → Set mℓ} {N : Set nℓ → Set nℓ} {A B} →
               (M A → N B) → IdentityT M A → IdentityT N B
mapIdentityT f (mkIdentityT x) = mkIdentityT (f x)

liftCatch : Catch E M A → Catch E (IdentityT M) A
liftCatch f m h = mkIdentityT $ f (runIdentityT m) (runIdentityT ∘ h)

liftCallCC : CallCC M A B → CallCC (IdentityT M) A B
liftCallCC callCC f = mkIdentityT $
    callCC (λ c → runIdentityT (f (mkIdentityT ∘ c)))
