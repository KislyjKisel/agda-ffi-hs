{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad where

open import Agda.Builtin.Bool using (Bool)
open import Agda.Primitive
open import Ffi.Hs.Data.String using (String)
open import Ffi.Hs.-base.Class using (Applicative; Alternative)

open Ffi.Hs.-base.Class public
    using (Monad; MonadPlus; MonadFail)

open import Ffi.Hs.Control.Monad.Fail public
    using (fail)

{-# FOREIGN GHC
import qualified Control.Monad
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ cℓ dℓ eℓ fℓ mℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        D : Set dℓ
        E : Set eℓ
        F : Set fℓ
        M : Set mℓ → Set mℓ

infixl 4 _<$!>_
infixl 1 _>>=_ _>>_
infixr 1 _=<<_ _>=>_ _<=<_

postulate
    return : ⦃ Monad M ⦄ → A → M A
    _>>=_  : ⦃ Monad M ⦄ → M A → (A → M B) → M B
    _>>_   : ⦃ Monad M ⦄ → M A → M B → M B

    mzero : ⦃ MonadPlus M ⦄ → M A
    mplus : ⦃ MonadPlus M ⦄ → M A → M A → M A

    _=<<_ : ⦃ Monad M ⦄ → (A → M B) → M A → M B
    _>=>_ : {A B C : Set mℓ} → ⦃ Monad M ⦄ → (A → M B) → (B → M C) → A → M C
    _<=<_ : {A B C : Set mℓ} → ⦃ Monad M ⦄ → (B → M C) → (A → M B) → A → M C

    forever : ⦃ Applicative M ⦄ → M A → M B

    join : ⦃ Monad M ⦄ → M (M A) → M A
    mfilter : ⦃ MonadPlus M ⦄ → (A → Bool) → M A → M A

    liftM  : ⦃ Monad M ⦄ → (A → B) → M A → M B
    liftM2 : ⦃ Monad M ⦄ → (A → B → C) → M A → M B → M C
    liftM3 : ⦃ Monad M ⦄ → (A → B → C → D) → M A → M B → M C → M D
    liftM4 : ⦃ Monad M ⦄ → (A → B → C → D → E) → M A → M B → M C → M D → M E
    liftM5 : ⦃ Monad M ⦄ → (A → B → C → D → E → F) → M A → M B → M C → M D → M E → M F
    ap     : ⦃ Monad M ⦄ → M (A → B) → M A → M B

    _<$!>_ : ⦃ Monad M ⦄ → (A → B) → M A → M B

{-# COMPILE GHC return = \ mℓ m a   AgdaMonad -> Control.Monad.return #-}
{-# COMPILE GHC _>>=_  = \ mℓ m a b AgdaMonad -> (Control.Monad.>>=)  #-}
{-# COMPILE GHC _>>_   = \ mℓ m a b AgdaMonad -> (Control.Monad.>>)   #-}

{-# COMPILE GHC mzero = \ mℓ m a AgdaMonadPlus -> Control.Monad.mzero #-}
{-# COMPILE GHC mplus = \ mℓ m a AgdaMonadPlus -> Control.Monad.mplus #-}

{-# COMPILE GHC _=<<_ = \ mℓ m a b   AgdaMonad -> (Control.Monad.=<<) #-}
{-# COMPILE GHC _>=>_ = \ mℓ a b c m AgdaMonad -> (Control.Monad.>=>) #-}
{-# COMPILE GHC _<=<_ = \ mℓ a b c m AgdaMonad -> (Control.Monad.<=<) #-}

{-# COMPILE GHC forever = \ mℓ m a b AgdaApplicative -> Control.Monad.forever #-}

{-# COMPILE GHC join    = \ mℓ m a AgdaMonad     -> Control.Monad.join    #-}
{-# COMPILE GHC mfilter = \ mℓ m a AgdaMonadPlus -> Control.Monad.mfilter #-}

{-# COMPILE GHC liftM  = \ mℓ m a b         AgdaMonad -> Control.Monad.liftM  #-}
{-# COMPILE GHC liftM2 = \ mℓ m a b c       AgdaMonad -> Control.Monad.liftM2 #-}
{-# COMPILE GHC liftM3 = \ mℓ m a b c d     AgdaMonad -> Control.Monad.liftM3 #-}
{-# COMPILE GHC liftM4 = \ mℓ m a b c d e   AgdaMonad -> Control.Monad.liftM4 #-}
{-# COMPILE GHC liftM5 = \ mℓ m a b c d e f AgdaMonad -> Control.Monad.liftM5 #-}
{-# COMPILE GHC ap     = \ mℓ m a b         AgdaMonad -> Control.Monad.ap     #-}

{-# COMPILE GHC _<$!>_ = \ mℓ m a b AgdaMonad -> (Control.Monad.<$!>) #-}

postulate
    Monad[M]⇒Applicative[M]     : ⦃ Monad M ⦄ → Applicative M
    MonadPlus[M]⇒Monad[M]       : ⦃ MonadPlus M ⦄ → Monad M
    MonadPlus[M]⇒Alternative[M] : ⦃ MonadPlus M ⦄ → Alternative M

{-# COMPILE GHC Monad[M]⇒Applicative[M]     = \ mℓ m AgdaMonad     -> AgdaApplicative #-}
{-# COMPILE GHC MonadPlus[M]⇒Monad[M]       = \ mℓ m AgdaMonadPlus -> AgdaMonad       #-}
{-# COMPILE GHC MonadPlus[M]⇒Alternative[M] = \ mℓ m AgdaMonadPlus -> AgdaAlternative #-}
