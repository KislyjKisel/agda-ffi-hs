{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.IO.Unlift where

open import Agda.Builtin.IO using (IO)
open import Agda.Primitive

open import Ffi.Hs.Control.Monad.IO.Class public
    using
    ( MonadIO
    ; liftIO
    )

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Monad.IO.Unlift
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ
        M N : Set aℓ → Set aℓ

postulate
    MonadUnliftIO : (Set aℓ → Set aℓ) → Set aℓ
    MonadUnliftIO[M]⇒MonadIO[M] : ⦃ MonadUnliftIO M ⦄ → MonadIO M

    withRunInIO : ⦃ MonadUnliftIO M ⦄ → ((∀{A} → M A → IO A) → IO B) → M B

    MonadUnliftIO[IO] : MonadUnliftIO {aℓ} IO
    -- todo: transformers' instances

{-# FOREIGN GHC data AgdaMonadUnliftIO mℓ m = Control.Monad.IO.Unlift.MonadUnliftIO m => AgdaMonadUnliftIO #-}
{-# COMPILE GHC MonadUnliftIO = type(0) AgdaMonadUnliftIO #-}

{-# COMPILE GHC MonadUnliftIO[M]⇒MonadIO[M] = \ mℓ m AgdaMonadUnliftIO -> AgdaMonadIO #-}

{-# COMPILE GHC withRunInIO = \ mℓ m AgdaMonadUnliftIO f -> Control.Monad.IO.Unlift.withRunInIO (\ g -> f (\ _ -> g)) #-}

{-# COMPILE GHC MonadUnliftIO[IO] = \ aℓ -> AgdaMonadUnliftIO #-}

record UnliftIO (M : Set aℓ → Set aℓ) : Set (lsuc aℓ) where
    constructor mkUnliftIO
    field
        unliftIO : ∀{A} → M A → IO A

{-# FOREIGN GHC type AgdaUnliftIO mℓ = Control.Monad.IO.Unlift.UnliftIO #-}
{-# COMPILE GHC UnliftIO = data(1) AgdaUnliftIO (Control.Monad.IO.Unlift.UnliftIO) #-}

postulate
    askUnliftIO        : {M : ∀{aℓ} → Set aℓ → Set aℓ} → ⦃ MonadUnliftIO (M {aℓ}) ⦄ → M (UnliftIO (M {aℓ}))
    askRunInIO         : ⦃ MonadUnliftIO M ⦄ → M (M A → IO A)
    withUnliftIO       : ⦃ MonadUnliftIO M ⦄ → (UnliftIO M → IO A) → M A
    toIO               : ⦃ MonadUnliftIO M ⦄ → M A → M (IO A)
    wrappedWithRunInIO : ⦃ MonadUnliftIO N ⦄ → (N B → M B) → (∀{A} → M A → N A) → ((∀{A} → M A → IO A) → IO B) → M B

{-# COMPILE GHC askUnliftIO  = \ aℓ m   AgdaMonadUnliftIO -> Control.Monad.IO.UnliftaskUnliftIO  #-}
{-# COMPILE GHC askRunInIO   = \ mℓ m a AgdaMonadUnliftIO -> Control.Monad.IO.UnliftaskRunInIO   #-}
{-# COMPILE GHC withUnliftIO = \ mℓ m a AgdaMonadUnliftIO -> Control.Monad.IO.UnliftwithUnliftIO #-}
{-# COMPILE GHC toIO         = \ mℓ m a AgdaMonadUnliftIO -> Control.Monad.IO.UnlifttoIO         #-}

{-# COMPILE GHC wrappedWithRunInIO =
    \ nℓ n b m AgdaMonadUnliftIO f g h -> Control.Monad.IO.UnliftwrappedWithRunInIO f (g ()) (\ i -> h (\ _ -> i)) #-}
