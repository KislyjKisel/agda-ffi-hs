{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.Internal.Text where

open import Agda.Builtin.IO                using (IO)
open import Agda.Builtin.Maybe             using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.Control.Monad.IO.Unlift using (MonadUnliftIO)
open import Ffi.Hs.Foreign.C.String        using (CString)

open import Ffi.Hs.Data.Text public
    using (Text; pack; unpack)

open Ffi.Hs.Foreign.C.String public
    using (withCStringLen)

private
    variable
        aℓ : Level
        A : Set aℓ
        M : Set aℓ → Set aℓ

postulate
    withCString       : ⦃ MonadUnliftIO M ⦄ → Text → (CString → M A) → M A
    withCStringOrNull : Maybe Text → (CString → IO A) → IO A
    withCStringEnd    : ⦃ MonadUnliftIO M ⦄ → Text → (CString → CString → M A) → M A
    peekCString       : CString → IO Text

{-# COMPILE GHC withCString       = DearImGui.Internal.Text.withCString       #-}
{-# COMPILE GHC withCStringOrNull = DearImGui.Internal.Text.withCStringOrNull #-}
{-# COMPILE GHC withCStringEnd    = DearImGui.Internal.Text.withCStringEnd    #-}
{-# COMPILE GHC peekCString       = DearImGui.Internal.Text.peekCString       #-}
