{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.Raw.IO where

open import Agda.Primitive          using (Level)
open import Ffi.Hs.-base.Class      using (MonadIO)
open import Ffi.Hs.-base.Unit       using (⊤; ⊤′)
open import Ffi.Hs.Foreign.C.String using (CString)
open import Ffi.Hs.Foreign.C.Types  using (CFloat)
open import Ffi.Hs.Foreign.Ptr      using (Ptr)

{-# FOREIGN GHC
import qualified DearImGui.Raw.IO
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ


postulate
    setIniFilename             : ⦃ MonadIO M ⦄ → CString → M ⊤′
    setLogFilename             : ⦃ MonadIO M ⦄ → CString → M ⊤′
    setMouseDoubleClickMaxDist : ⦃ MonadIO M ⦄ → CFloat → M ⊤′
    setMouseDoubleClickTime    : ⦃ MonadIO M ⦄ → CFloat → M ⊤′
    setMouseDragThreshold      : ⦃ MonadIO M ⦄ → CFloat → M ⊤′
    setKeyRepeatDelay          : ⦃ MonadIO M ⦄ → CFloat → M ⊤′
    setKeyRepeatRate           : ⦃ MonadIO M ⦄ → CFloat → M ⊤′
    setUserData                : ⦃ MonadIO M ⦄ → Ptr ⊤ → M ⊤′

{-# COMPILE GHC setIniFilename             = \ mℓ m AgdaMonadIO -> DearImGui.Raw.IO.setIniFilename             #-}
{-# COMPILE GHC setLogFilename             = \ mℓ m AgdaMonadIO -> DearImGui.Raw.IO.setLogFilename             #-}
{-# COMPILE GHC setMouseDoubleClickMaxDist = \ mℓ m AgdaMonadIO -> DearImGui.Raw.IO.setMouseDoubleClickMaxDist #-}
{-# COMPILE GHC setMouseDoubleClickTime    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.IO.setMouseDoubleClickTime    #-}
{-# COMPILE GHC setMouseDragThreshold      = \ mℓ m AgdaMonadIO -> DearImGui.Raw.IO.setMouseDragThreshold      #-}
{-# COMPILE GHC setKeyRepeatDelay          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.IO.setKeyRepeatDelay          #-}
{-# COMPILE GHC setKeyRepeatRate           = \ mℓ m AgdaMonadIO -> DearImGui.Raw.IO.setKeyRepeatRate           #-}
{-# COMPILE GHC setUserData                = \ mℓ m AgdaMonadIO -> DearImGui.Raw.IO.setUserData                #-}
