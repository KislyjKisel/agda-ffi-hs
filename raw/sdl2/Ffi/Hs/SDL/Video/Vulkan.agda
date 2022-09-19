{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Video.Vulkan where

open import Agda.Builtin.Char         using (Char)
open import Agda.Builtin.List         using (List)
open import Agda.Builtin.Maybe        using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class        using (MonadIO)
open import Ffi.Hs.-base.Level        using (Liftℓ)
open import Ffi.Hs.-base.Unit         using (⊤)
open import Ffi.Hs.Foreign.C.String   using (CString)
open import Ffi.Hs.Foreign.C.Types    using (CInt)
open import Ffi.Hs.SDL.Internal.Types using (Window)
open import Ffi.Hs.SDL.Vect           using (V2)

open import Ffi.Hs.SDL.Raw.Types public
    using
    ( VkInstance
    ; VkSurfaceKHR
    ; VkGetInstanceProcAddrFunc
    )

{-# FOREIGN GHC
import qualified SDL.Video.Vulkan
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

private
    variable
        aℓ : Level
        M : Set aℓ → Set aℓ


postulate
    vkLoadLibrary              : ⦃ MonadIO M ⦄ → Maybe (List Char) → M ⊤
    vkUnloadLibrary            : ⦃ MonadIO M ⦄ → M ⊤
    vkGetVkGetInstanceProcAddr : ⦃ MonadIO M ⦄ → M (Liftℓ _ VkGetInstanceProcAddrFunc)
    vkGetInstanceExtensions    : ⦃ MonadIO M ⦄ → Window → M (Liftℓ _ (List CString))
    vkCreateSurface            : ⦃ MonadIO M ⦄ → Window → VkInstance → M (Liftℓ _ VkSurfaceKHR)
    vkGetDrawableSize          : ⦃ MonadIO M ⦄ → Window → M (Liftℓ _ (V2 CInt))

{-# COMPILE GHC vkLoadLibrary              = \ mℓ m AgdaMonadIO -> SDL.Video.Vulkan.vkLoadLibrary              #-}
{-# COMPILE GHC vkUnloadLibrary            = \ mℓ m AgdaMonadIO -> SDL.Video.Vulkan.vkUnloadLibrary            #-}
{-# COMPILE GHC vkGetVkGetInstanceProcAddr = \ mℓ m AgdaMonadIO -> SDL.Video.Vulkan.vkGetVkGetInstanceProcAddr #-}
{-# COMPILE GHC vkGetInstanceExtensions    = \ mℓ m AgdaMonadIO -> SDL.Video.Vulkan.vkGetInstanceExtensions    #-}
{-# COMPILE GHC vkCreateSurface            = \ mℓ m AgdaMonadIO -> SDL.Video.Vulkan.vkCreateSurface            #-}
{-# COMPILE GHC vkGetDrawableSize          = \ mℓ m AgdaMonadIO -> SDL.Video.Vulkan.vkGetDrawableSize          #-}
