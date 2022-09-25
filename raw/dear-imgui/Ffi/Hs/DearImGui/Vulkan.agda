{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.Vulkan where

-- todo: (req Vulkan bindings, preprocessor)

record InitInfo : Set where
    constructor mkInitInfo
    field
        instance'      : Instance
        physicalDevice : PhysicalDevice
        device         : Device
        queueFamily    : Word32
        queue          : Queue
        pipelineCache  : PipelineCache
        descriptorPool : DescriptorPool
        subpass        : Word32
        minImageCount  : Word32
        imageCount     : Word32
        msaaSamples    : SampleCountFlagBits
        mbAllocator    : Maybe AllocationCallbacks
        checkResult    : Result → IO ⊤

{-# COMPILE GHC InitInfo = data DearImGui.Vulkan.InitInfo (DearImGui.Vulkan.InitInfo) #-}

postulate
    withVulkan                     : ⦃ MonadUnliftIO M ⦄ → InitInfo → RenderPass → (Bool → M A) → M A
    vulkanInit                     : ⦃ MonadIO M ⦄ → InitInfo → RenderPass → M (Liftℓ _ (Tuple2 (FunPtr (Result → IO ⊤)) Bool))
    vulkanShutdown                 : ⦃ MonadIO M ⦄ → Tuple2 (FunPtr A) B → M ⊤′
    vulkanNewFrame                 : ⦃ MonadIO M ⦄ → M ⊤′
    vulkanRenderDrawData           : ⦃ MonadIO M ⦄ → DrawData → CommandBuffer → Maybe Pipeline → M ⊤′
    vulkanCreateFontsTexture       : ⦃ MonadIO M ⦄ → CommandBuffer → M (Liftℓ _ Bool)
    vulkanDestroyFontUploadObjects : ⦃ MonadIO M ⦄ → M ⊤′
    vulkanSetMinImageCount         : ⦃ MonadIO M ⦄ → Word32 → M ⊤′
    vulkanAddTexture               : ⦃ MonadIO M ⦄ → Sampler → ImageView → ImageLayout → M (Liftℓ _ DescriptorSet)

{-# COMPILE GHC withVulkan                     = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.Vulkan.withVulkan                     #-}
{-# COMPILE GHC vulkanInit                     = \ mℓ m AgdaMonadIO         -> DearImGui.Vulkan.vulkanInit                     #-}
{-# COMPILE GHC vulkanShutdown                 = \ mℓ m AgdaMonadIO         -> DearImGui.Vulkan.vulkanShutdown                 #-}
{-# COMPILE GHC vulkanNewFrame                 = \ mℓ m AgdaMonadIO         -> DearImGui.Vulkan.vulkanNewFrame                 #-}
{-# COMPILE GHC vulkanRenderDrawData           = \ mℓ m AgdaMonadIO         -> DearImGui.Vulkan.vulkanRenderDrawData           #-}
{-# COMPILE GHC vulkanCreateFontsTexture       = \ mℓ m AgdaMonadIO         -> DearImGui.Vulkan.vulkanCreateFontsTexture       #-}
{-# COMPILE GHC vulkanDestroyFontUploadObjects = \ mℓ m AgdaMonadIO         -> DearImGui.Vulkan.vulkanDestroyFontUploadObjects #-}
{-# COMPILE GHC vulkanSetMinImageCount         = \ mℓ m AgdaMonadIO         -> DearImGui.Vulkan.vulkanSetMinImageCount         #-}
{-# COMPILE GHC vulkanAddTexture               = \ mℓ m AgdaMonadIO         -> DearImGui.Vulkan.vulkanAddTexture               #-}
