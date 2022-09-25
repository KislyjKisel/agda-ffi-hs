{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.Raw.DrawList where

open import Agda.Primitive                   using (Level)
open import Ffi.Hs.-base.Class               using (MonadIO)
open import Ffi.Hs.-base.Level               using (Liftℓ)
open import Ffi.Hs.-base.Unit                using (⊤; ⊤′)
open import Ffi.Hs.-dear-imgui.Types
open import Ffi.Hs.Foreign.C.String          using (CString)
open import Ffi.Hs.Foreign.C.Types           using (CInt; CBool; CFloat)
open import Ffi.Hs.Foreign.Ptr               using (Ptr)

{-# FOREIGN GHC
import qualified DearImGui.Raw.DrawList
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ


data DrawList : Set where
    mkDrawList : Ptr ImDrawList → DrawList

{-# COMPILE GHC DrawList = data DearImGui.Raw.DrawList.DrawList (DearImGui.Raw.DrawList.DrawList) #-}

postulate
    new     : ⦃ MonadIO M ⦄ → M (Liftℓ _ DrawList)
    destroy : ⦃ MonadIO M ⦄ → DrawList → M ⊤′

{-# COMPILE GHC new     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.new #-}
{-# COMPILE GHC destroy = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.destroy #-}

postulate
    addLine                 : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → ImU32 → CFloat → M ⊤′
    addRect                 : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → ImU32 → CFloat → ImDrawFlags → CFloat → M ⊤′
    addRectFilled           : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → ImU32 → CFloat → ImDrawFlags → M ⊤′
    addRectFilledMultiColor : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → ImU32 → ImU32 → ImU32 → ImU32 → M ⊤′
    addQuad                 : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → ImU32 → CFloat → M ⊤′
    addQuadFilled           : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → ImU32 → M ⊤′
    addTriangle             : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → ImU32 → CFloat → M ⊤′
    addTriangleFilled       : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → ImU32 → M ⊤′
    addCircle               : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → CFloat → ImU32 → CInt → CFloat → M ⊤′
    addCircleFilled         : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → CFloat → ImU32 → CInt → M ⊤′
    addNgon                 : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → CFloat → ImU32 → CInt → CFloat → M ⊤′
    addNgonFilled           : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → CFloat → ImU32 → CInt → M ⊤′
    addText-                : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → ImU32 → CString → CString → M ⊤′
    addText                 : ⦃ MonadIO M ⦄ → DrawList → Ptr ImFont → CFloat → Ptr ImVec2 → ImU32 → CString → CString → CFloat → Ptr ImVec4 → M ⊤′
    addPolyLine             : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → CInt → ImU32 → ImDrawFlags → CFloat → M ⊤′
    addConvexPolyFilled     : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → CInt → ImU32 → M ⊤′
    addBezierCubic          : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → ImU32 → CFloat → CInt → M ⊤′
    addBezierQuadratic      : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → ImU32 → CFloat → CInt → M ⊤′

    addImage        : ⦃ MonadIO M ⦄ → DrawList → Ptr ⊤ → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → ImU32 → M ⊤′
    addImageQuad    : ⦃ MonadIO M ⦄ → DrawList → Ptr ⊤ → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → ImU32 → M ⊤′
    addImageRounded : ⦃ MonadIO M ⦄ → DrawList → Ptr ⊤ → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → ImU32 → CFloat → ImDrawFlags → M ⊤′

    pathClear                  : ⦃ MonadIO M ⦄ → DrawList → M ⊤′
    pathLineTo                 : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → M ⊤′
    pathLineToMergeDuplicate   : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → M ⊤′
    pathFillConvex             : ⦃ MonadIO M ⦄ → DrawList → ImU32 → M ⊤′
    pathStroke                 : ⦃ MonadIO M ⦄ → DrawList → ImU32 → ImDrawFlags → CFloat → M ⊤′
    pathArcTo                  : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → CFloat → CFloat → CFloat → CInt → M ⊤′
    pathArcToFast              : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → CFloat → CInt → CInt → M ⊤′
    pathBezierCubicCurveTo     : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → CInt → M ⊤′
    pathBezierQuadraticCurveTo : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → CInt → M ⊤′
    pathRect                   : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → CFloat → ImDrawFlags → M ⊤′

    addDrawCmd  : ⦃ MonadIO M ⦄ → DrawList → M ⊤′
    cloneOutput : ⦃ MonadIO M ⦄ → DrawList → M (Liftℓ _ DrawList)

    pushClipRect           : ⦃ MonadIO M ⦄ → DrawList → Ptr ImVec2 → Ptr ImVec2 → CBool → M ⊤′
    pushClipRectFullScreen : ⦃ MonadIO M ⦄ → DrawList → M ⊤′
    popClipRect            : ⦃ MonadIO M ⦄ → DrawList → M ⊤′
    getClipRectMin         : ⦃ MonadIO M ⦄ → DrawList → M (Liftℓ _ ImVec2)
    getClipRectMax         : ⦃ MonadIO M ⦄ → DrawList → M (Liftℓ _ ImVec2)
    pushTextureID          : ⦃ MonadIO M ⦄ → DrawList → Ptr ⊤ → M ⊤′
    popTextureID           : ⦃ MonadIO M ⦄ → DrawList → M ⊤′

{-# COMPILE GHC addLine                    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addLine                    #-}
{-# COMPILE GHC addRect                    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addRect                    #-}
{-# COMPILE GHC addRectFilled              = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addRectFilled              #-}
{-# COMPILE GHC addRectFilledMultiColor    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addRectFilledMultiColor    #-}
{-# COMPILE GHC addQuad                    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addQuad                    #-}
{-# COMPILE GHC addQuadFilled              = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addQuadFilled              #-}
{-# COMPILE GHC addTriangle                = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addTriangle                #-}
{-# COMPILE GHC addTriangleFilled          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addTriangleFilled          #-}
{-# COMPILE GHC addCircle                  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addCircle                  #-}
{-# COMPILE GHC addCircleFilled            = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addCircleFilled            #-}
{-# COMPILE GHC addNgon                    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addNgon                    #-}
{-# COMPILE GHC addNgonFilled              = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addNgonFilled              #-}
{-# COMPILE GHC addText-                   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addText_                   #-}
{-# COMPILE GHC addText                    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addText                    #-}
{-# COMPILE GHC addPolyLine                = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addPolyLine                #-}
{-# COMPILE GHC addConvexPolyFilled        = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addConvexPolyFilled        #-}
{-# COMPILE GHC addBezierCubic             = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addBezierCubic             #-}
{-# COMPILE GHC addBezierQuadratic         = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addBezierQuadratic         #-}

{-# COMPILE GHC addImage                   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addImage                   #-}
{-# COMPILE GHC addImageQuad               = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addImageQuad               #-}
{-# COMPILE GHC addImageRounded            = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addImageRounded            #-}

{-# COMPILE GHC pathClear                  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathClear                  #-}
{-# COMPILE GHC pathLineTo                 = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathLineTo                 #-}
{-# COMPILE GHC pathLineToMergeDuplicate   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathLineToMergeDuplicate   #-}
{-# COMPILE GHC pathFillConvex             = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathFillConvex             #-}
{-# COMPILE GHC pathStroke                 = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathStroke                 #-}
{-# COMPILE GHC pathArcTo                  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathArcTo                  #-}
{-# COMPILE GHC pathArcToFast              = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathArcToFast              #-}
{-# COMPILE GHC pathBezierCubicCurveTo     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathBezierCubicCurveTo     #-}
{-# COMPILE GHC pathBezierQuadraticCurveTo = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathBezierQuadraticCurveTo #-}
{-# COMPILE GHC pathRect                   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pathRect                   #-}

{-# COMPILE GHC addDrawCmd                 = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.addDrawCmd                 #-}
{-# COMPILE GHC cloneOutput                = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.cloneOutput                #-}

{-# COMPILE GHC pushClipRect               = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pushClipRect               #-}
{-# COMPILE GHC pushClipRectFullScreen     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pushClipRectFullScreen     #-}
{-# COMPILE GHC popClipRect                = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.popClipRect                #-}
{-# COMPILE GHC getClipRectMin             = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.getClipRectMin             #-}
{-# COMPILE GHC getClipRectMax             = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.getClipRectMax             #-}
{-# COMPILE GHC pushTextureID              = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.pushTextureID              #-}
{-# COMPILE GHC popTextureID               = \ mℓ m AgdaMonadIO -> DearImGui.Raw.DrawList.popTextureID               #-}
