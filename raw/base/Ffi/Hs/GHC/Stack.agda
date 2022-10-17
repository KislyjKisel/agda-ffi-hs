{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Stack where

open import Agda.Builtin.Char       using (Char)
open import Agda.Builtin.IO         using (IO)
open import Agda.Builtin.List       using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class      using (Eq; Show)
open import Ffi.Hs.Data.Int         using (Int)
open import Ffi.Hs.Data.Tuple       using (Tuple2)
open import Ffi.Hs.Foreign.C.String using (CString)
open import Ffi.Hs.Foreign.Ptr      using (Ptr)
open import Ffi.Hs.GHC.IsList       using (IsList)

{-# FOREIGN GHC
import qualified GHC.Stack
import MAlonzo.Code.Ffi.Hs.GHC.IsList (AgdaIsList(AgdaIsList))
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

record SrcLoc : Set where
    constructor mkSrcLoc
    field
        srcLocPackage   : List Char
        srcLocModule    : List Char
        srcLocFile      : List Char
        srcLocStartLine : Int
        srcLocStartCol  : Int
        srcLocEndLine   : Int
        srcLocEndCol    : Int

{-# COMPILE GHC SrcLoc = data GHC.Stack.SrcLoc (GHC.Stack.SrcLoc) #-}

postulate
    Show[SrcLoc] : Show SrcLoc
    Eq[SrcLoc]   : Eq SrcLoc

    prettySrcLoc : SrcLoc → List Char

{-# COMPILE GHC Show[SrcLoc] = AgdaShow #-}
{-# COMPILE GHC Eq[SrcLoc]   = AgdaEq   #-}

{-# COMPILE GHC prettySrcLoc = GHC.Stack.prettySrcLoc #-}

postulate
    currentCallStack : IO (List (List Char))
    whoCreated       : A → IO (List (List Char))

{-# COMPILE GHC currentCallStack =           GHC.Stack.currentCallStack #-}
{-# COMPILE GHC whoCreated       = \ aℓ a -> GHC.Stack.whoCreated       #-}


data HasCallStack : Set where
    mkHasCallStack : HasCallStack

{-# FOREIGN GHC data AgdaHasCallStack = GHC.Stack.HasCallStack => AgdaHasCallStack #-}
{-# COMPILE GHC HasCallStack = data AgdaHasCallStack (AgdaHasCallStack) #-}


postulate
    CallStack : Set

    IsList[CallStack] : IsList CallStack
    Show[CallStack]   : Show CallStack

    callStack           : ⦃ HasCallStack ⦄ → CallStack
    emptyCallStack      : CallStack
    freezeCallStack     : CallStack → CallStack
    fromCallSiteList    : List (Tuple2 (List Char) SrcLoc) → CallStack
    getCallStack        : CallStack → List (Tuple2 (List Char) SrcLoc)
    popCallStack        : CallStack → CallStack
    prettyCallStack     : CallStack → List Char
    pushCallStack       : Tuple2 (List Char) SrcLoc → CallStack → CallStack
    withFrozenCallStack : ⦃ HasCallStack ⦄ → (⦃ HasCallStack ⦄ → A) → A

{-# COMPILE GHC CallStack = type GHC.Stack.CallStack #-}

{-# COMPILE GHC IsList[CallStack] = AgdaIsList #-}
{-# COMPILE GHC Show[CallStack]   = AgdaShow   #-}

{-# COMPILE GHC callStack = \ AgdaHasCallStack -> GHC.Stack.callStack #-}
{-# COMPILE GHC emptyCallStack      = GHC.Stack.emptyCallStack   #-}
{-# COMPILE GHC freezeCallStack     = GHC.Stack.freezeCallStack  #-}
{-# COMPILE GHC fromCallSiteList    = GHC.Stack.fromCallSiteList #-}
{-# COMPILE GHC getCallStack        = GHC.Stack.getCallStack     #-}
{-# COMPILE GHC popCallStack        = GHC.Stack.popCallStack     #-}
{-# COMPILE GHC prettyCallStack     = GHC.Stack.prettyCallStack  #-}
{-# COMPILE GHC pushCallStack       = GHC.Stack.pushCallStack    #-}
{-# COMPILE GHC withFrozenCallStack =
    \ aℓ a AgdaHasCallStack f -> GHC.Stack.withFrozenCallStack (f AgdaHasCallStack) #-}

postulate
    CostCentreStack : Set
    CostCentre      : Set

    getCurrentCCS : A → IO (Ptr CostCentreStack)
    getCCSOf      : A → IO (Ptr CostCentreStack)
    clearCCS      : IO A → IO A
    ccsCC         : Ptr CostCentreStack → IO (Ptr CostCentre)
    ccsParent     : Ptr CostCentreStack → IO (Ptr CostCentreStack)
    ccLabel       : Ptr CostCentre → IO CString
    ccModule      : Ptr CostCentre → IO CString
    ccSrcSpan     : Ptr CostCentre → IO CString
    ccsToStrings  : Ptr CostCentreStack → IO (List (List Char))
    renderStack   : List (List Char) → List Char

{-# COMPILE GHC CostCentreStack = type GHC.Stack.CostCentreStack #-}
{-# COMPILE GHC CostCentre      = type GHC.Stack.CostCentre      #-}

{-# COMPILE GHC getCurrentCCS = \ aℓ a -> GHC.Stack.getCurrentCCS #-}
{-# COMPILE GHC getCCSOf      = \ aℓ a -> GHC.Stack.getCCSOf      #-}
{-# COMPILE GHC clearCCS      = \ aℓ a -> GHC.Stack.clearCCS      #-}
{-# COMPILE GHC ccsCC         =           GHC.Stack.ccsCC         #-}
{-# COMPILE GHC ccsParent     =           GHC.Stack.ccsParent     #-}
{-# COMPILE GHC ccLabel       =           GHC.Stack.ccLabel       #-}
{-# COMPILE GHC ccModule      =           GHC.Stack.ccModule      #-}
{-# COMPILE GHC ccSrcSpan     =           GHC.Stack.ccSrcSpan     #-}
{-# COMPILE GHC ccsToStrings  =           GHC.Stack.ccsToStrings  #-}
{-# COMPILE GHC renderStack   =           GHC.Stack.renderStack   #-}
