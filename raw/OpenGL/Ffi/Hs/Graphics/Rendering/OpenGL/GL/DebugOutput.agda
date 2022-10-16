{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput where

open import Agda.Builtin.Char        using (Char)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Builtin.Unit        using (⊤)
open import Agda.Primitive           using (Level)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar; SettableStateVar)
open import Ffi.Hs.Graphics.GL.Types using (GLuint; GLsizei)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays using (Capability)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.DebugOutput
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


data DebugSource : Set where
    DebugSourceAPI            : DebugSource
    DebugSourceShaderCompiler : DebugSource
    DebugSourceWindowSystem   : DebugSource
    DebugSourceThirdParty     : DebugSource
    DebugSourceApplication    : DebugSource
    DebugSourceOther          : DebugSource

{-# COMPILE GHC DebugSource = data Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSource
    ( Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSourceAPI
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSourceShaderCompiler
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSourceWindowSystem
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSourceThirdParty
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSourceApplication
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSourceOther
    ) #-}

postulate
    Eq[DebugSource]   : Eq DebugSource
    Ord[DebugSource]  : Ord DebugSource
    Show[DebugSource] : Show DebugSource

{-# COMPILE GHC Eq[DebugSource]   = AgdaEq   #-}
{-# COMPILE GHC Ord[DebugSource]  = AgdaOrd  #-}
{-# COMPILE GHC Show[DebugSource] = AgdaShow #-}


data DebugType : Set where
    DebugTypeError              : DebugType
    DebugTypeDeprecatedBehavior : DebugType
    DebugTypeUndefinedBehavior  : DebugType
    DebugTypePerformance        : DebugType
    DebugTypePortability        : DebugType
    DebugTypeMarker             : DebugType
    DebugTypePushGroup          : DebugType
    DebugTypePopGroup           : DebugType
    DebugTypeOther              : DebugType

{-# COMPILE GHC DebugType = data Graphics.Rendering.OpenGL.GL.DebugOutput.DebugType
    ( Graphics.Rendering.OpenGL.GL.DebugOutput.DebugTypeError
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugTypeDeprecatedBehavior
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugTypeUndefinedBehavior
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugTypePerformance
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugTypePortability
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugTypeMarker
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugTypePushGroup
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugTypePopGroup
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugTypeOther
    ) #-}

postulate
    Eq[DebugType]   : Eq DebugType
    Ord[DebugType]  : Ord DebugType
    Show[DebugType] : Show DebugType

{-# COMPILE GHC Eq[DebugType]   = AgdaEq   #-}
{-# COMPILE GHC Ord[DebugType]  = AgdaOrd  #-}
{-# COMPILE GHC Show[DebugType] = AgdaShow #-}


data DebugMessageID : Set where
    mkDebugMessageID : GLuint → DebugMessageID

{-# COMPILE GHC DebugMessageID = data Graphics.Rendering.OpenGL.GL.DebugOutput.DebugMessageID
    ( Graphics.Rendering.OpenGL.GL.DebugOutput.DebugMessageID
    ) #-}

postulate
    Eq[DebugMessageID]   : Eq DebugMessageID
    Ord[DebugMessageID]  : Ord DebugMessageID
    Show[DebugMessageID] : Show DebugMessageID

{-# COMPILE GHC Eq[DebugMessageID]   = AgdaEq   #-}
{-# COMPILE GHC Ord[DebugMessageID]  = AgdaOrd  #-}
{-# COMPILE GHC Show[DebugMessageID] = AgdaShow #-}


data DebugSeverity : Set where
    DebugSeverityHigh         : DebugSeverity
    DebugSeverityMedium       : DebugSeverity
    DebugSeverityLow          : DebugSeverity
    DebugSeverityNotification : DebugSeverity

{-# COMPILE GHC DebugSeverity = data Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSeverity
    ( Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSeverityHigh
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSeverityMedium
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSeverityLow
    | Graphics.Rendering.OpenGL.GL.DebugOutput.DebugSeverityNotification
    ) #-}


data DebugMessage : Set where
    mkDebugMessage : DebugSource → DebugType → DebugMessageID → DebugSeverity → List Char → DebugMessage

{-# COMPILE GHC DebugMessage = data Graphics.Rendering.OpenGL.GL.DebugOutput.DebugMessage
    ( Graphics.Rendering.OpenGL.GL.DebugOutput.DebugMessage
    ) #-}

postulate
    Eq[DebugMessage]   : Eq DebugMessage
    Ord[DebugMessage]  : Ord DebugMessage
    Show[DebugMessage] : Show DebugMessage

{-# COMPILE GHC Eq[DebugMessage]   = AgdaEq   #-}
{-# COMPILE GHC Ord[DebugMessage]  = AgdaOrd  #-}
{-# COMPILE GHC Show[DebugMessage] = AgdaShow #-}


data MessageGroup : Set where
    mkMessageGroup      : Maybe DebugSource → Maybe DebugType → Maybe DebugSeverity → MessageGroup
    MessageGroupWithIDs : DebugSource → DebugType → List DebugMessageID → MessageGroup

{-# COMPILE GHC MessageGroup = data Graphics.Rendering.OpenGL.GL.DebugOutput.MessageGroup
    ( Graphics.Rendering.OpenGL.GL.DebugOutput.MessageGroup
    | Graphics.Rendering.OpenGL.GL.DebugOutput.MessageGroupWithIDs
    ) #-}

postulate
    Eq[MessageGroup]   : Eq MessageGroup
    Ord[MessageGroup]  : Ord MessageGroup
    Show[MessageGroup] : Show MessageGroup

{-# COMPILE GHC Eq[MessageGroup]   = AgdaEq   #-}
{-# COMPILE GHC Ord[MessageGroup]  = AgdaOrd  #-}
{-# COMPILE GHC Show[MessageGroup] = AgdaShow #-}


data DebugGroup : Set where
    mkDebugGroup : DebugSource → DebugMessageID → List Char → DebugGroup

{-# COMPILE GHC DebugGroup = data Graphics.Rendering.OpenGL.GL.DebugOutput.DebugGroup
    ( Graphics.Rendering.OpenGL.GL.DebugOutput.DebugGroup
    ) #-}


postulate
    CanBeLabeled : Set aℓ → Set aℓ
    objectLabel : ⦃ CanBeLabeled A ⦄ → A → StateVar (Maybe (List Char))

{-# FOREIGN GHC data AgdaCanBeLabeled aℓ a = Graphics.Rendering.OpenGL.GL.DebugOutput.CanBeLabeled a => AgdaCanBeLabeled #-}
{-# COMPILE GHC CanBeLabeled = type(0) AgdaCanBeLabeled #-}

{-# COMPILE GHC objectLabel = \ aℓ a AgdaCanBeLabeled -> Graphics.Rendering.OpenGL.GL.DebugOutput.objectLabel #-}

postulate
    debugOutput             : StateVar Capability
    maxDebugMessageLength   : GettableStateVar GLsizei
    debugMessageCallback    : StateVar (Maybe (DebugMessage → IO ⊤))
    maxDebugLoggedMessages  : GettableStateVar GLsizei
    debugLoggedMessages     : IO (List DebugMessage)
    debugMessageInsert      : DebugMessage → IO ⊤
    debugMessageControl     : MessageGroup → SettableStateVar Capability
    pushDebugGroup          : DebugSource → DebugMessageID → List Char → IO ⊤
    popDebugGroup           : IO ⊤
    maxDebugGroupStackDepth : GettableStateVar GLsizei
    maxLabelLength          : GettableStateVar GLsizei
    debugOutputSynchronous  : StateVar Capability

    withDebugGroup : DebugSource → DebugMessageID → List Char → IO A → IO A

{-# COMPILE GHC debugOutput             = Graphics.Rendering.OpenGL.GL.DebugOutput.debugOutput             #-}
{-# COMPILE GHC maxDebugMessageLength   = Graphics.Rendering.OpenGL.GL.DebugOutput.maxDebugMessageLength   #-}
{-# COMPILE GHC debugMessageCallback    = Graphics.Rendering.OpenGL.GL.DebugOutput.debugMessageCallback    #-}
{-# COMPILE GHC maxDebugLoggedMessages  = Graphics.Rendering.OpenGL.GL.DebugOutput.maxDebugLoggedMessages  #-}
{-# COMPILE GHC debugLoggedMessages     = Graphics.Rendering.OpenGL.GL.DebugOutput.debugLoggedMessages     #-}
{-# COMPILE GHC debugMessageInsert      = Graphics.Rendering.OpenGL.GL.DebugOutput.debugMessageInsert      #-}
{-# COMPILE GHC debugMessageControl     = Graphics.Rendering.OpenGL.GL.DebugOutput.debugMessageControl     #-}
{-# COMPILE GHC pushDebugGroup          = Graphics.Rendering.OpenGL.GL.DebugOutput.pushDebugGroup          #-}
{-# COMPILE GHC popDebugGroup           = Graphics.Rendering.OpenGL.GL.DebugOutput.popDebugGroup           #-}
{-# COMPILE GHC maxDebugGroupStackDepth = Graphics.Rendering.OpenGL.GL.DebugOutput.maxDebugGroupStackDepth #-}
{-# COMPILE GHC maxLabelLength          = Graphics.Rendering.OpenGL.GL.DebugOutput.maxLabelLength          #-}
{-# COMPILE GHC debugOutputSynchronous  = Graphics.Rendering.OpenGL.GL.DebugOutput.debugOutputSynchronous  #-}

{-# COMPILE GHC withDebugGroup = \ aℓ a -> Graphics.Rendering.OpenGL.GL.DebugOutput.withDebugGroup #-}
