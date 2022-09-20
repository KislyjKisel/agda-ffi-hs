{-# OPTIONS --without-K #-}

module Ffi.Hs.Debug.Trace where

open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Show; Applicative)
open import Ffi.Hs.-base.Unit  using (⊤; ⊤′)

{-# FOREIGN GHC
import qualified Debug.Trace
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ fℓ : Level
        A : Set aℓ
        B : Set bℓ
        F : Set fℓ → Set fℓ

postulate
    trace       : List Char → A → A
    traceId     : List Char → List Char
    traceShow   : ⦃ Show A ⦄ → A → B → B
    traceShowId : ⦃ Show A ⦄ → A → A
    traceStack  : List Char → A → A
    traceIO     : List Char → IO ⊤
    traceM      : ⦃ Applicative F ⦄ → List Char → F ⊤′
    traceShowM  : ⦃ Show A ⦄ → ⦃ Applicative F ⦄ → A → F ⊤′

{-# COMPILE GHC trace       = \ aℓ a                               -> Debug.Trace.trace       #-}
{-# COMPILE GHC traceId     =                                         Debug.Trace.traceId     #-}
{-# COMPILE GHC traceShow   = \ aℓ a bℓ b AgdaShow                 -> Debug.Trace.traceShow   #-}
{-# COMPILE GHC traceShowId = \ aℓ a AgdaShow                      -> Debug.Trace.traceShowId #-}
{-# COMPILE GHC traceStack  = \ aℓ a                               -> Debug.Trace.traceStack  #-}
{-# COMPILE GHC traceIO     =                                         Debug.Trace.traceIO     #-}
{-# COMPILE GHC traceM      = \ fℓ f AgdaApplicative               -> Debug.Trace.traceM      #-}
{-# COMPILE GHC traceShowM  = \ aℓ a fℓ f AgdaShow AgdaApplicative -> Debug.Trace.traceShowM  #-}

postulate
    traceEvent    : List Char → A → A
    traceEventIO  : List Char → IO ⊤
    flushEventLog : IO ⊤
    traceMarker   : List Char → A → A
    traceMarkerIO : List Char → IO ⊤

{-# COMPILE GHC traceEvent    = \ aℓ a -> Debug.Trace.traceEvent    #-}
{-# COMPILE GHC traceEventIO  =           Debug.Trace.traceEventIO  #-}
{-# COMPILE GHC flushEventLog =           Debug.Trace.flushEventLog #-}
{-# COMPILE GHC traceMarker   = \ aℓ a -> Debug.Trace.traceMarker   #-}
{-# COMPILE GHC traceMarkerIO =           Debug.Trace.traceMarkerIO #-}
