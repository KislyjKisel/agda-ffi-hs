{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Internal.Types where

open import Ffi.Hs.-base.Class          using (Eq; Data; Ord; Show)
open import Ffi.Hs.SDL.Raw.Types as Raw using ()

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Internal.Types
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

record Joystick : Set where
    constructor mkJoystick
    field
        joystickPtr : Raw.Joystick

{-# COMPILE GHC Joystick = data SDL.Internal.Types.Joystick (SDL.Internal.Types.Joystick) #-}

postulate
    Eq[Joystick]   : Eq Joystick
    Data[Joystick] : Data Joystick
    Ord[Joystick]  : Ord Joystick
    Show[Joystick] : Show Joystick

{-# COMPILE GHC Eq[Joystick]   = AgdaEq   #-}
{-# COMPILE GHC Data[Joystick] = AgdaData #-}
{-# COMPILE GHC Ord[Joystick]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Joystick] = AgdaShow #-}


data Window : Set where
    mkWindow : Raw.Window → Window

{-# COMPILE GHC Window = data SDL.Internal.Types.Window (SDL.Internal.Types.Window) #-}

postulate
    Eq[Window]   : Eq Window
    Data[Window] : Data Window
    Ord[Window]  : Ord Window
    Show[Window] : Show Window

{-# COMPILE GHC Eq[Window]   = AgdaEq   #-}
{-# COMPILE GHC Data[Window] = AgdaData #-}
{-# COMPILE GHC Ord[Window]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Window] = AgdaShow #-}


data Renderer : Set where
    mkRenderer : Raw.Renderer → Renderer

{-# COMPILE GHC Renderer = data SDL.Internal.Types.Renderer (SDL.Internal.Types.Renderer) #-}

postulate
    Eq[Renderer]   : Eq Renderer
    Data[Renderer] : Data Renderer
    Ord[Renderer]  : Ord Renderer
    Show[Renderer] : Show Renderer

{-# COMPILE GHC Eq[Renderer]   = AgdaEq   #-}
{-# COMPILE GHC Data[Renderer] = AgdaData #-}
{-# COMPILE GHC Ord[Renderer]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Renderer] = AgdaShow #-}
