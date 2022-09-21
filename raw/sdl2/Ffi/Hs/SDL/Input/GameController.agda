{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Input.GameController where

open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Input.GameController
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

data ControllerButton : Set where
    ControllerButtonInvalid       : ControllerButton
    ControllerButtonA             : ControllerButton
    ControllerButtonB             : ControllerButton
    ControllerButtonX             : ControllerButton
    ControllerButtonY             : ControllerButton
    ControllerButtonBack          : ControllerButton
    ControllerButtonGuide         : ControllerButton
    ControllerButtonStart         : ControllerButton
    ControllerButtonLeftStick     : ControllerButton
    ControllerButtonRightStick    : ControllerButton
    ControllerButtonLeftShoulder  : ControllerButton
    ControllerButtonRightShoulder : ControllerButton
    ControllerButtonDpadUp        : ControllerButton
    ControllerButtonDpadDown      : ControllerButton
    ControllerButtonDpadLeft      : ControllerButton
    ControllerButtonDpadRight     : ControllerButton

{-# COMPILE GHC ControllerButton = data SDL.Input.GameController.ControllerButton
    ( SDL.Input.GameController.ControllerButtonInvalid
    | SDL.Input.GameController.ControllerButtonA
    | SDL.Input.GameController.ControllerButtonB
    | SDL.Input.GameController.ControllerButtonX
    | SDL.Input.GameController.ControllerButtonY
    | SDL.Input.GameController.ControllerButtonBack
    | SDL.Input.GameController.ControllerButtonGuide
    | SDL.Input.GameController.ControllerButtonStart
    | SDL.Input.GameController.ControllerButtonLeftStick
    | SDL.Input.GameController.ControllerButtonRightStick
    | SDL.Input.GameController.ControllerButtonLeftShoulder
    | SDL.Input.GameController.ControllerButtonRightShoulder
    | SDL.Input.GameController.ControllerButtonDpadUp
    | SDL.Input.GameController.ControllerButtonDpadDown
    | SDL.Input.GameController.ControllerButtonDpadLeft
    | SDL.Input.GameController.ControllerButtonDpadRight
    ) #-}

postulate
    Eq[ControllerButton]   : Eq ControllerButton
    Data[ControllerButton] : Data ControllerButton
    Ord[ControllerButton]  : Ord ControllerButton
    Read[ControllerButton] : Read ControllerButton
    Show[ControllerButton] : Show ControllerButton

{-# COMPILE GHC Eq[ControllerButton]   = AgdaEq   #-}
{-# COMPILE GHC Data[ControllerButton] = AgdaData #-}
{-# COMPILE GHC Ord[ControllerButton]  = AgdaOrd  #-}
{-# COMPILE GHC Read[ControllerButton] = AgdaRead #-}
{-# COMPILE GHC Show[ControllerButton] = AgdaShow #-}


data ControllerButtonState : Set where
    ControllerButtonPressed      : ControllerButtonState
    ControllerButtonReleased     : ControllerButtonState
    ControllerButtonInvalidState : ControllerButtonState

{-# COMPILE GHC ControllerButtonState = data SDL.Input.GameController.ControllerButtonState
    ( SDL.Input.GameController.ControllerButtonPressed
    | SDL.Input.GameController.ControllerButtonReleased
    | SDL.Input.GameController.ControllerButtonInvalidState
    ) #-}

postulate
    Eq[ControllerButtonState]   : Eq ControllerButtonState
    Data[ControllerButtonState] : Data ControllerButtonState
    Ord[ControllerButtonState]  : Ord ControllerButtonState
    Read[ControllerButtonState] : Read ControllerButtonState
    Show[ControllerButtonState] : Show ControllerButtonState

{-# COMPILE GHC Eq[ControllerButtonState]   = AgdaEq   #-}
{-# COMPILE GHC Data[ControllerButtonState] = AgdaData #-}
{-# COMPILE GHC Ord[ControllerButtonState]  = AgdaOrd  #-}
{-# COMPILE GHC Read[ControllerButtonState] = AgdaRead #-}
{-# COMPILE GHC Show[ControllerButtonState] = AgdaShow #-}


data ControllerDeviceConnection : Set where
    ControllerDeviceAdded    : ControllerDeviceConnection
    ControllerDeviceRemoved  : ControllerDeviceConnection
    ControllerDeviceRemapped : ControllerDeviceConnection

{-# COMPILE GHC ControllerDeviceConnection = data SDL.Input.GameController.ControllerDeviceConnection
    ( SDL.Input.GameController.ControllerDeviceAdded
    | SDL.Input.GameController.ControllerDeviceRemoved
    | SDL.Input.GameController.ControllerDeviceRemapped
    ) #-}

postulate
    Eq[ControllerDeviceConnection]   : Eq ControllerDeviceConnection
    Data[ControllerDeviceConnection] : Data ControllerDeviceConnection
    Ord[ControllerDeviceConnection]  : Ord ControllerDeviceConnection
    Read[ControllerDeviceConnection] : Read ControllerDeviceConnection
    Show[ControllerDeviceConnection] : Show ControllerDeviceConnection

{-# COMPILE GHC Eq[ControllerDeviceConnection]   = AgdaEq   #-}
{-# COMPILE GHC Data[ControllerDeviceConnection] = AgdaData #-}
{-# COMPILE GHC Ord[ControllerDeviceConnection]  = AgdaOrd  #-}
{-# COMPILE GHC Read[ControllerDeviceConnection] = AgdaRead #-}
{-# COMPILE GHC Show[ControllerDeviceConnection] = AgdaShow #-}
