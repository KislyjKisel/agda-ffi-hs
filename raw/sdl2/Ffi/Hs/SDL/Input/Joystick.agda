{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Input.Joystick where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.String    using () renaming (String to Text)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level     using (Liftℓ)
open import Ffi.Hs.-base.Unit      using (⊤′)
open import Ffi.Hs.Data.Int        using (Int16; Int32)
-- open import Ffi.Hs.Data.Vector     using (Vector)
open import Ffi.Hs.Foreign.C.Types using (CInt)
open import Ffi.Hs.SDL.Vect        using (V2)

open import Ffi.Hs.SDL.Internal.Types public
    using
    ( Joystick
    ; Eq[Joystick]
    ; Data[Joystick]
    ; Ord[Joystick]
    ; Show[Joystick]
    )

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Input.Joystick
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ

record JoystickDevice : Set where
    constructor mkJoystickDevice
    field
        joystickDeviceName : Text
        joystickDeviceId   : CInt

{-# COMPILE GHC JoystickDevice = data SDL.Input.Joystick.JoystickDevice (SDL.Input.Joystick.JoystickDevice) #-}

postulate
    Eq[JoystickDevice]   : Eq JoystickDevice
    Ord[JoystickDevice]  : Ord JoystickDevice
    Read[JoystickDevice] : Read JoystickDevice
    Show[JoystickDevice] : Show JoystickDevice

{-# COMPILE GHC Eq[JoystickDevice]   = AgdaEq   #-}
{-# COMPILE GHC Ord[JoystickDevice]  = AgdaOrd  #-}
{-# COMPILE GHC Read[JoystickDevice] = AgdaRead #-}
{-# COMPILE GHC Show[JoystickDevice] = AgdaShow #-}


data JoyButtonState : Set where
    JoyButtonPressed  : JoyButtonState
    JoyButtonReleased : JoyButtonState

{-# COMPILE GHC JoyButtonState = data SDL.Input.Joystick.JoyButtonState
    ( SDL.Input.Joystick.JoyButtonPressed
    | SDL.Input.Joystick.JoyButtonReleased
    ) #-}

postulate
    Eq[JoyButtonState]   : Eq JoyButtonState
    Data[JoyButtonState] : Data JoyButtonState
    Ord[JoyButtonState]  : Ord JoyButtonState
    Read[JoyButtonState] : Read JoyButtonState
    Show[JoyButtonState] : Show JoyButtonState

{-# COMPILE GHC Eq[JoyButtonState]   = AgdaEq   #-}
{-# COMPILE GHC Data[JoyButtonState] = AgdaData #-}
{-# COMPILE GHC Ord[JoyButtonState]  = AgdaOrd  #-}
{-# COMPILE GHC Read[JoyButtonState] = AgdaRead #-}
{-# COMPILE GHC Show[JoyButtonState] = AgdaShow #-}


data JoyHatPosition : Set where
    HatCentered  : JoyHatPosition
    HatUp        : JoyHatPosition
    HatRight     : JoyHatPosition
    HatDown      : JoyHatPosition
    HatLeft      : JoyHatPosition
    HatRightUp   : JoyHatPosition
    HatRightDown : JoyHatPosition
    HatLeftUp    : JoyHatPosition
    HatLeftDown  : JoyHatPosition

{-# COMPILE GHC JoyHatPosition = data SDL.Input.Joystick.JoyHatPosition
    ( SDL.Input.Joystick.HatCentered
    | SDL.Input.Joystick.HatUp
    | SDL.Input.Joystick.HatRight
    | SDL.Input.Joystick.HatDown
    | SDL.Input.Joystick.HatLeft
    | SDL.Input.Joystick.HatRightUp
    | SDL.Input.Joystick.HatRightDown
    | SDL.Input.Joystick.HatLeftUp
    | SDL.Input.Joystick.HatLeftDown
    ) #-}

postulate
    Eq[JoyHatPosition]   : Eq JoyHatPosition
    Data[JoyHatPosition] : Data JoyHatPosition
    Ord[JoyHatPosition]  : Ord JoyHatPosition
    Read[JoyHatPosition] : Read JoyHatPosition
    Show[JoyHatPosition] : Show JoyHatPosition

{-# COMPILE GHC Eq[JoyHatPosition]   = AgdaEq   #-}
{-# COMPILE GHC Data[JoyHatPosition] = AgdaData #-}
{-# COMPILE GHC Ord[JoyHatPosition]  = AgdaOrd  #-}
{-# COMPILE GHC Read[JoyHatPosition] = AgdaRead #-}
{-# COMPILE GHC Show[JoyHatPosition] = AgdaShow #-}


data JoyDeviceConnection : Set where
    JoyDeviceAdded   : JoyDeviceConnection
    JoyDeviceRemoved : JoyDeviceConnection

{-# COMPILE GHC JoyDeviceConnection = data SDL.Input.Joystick.JoyDeviceConnection
    ( SDL.Input.Joystick.JoyDeviceAdded
    | SDL.Input.Joystick.JoyDeviceRemoved
    ) #-}

postulate
    Eq[JoyDeviceConnection]   : Eq JoyDeviceConnection
    Data[JoyDeviceConnection] : Data JoyDeviceConnection
    Ord[JoyDeviceConnection]  : Ord JoyDeviceConnection
    Read[JoyDeviceConnection] : Read JoyDeviceConnection
    Show[JoyDeviceConnection] : Show JoyDeviceConnection

{-# COMPILE GHC Eq[JoyDeviceConnection]   = AgdaEq   #-}
{-# COMPILE GHC Data[JoyDeviceConnection] = AgdaData #-}
{-# COMPILE GHC Ord[JoyDeviceConnection]  = AgdaOrd  #-}
{-# COMPILE GHC Read[JoyDeviceConnection] = AgdaRead #-}
{-# COMPILE GHC Show[JoyDeviceConnection] = AgdaShow #-}


postulate
    numJoysticks       : ⦃ MonadIO M ⦄ → M (Liftℓ _ CInt)
    -- todo: (req Data.Vector) availableJoysticks : ⦃ MonadIO M ⦄ → M (Liftℓ _ (Vector JoystickDevice))
    openJoystick       : ⦃ MonadIO M ⦄ → JoystickDevice → M (Liftℓ _ Joystick)
    closeJoystick      : ⦃ MonadIO M ⦄ → Joystick → M ⊤′
    getJoystickID      : ⦃ MonadIO M ⦄ → Joystick → M (Liftℓ _ Int32)

    buttonPressed : ⦃ MonadIO M ⦄ → Joystick → CInt → M (Liftℓ _ Bool)
    ballDelta     : ⦃ MonadIO M ⦄ → Joystick → CInt → M (Liftℓ _ (V2 CInt))
    axisPosition  : ⦃ MonadIO M ⦄ → Joystick → CInt → M (Liftℓ _ Int16)
    numAxes       : ⦃ MonadIO M ⦄ → Joystick → M (Liftℓ _ CInt)
    numButtons    : ⦃ MonadIO M ⦄ → Joystick → M (Liftℓ _ CInt)
    numBalls      : ⦃ MonadIO M ⦄ → Joystick → M (Liftℓ _ CInt)
    getHat        : ⦃ MonadIO M ⦄ → Joystick → CInt → M (Liftℓ _ JoyHatPosition)
    numHats       : ⦃ MonadIO M ⦄ → Joystick → M (Liftℓ _  CInt)

{-# COMPILE GHC numJoysticks       = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.numJoysticks       #-}
-- todo: {-# COMPILE GHC availableJoysticks = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.availableJoysticks #-}
{-# COMPILE GHC openJoystick       = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.openJoystick       #-}
{-# COMPILE GHC closeJoystick      = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.closeJoystick      #-}
{-# COMPILE GHC getJoystickID      = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.getJoystickID      #-}

{-# COMPILE GHC buttonPressed = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.buttonPressed #-}
{-# COMPILE GHC ballDelta     = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.ballDelta     #-}
{-# COMPILE GHC axisPosition  = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.axisPosition  #-}
{-# COMPILE GHC numAxes       = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.numAxes       #-}
{-# COMPILE GHC numButtons    = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.numButtons    #-}
{-# COMPILE GHC numBalls      = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.numBalls      #-}
{-# COMPILE GHC getHat        = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.getHat        #-}
{-# COMPILE GHC numHats       = \ mℓ m AgdaMonadIO -> SDL.Input.Joystick.numHats       #-}
