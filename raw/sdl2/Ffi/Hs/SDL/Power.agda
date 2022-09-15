{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Power where

open import Agda.Builtin.Maybe            using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.Monad.IO.Class using (MonadIO)
open import Ffi.Hs.Foreign.C.Types        using (CInt)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Power
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ

data BatteryState : Set where
    Draining : BatteryState
    Charged  : BatteryState
    Charging : BatteryState

{-# COMPILE GHC BatteryState = data SDL.Power.BatteryState
    ( SDL.Power.Draining
    | SDL.Power.Charged
    | SDL.Power.Charging
    ) #-}

postulate
    Bounded[BatteryState] : Bounded BatteryState
    Enum[BatteryState]    : Enum BatteryState
    Eq[BatteryState]      : Eq BatteryState
    Data[BatteryState]    : Data BatteryState
    Ord[BatteryState]     : Ord BatteryState
    Read[BatteryState]    : Read BatteryState
    Show[BatteryState]    : Show BatteryState

{-# COMPILE GHC Bounded[BatteryState] = AgdaBounded #-}
{-# COMPILE GHC Enum[BatteryState]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[BatteryState]      = AgdaEq      #-}
{-# COMPILE GHC Data[BatteryState]    = AgdaData    #-}
{-# COMPILE GHC Ord[BatteryState]     = AgdaOrd     #-}
{-# COMPILE GHC Read[BatteryState]    = AgdaRead    #-}
{-# COMPILE GHC Show[BatteryState]    = AgdaShow    #-}

record Charge : Set where
    constructor mkCharge
    field
        chargeSecondsLeft : Maybe CInt
        chargePercent     : Maybe CInt

{-# COMPILE GHC Charge = data SDL.Power.Charge (SDL.Power.Charge) #-}

postulate
    Eq[Charge]   : Eq Charge
    Ord[Charge]  : Ord Charge
    Read[Charge] : Read Charge
    Show[Charge] : Show Charge

{-# COMPILE GHC Eq[Charge]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Charge]  = AgdaOrd  #-}
{-# COMPILE GHC Read[Charge] = AgdaRead #-}
{-# COMPILE GHC Show[Charge] = AgdaShow #-}

data PowerState : Set where
    Battery           : BatteryState → Charge → PowerState
    Mains             : PowerState
    UnknownPowerState : PowerState

{-# COMPILE GHC PowerState = data SDL.Power.PowerState
    ( SDL.Power.Battery
    | SDL.Power.Mains
    | SDL.Power.UnknownPowerState
    ) #-}

postulate
    Eq[PowerState]   : Eq PowerState
    Ord[PowerState]  : Ord PowerState
    Read[PowerState] : Read PowerState
    Show[PowerState] : Show PowerState

{-# COMPILE GHC Eq[PowerState]   = AgdaEq   #-}
{-# COMPILE GHC Ord[PowerState]  = AgdaOrd  #-}
{-# COMPILE GHC Read[PowerState] = AgdaRead #-}
{-# COMPILE GHC Show[PowerState] = AgdaShow #-}

postulate
    getPowerInfo : ⦃ MonadIO M ⦄ → M PowerState

{-# COMPILE GHC getPowerInfo = \ mℓ m AgdaMonadIO -> SDL.Power.getPowerInfo #-}
