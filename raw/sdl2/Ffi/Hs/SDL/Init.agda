{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Init where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit  using (⊤; ⊤′)
open import Ffi.Hs.Data.Tuple  using (Tuple3)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Init
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ mℓ : Level
        A : Set aℓ
        F : Set aℓ → Set bℓ
        M : Set mℓ → Set mℓ

data InitFlag : Set where
    InitTimer          : InitFlag
    InitAudio          : InitFlag
    InitVideo          : InitFlag
    InitJoystick       : InitFlag
    InitHaptic         : InitFlag
    InitGameController : InitFlag
    InitEvents         : InitFlag

{-# COMPILE GHC InitFlag = data SDL.Init.InitFlag
    ( SDL.Init.InitTimer
    | SDL.Init.InitAudio
    | SDL.Init.InitVideo
    | SDL.Init.InitJoystick
    | SDL.Init.InitHaptic
    | SDL.Init.InitGameController
    | SDL.Init.InitEvents
    ) #-}

postulate
    Bounded[InitFlag] : Bounded InitFlag
    Enum[InitFlag]    : Enum InitFlag
    Eq[InitFlag]      : Eq InitFlag
    Data[InitFlag]    : Data InitFlag
    Ord[InitFlag]     : Ord InitFlag
    Read[InitFlag]    : Read InitFlag
    Show[InitFlag]    : Show InitFlag

{-# COMPILE GHC Bounded[InitFlag] = AgdaBounded #-}
{-# COMPILE GHC Enum[InitFlag]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[InitFlag]      = AgdaEq      #-}
{-# COMPILE GHC Data[InitFlag]    = AgdaData    #-}
{-# COMPILE GHC Ord[InitFlag]     = AgdaOrd     #-}
{-# COMPILE GHC Read[InitFlag]    = AgdaRead    #-}
{-# COMPILE GHC Show[InitFlag]    = AgdaShow    #-}

postulate
    initialize    : ⦃ Foldable F ⦄ → ⦃ MonadIO M ⦄ → F InitFlag → M ⊤′
    initializeAll : ⦃ MonadIO M ⦄ → M ⊤′
    quit          : ⦃ MonadIO M ⦄ → M ⊤′
    version       : ⦃ Integral A ⦄ → ⦃ MonadIO M ⦄ → M (Tuple3 A A A)

{-# COMPILE GHC initialize    = \ bℓ f mℓ m AgdaFoldable AgdaMonadIO -> SDL.Init.initialize    #-}
{-# COMPILE GHC initializeAll = \ mℓ m AgdaMonadIO                   -> SDL.Init.initializeAll #-}
{-# COMPILE GHC quit          = \ mℓ m AgdaMonadIO                   -> SDL.Init.quit          #-}
{-# COMPILE GHC version       = \ aℓ a m AgdaIntegral AgdaMonadIO    -> SDL.Init.version       #-}
