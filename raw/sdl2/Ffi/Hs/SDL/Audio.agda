{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Audio where

open import Agda.Builtin.IO                     using (IO)
open import Agda.Builtin.Maybe                  using (Maybe)
open import Agda.Builtin.String                 using () renaming (String to Text)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Float                  using (Float)
open import Ffi.Hs.-base.Level                  using (Liftℓ)
open import Ffi.Hs.-base.Unit                   using (⊤)
open import Ffi.Hs.Data.Int                     using (Int8; Int16; Int32)
open import Ffi.Hs.Data.Tuple                   using (Tuple2)
open import Ffi.Hs.Data.Vector.Storable         using (Vector)
open import Ffi.Hs.Data.Vector.Storable.Mutable using (IOVector)
open import Ffi.Hs.Data.Word                    using (Word8; Word16; Word32)
open import Ffi.Hs.Foreign.C.Types              using (CInt)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Audio
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        M : Set aℓ → Set aℓ

postulate
    AudioDevice : Set
    Eq[AudioDevice] : Eq AudioDevice

{-# COMPILE GHC AudioDevice = type SDL.Audio.AudioDevice #-}
{-# COMPILE GHC Eq[AudioDevice] = AgdaEq #-}


data AudioDeviceUsage : Set where
    ForPlayback : AudioDeviceUsage
    ForCapture  : AudioDeviceUsage

{-# COMPILE GHC AudioDeviceUsage = data SDL.Audio.AudioDeviceUsage
    ( SDL.Audio.ForPlayback
    | SDL.Audio.ForCapture
    ) #-}

postulate
    Bounded[AudioDeviceUsage] : Bounded AudioDeviceUsage
    Enum[AudioDeviceUsage]    : Enum AudioDeviceUsage
    Eq[AudioDeviceUsage]      : Eq AudioDeviceUsage
    Data[AudioDeviceUsage]    : Data AudioDeviceUsage
    Ord[AudioDeviceUsage]     : Ord AudioDeviceUsage
    Read[AudioDeviceUsage]    : Read AudioDeviceUsage
    Show[AudioDeviceUsage]    : Show AudioDeviceUsage

{-# COMPILE GHC Bounded[AudioDeviceUsage] = AgdaBounded #-}
{-# COMPILE GHC Enum[AudioDeviceUsage]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[AudioDeviceUsage]      = AgdaEq      #-}
{-# COMPILE GHC Data[AudioDeviceUsage]    = AgdaData    #-}
{-# COMPILE GHC Ord[AudioDeviceUsage]     = AgdaOrd     #-}
{-# COMPILE GHC Read[AudioDeviceUsage]    = AgdaRead    #-}
{-# COMPILE GHC Show[AudioDeviceUsage]    = AgdaShow    #-}


data Channels : Set where
    Mono         : Channels
    Stereo       : Channels
    Quad         : Channels
    FivePointOne : Channels

{-# COMPILE GHC Channels = data SDL.Audio.Channels
    ( SDL.Audio.Mono
    | SDL.Audio.Stereo
    | SDL.Audio.Quad
    | SDL.Audio.FivePointOne
    ) #-}

postulate
    Bounded[Channels] : Bounded Channels
    Enum[Channels]    : Enum Channels
    Eq[Channels]      : Eq Channels
    Data[Channels]    : Data Channels
    Ord[Channels]     : Ord Channels
    Read[Channels]    : Read Channels
    Show[Channels]    : Show Channels

{-# COMPILE GHC Bounded[Channels] = AgdaBounded #-}
{-# COMPILE GHC Enum[Channels]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[Channels]      = AgdaEq      #-}
{-# COMPILE GHC Data[Channels]    = AgdaData    #-}
{-# COMPILE GHC Ord[Channels]     = AgdaOrd     #-}
{-# COMPILE GHC Read[Channels]    = AgdaRead    #-}
{-# COMPILE GHC Show[Channels]    = AgdaShow    #-}


data Changeable (A : Set aℓ) : Set aℓ where
    Mandate : A → Changeable A
    Desire  : A → Changeable A

{-# FOREIGN GHC type AgdaChangeable aℓ = SDL.Audio.Changeable #-}
{-# COMPILE GHC Changeable = data(1) AgdaChangeable
    ( SDL.Audio.Mandate
    | SDL.Audio.Desire
    ) #-}

postulate
    Functor[Changeable]     : Functor {aℓ} Changeable
    Foldable[Changeable]    : Foldable {aℓ} Changeable
    Traversable[Changeable] : Traversable {aℓ} Changeable
    Eq[Changeable[A]]       : ⦃ Eq A ⦄ → Eq (Changeable A)
    Data[Changeable[A]]     : ⦃ Data A ⦄ → Data (Changeable A)
    Read[Changeable[A]]     : ⦃ Read A ⦄ → Read (Changeable A)
    Show[Changeable[A]]     : ⦃ Show A ⦄ → Show (Changeable A)

{-# COMPILE GHC Functor[Changeable]     = AgdaFunctor     #-}
{-# COMPILE GHC Foldable[Changeable]    = AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Changeable] = AgdaTraversable #-}
{-# COMPILE GHC Eq[Changeable[A]]       = AgdaEq          #-}
{-# COMPILE GHC Data[Changeable[A]]     = AgdaData        #-}
{-# COMPILE GHC Read[Changeable[A]]     = AgdaRead        #-}
{-# COMPILE GHC Show[Changeable[A]]     = AgdaShow        #-}


data AudioFormat : Set → Set₁ where
    Signed8BitAudio          : AudioFormat Int8
    Unsigned8BitAudio        : AudioFormat Word8
    Signed16BitLEAudio       : AudioFormat Int16
    Signed16BitBEAudio       : AudioFormat Int16
    Signed16BitNativeAudio   : AudioFormat Int16
    Unsigned16BitLEAudio     : AudioFormat Word16
    Unsigned16BitBEAudio     : AudioFormat Word16
    Unsigned16BitNativeAudio : AudioFormat Word16
    Signed32BitLEAudio       : AudioFormat Int32
    Signed32BitBEAudio       : AudioFormat Int32
    Signed32BitNativeAudio   : AudioFormat Int32
    FloatingLEAudio          : AudioFormat Float
    FloatingBEAudio          : AudioFormat Float
    FloatingNativeAudio      : AudioFormat Float

{-# COMPILE GHC AudioFormat = data SDL.Audio.AudioFormat
    ( SDL.Audio.Signed8BitAudio
    | SDL.Audio.Unsigned8BitAudio
    | SDL.Audio.Signed16BitLEAudio
    | SDL.Audio.Signed16BitBEAudio
    | SDL.Audio.Signed16BitNativeAudio
    | SDL.Audio.Unsigned16BitLEAudio
    | SDL.Audio.Unsigned16BitBEAudio
    | SDL.Audio.Unsigned16BitNativeAudio
    | SDL.Audio.Signed32BitLEAudio
    | SDL.Audio.Signed32BitBEAudio
    | SDL.Audio.Signed32BitNativeAudio
    | SDL.Audio.FloatingLEAudio
    | SDL.Audio.FloatingBEAudio
    | SDL.Audio.FloatingNativeAudio
    ) #-}

postulate
    Eq[AudioFormat[A]]   : Eq (AudioFormat A)
    Ord[AudioFormat[A]]  : Ord (AudioFormat A)
    Show[AudioFormat[A]] : Show (AudioFormat A)

{-# COMPILE GHC Eq[AudioFormat[A]]   = AgdaEq   #-}
{-# COMPILE GHC Ord[AudioFormat[A]]  = AgdaOrd  #-}
{-# COMPILE GHC Show[AudioFormat[A]] = AgdaShow #-}


record OpenDeviceSpec : Set₁ where
    constructor mkOpenDeviceSpec
    field
        {SampleType} : Set
        openDeviceFreq : Changeable CInt
        openDeviceFormat : Changeable (AudioFormat SampleType)
        openDeviceChannels : Changeable Channels
        openDeviceSamples : Word16
        openDeviceCallback : ∀{ActualSampleType : Set} → AudioFormat ActualSampleType → IOVector ActualSampleType → IO (⊤ {lzero})
        openDeviceUsage : AudioDeviceUsage
        openDeviceName : Maybe Text

{-# COMPILE GHC OpenDeviceSpec = data SDL.Audio.OpenDeviceSpec (SDL.Audio.OpenDeviceSpec) #-}


data LockState : Set where
    Locked   : LockState
    Unlocked : LockState

{-# COMPILE GHC LockState = data SDL.Audio.LockState
    ( SDL.Audio.Locked
    | SDL.Audio.Unlocked
    ) #-}

postulate
    Bounded[LockState] : Bounded LockState
    Enum[LockState]    : Bounded LockState
    Eq[LockState]      : Bounded LockState
    Data[LockState]    : Bounded LockState
    Ord[LockState]     : Bounded LockState
    Read[LockState]    : Bounded LockState
    Show[LockState]    : Bounded LockState

{-# COMPILE GHC Bounded[LockState] = AgdaBounded #-}
{-# COMPILE GHC Enum[LockState]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[LockState]      = AgdaEq      #-}
{-# COMPILE GHC Data[LockState]    = AgdaData    #-}
{-# COMPILE GHC Ord[LockState]     = AgdaOrd     #-}
{-# COMPILE GHC Read[LockState]    = AgdaRead    #-}
{-# COMPILE GHC Show[LockState]    = AgdaShow    #-}


data PlaybackState : Set where
    Pause : PlaybackState
    Play  : PlaybackState

{-# COMPILE GHC PlaybackState = SDL.Audio.PlaybackState
    ( SDL.Audio.Pause
    | SDL.Audio.Play
    ) #-}

postulate
    Bounded[PlaybackState] : Bounded PlaybackState
    Enum[PlaybackState]    : Bounded PlaybackState
    Eq[PlaybackState]      : Bounded PlaybackState
    Data[PlaybackState]    : Bounded PlaybackState
    Ord[PlaybackState]     : Bounded PlaybackState
    Read[PlaybackState]    : Bounded PlaybackState
    Show[PlaybackState]    : Bounded PlaybackState

{-# COMPILE GHC Bounded[PlaybackState] = AgdaBounded #-}
{-# COMPILE GHC Enum[PlaybackState]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[PlaybackState]      = AgdaEq      #-}
{-# COMPILE GHC Data[PlaybackState]    = AgdaData    #-}
{-# COMPILE GHC Ord[PlaybackState]     = AgdaOrd     #-}
{-# COMPILE GHC Read[PlaybackState]    = AgdaRead    #-}
{-# COMPILE GHC Show[PlaybackState]    = AgdaShow    #-}


data AudioDeviceStatus : Set where
    Playing : AudioDeviceStatus
    Paused  : AudioDeviceStatus
    Stopped : AudioDeviceStatus

{-# COMPILE GHC AudioDeviceStatus = data SDL.Audio.AudioDeviceStatus
    ( SDL.Audio.Playing
    | SDL.Audio.Paused
    | SDL.Audio.Stopped
    ) #-}

postulate
    Bounded[AudioDeviceStatus] : Bounded AudioDeviceStatus
    Enum[AudioDeviceStatus]    : Bounded AudioDeviceStatus
    Eq[AudioDeviceStatus]      : Bounded AudioDeviceStatus
    Data[AudioDeviceStatus]    : Bounded AudioDeviceStatus
    Ord[AudioDeviceStatus]     : Bounded AudioDeviceStatus
    Read[AudioDeviceStatus]    : Bounded AudioDeviceStatus
    Show[AudioDeviceStatus]    : Bounded AudioDeviceStatus

{-# COMPILE GHC Bounded[AudioDeviceStatus] = AgdaBounded #-}
{-# COMPILE GHC Enum[AudioDeviceStatus]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[AudioDeviceStatus]      = AgdaEq      #-}
{-# COMPILE GHC Data[AudioDeviceStatus]    = AgdaData    #-}
{-# COMPILE GHC Ord[AudioDeviceStatus]     = AgdaOrd     #-}
{-# COMPILE GHC Read[AudioDeviceStatus]    = AgdaRead    #-}
{-# COMPILE GHC Show[AudioDeviceStatus]    = AgdaShow    #-}


record AudioSpec : Set₁ where
    constructor mkAudioSpec
    field
        {SampleType}      : Set
        audioSpecFreq     : CInt
        audioSpecFormat   : AudioFormat SampleType
        audioSpecChannels : Channels
        audioSpecSilence  : Word8
        audioSpecSamples  : Word16
        audioSpecSize     : Word32
        audioSpecCallback : AudioFormat SampleType → IOVector SampleType → IO (⊤ {lzero})

{-# COMPILE GHC AudioSpec = data SDL.Audio.AudioSpec (SDL.Audio.AudioSpec) #-}


postulate
    openAudioDevice             : ⦃ MonadIO M ⦄ → OpenDeviceSpec → M (Liftℓ _ (Tuple2 AudioDevice AudioSpec))
    closeAudioDevice            : ⦃ MonadIO M ⦄ → AudioDevice → M ⊤
    setAudioDeviceLocked        : ⦃ MonadIO M ⦄ → AudioDevice → LockState → M ⊤
    setAudioDevicePlaybackState : ⦃ MonadIO M ⦄ → AudioDevice → PlaybackState → M ⊤
    audioDeviceStatus           : ⦃ MonadIO M ⦄ → AudioDevice → M (Liftℓ _ AudioDeviceStatus)
    getAudioDeviceNames         : ⦃ MonadIO M ⦄ → AudioDeviceUsage → M (Liftℓ _ (Maybe (Vector Text)))

{-# COMPILE GHC openAudioDevice             = \ mℓ m AgdaMonadIO -> SDL.Audio.openAudioDevice             #-}
{-# COMPILE GHC closeAudioDevice            = \ mℓ m AgdaMonadIO -> SDL.Audio.closeAudioDevice            #-}
{-# COMPILE GHC setAudioDeviceLocked        = \ mℓ m AgdaMonadIO -> SDL.Audio.setAudioDeviceLocked        #-}
{-# COMPILE GHC setAudioDevicePlaybackState = \ mℓ m AgdaMonadIO -> SDL.Audio.setAudioDevicePlaybackState #-}
{-# COMPILE GHC audioDeviceStatus           = \ mℓ m AgdaMonadIO -> SDL.Audio.audioDeviceStatus           #-}
{-# COMPILE GHC getAudioDeviceNames         = \ mℓ m AgdaMonadIO -> SDL.Audio.getAudioDeviceNames         #-}


postulate
    AudioDriver : Set
    Eq[AudioDriver]   : Eq AudioDriver
    Show[AudioDriver] : Show AudioDriver

    audioDriverName    : AudioDriver → Text
    getAudioDrivers    : ⦃ MonadIO M ⦄ → M (Liftℓ _ (Vector AudioDriver))
    currentAudioDriver : ⦃ MonadIO M ⦄ → M (Liftℓ _ (Maybe Text))

    audioInit : ⦃ MonadIO M ⦄ → AudioDriver → M ⊤

{-# COMPILE GHC AudioDriver = type SDL.Audio.AudioDriver #-}
{-# COMPILE GHC Eq[AudioDriver]   = AgdaEq   #-}
{-# COMPILE GHC Show[AudioDriver] = AgdaShow #-}

{-# COMPILE GHC audioDriverName    =                       SDL.Audio.audioDriverName    #-}
{-# COMPILE GHC getAudioDrivers    = \ mℓ m AgdaMonadIO -> SDL.Audio.getAudioDrivers    #-}
{-# COMPILE GHC currentAudioDriver = \ mℓ m AgdaMonadIO -> SDL.Audio.currentAudioDriver #-}

{-# COMPILE GHC audioInit = \ mℓ m AgdaMonadIO -> SDL.Audio.audioInit #-}
