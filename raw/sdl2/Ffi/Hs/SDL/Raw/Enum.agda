{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Raw.Enum where

open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Num; Eq)
open import Ffi.Hs.Data.Int        using (Int32)
open import Ffi.Hs.Data.Word       using (Word16; Word32)
open import Ffi.Hs.Foreign.C.Types using (CInt)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Raw.Enum
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


AudioFormat : Set
AudioFormat = Word16

postulate
    SDL-AUDIO-S8     : AudioFormat
    SDL-AUDIO-U8     : AudioFormat
    SDL-AUDIO-S16LSB : AudioFormat
    SDL-AUDIO-S16MSB : AudioFormat
    SDL-AUDIO-S16SYS : AudioFormat
    SDL-AUDIO-U16LSB : AudioFormat
    SDL-AUDIO-U16MSB : AudioFormat
    SDL-AUDIO-U16SYS : AudioFormat
    SDL-AUDIO-S32LSB : AudioFormat
    SDL-AUDIO-S32MSB : AudioFormat
    SDL-AUDIO-S32SYS : AudioFormat
    SDL-AUDIO-F32LSB : AudioFormat
    SDL-AUDIO-F32MSB : AudioFormat
    SDL-AUDIO-F32SYS : AudioFormat

{-# COMPILE GHC SDL-AUDIO-S8     = SDL.Raw.Enum.SDL_AUDIO_S8     #-}
{-# COMPILE GHC SDL-AUDIO-U8     = SDL.Raw.Enum.SDL_AUDIO_U8     #-}
{-# COMPILE GHC SDL-AUDIO-S16LSB = SDL.Raw.Enum.SDL_AUDIO_S16LSB #-}
{-# COMPILE GHC SDL-AUDIO-S16MSB = SDL.Raw.Enum.SDL_AUDIO_S16MSB #-}
{-# COMPILE GHC SDL-AUDIO-S16SYS = SDL.Raw.Enum.SDL_AUDIO_S16SYS #-}
{-# COMPILE GHC SDL-AUDIO-U16LSB = SDL.Raw.Enum.SDL_AUDIO_U16LSB #-}
{-# COMPILE GHC SDL-AUDIO-U16MSB = SDL.Raw.Enum.SDL_AUDIO_U16MSB #-}
{-# COMPILE GHC SDL-AUDIO-U16SYS = SDL.Raw.Enum.SDL_AUDIO_U16SYS #-}
{-# COMPILE GHC SDL-AUDIO-S32LSB = SDL.Raw.Enum.SDL_AUDIO_S32LSB #-}
{-# COMPILE GHC SDL-AUDIO-S32MSB = SDL.Raw.Enum.SDL_AUDIO_S32MSB #-}
{-# COMPILE GHC SDL-AUDIO-S32SYS = SDL.Raw.Enum.SDL_AUDIO_S32SYS #-}
{-# COMPILE GHC SDL-AUDIO-F32LSB = SDL.Raw.Enum.SDL_AUDIO_F32LSB #-}
{-# COMPILE GHC SDL-AUDIO-F32MSB = SDL.Raw.Enum.SDL_AUDIO_F32MSB #-}
{-# COMPILE GHC SDL-AUDIO-F32SYS = SDL.Raw.Enum.SDL_AUDIO_F32SYS #-}


AudioStatus : Set
AudioStatus = Word32

postulate
    SDL-AUDIO-STOPPED : AudioStatus
    SDL-AUDIO-PLAYING : AudioStatus
    SDL-AUDIO-PAUSED  : AudioStatus

{-# COMPILE GHC SDL-AUDIO-STOPPED = SDL.Raw.Enum.SDL_AUDIO_STOPPED #-}
{-# COMPILE GHC SDL-AUDIO-PLAYING = SDL.Raw.Enum.SDL_AUDIO_PLAYING #-}
{-# COMPILE GHC SDL-AUDIO-PAUSED  = SDL.Raw.Enum.SDL_AUDIO_PAUSED  #-}


BlendMode : Set
BlendMode = Word32

postulate
    SDL-BLENDMODE-NONE  : BlendMode
    SDL-BLENDMODE-BLEND : BlendMode
    SDL-BLENDMODE-ADD   : BlendMode
    SDL-BLENDMODE-MOD   : BlendMode

{-# COMPILE GHC SDL-BLENDMODE-NONE  = SDL.Raw.Enum.SDL_BLENDMODE_NONE  #-}
{-# COMPILE GHC SDL-BLENDMODE-BLEND = SDL.Raw.Enum.SDL_BLENDMODE_BLEND #-}
{-# COMPILE GHC SDL-BLENDMODE-ADD   = SDL.Raw.Enum.SDL_BLENDMODE_ADD   #-}
{-# COMPILE GHC SDL-BLENDMODE-MOD   = SDL.Raw.Enum.SDL_BLENDMODE_MOD   #-}


BlendOperation : Set
BlendOperation = Word32

postulate
    SDL-BLENDOPERATION-ADD          : BlendOperation
    SDL-BLENDOPERATION-SUBTRACT     : BlendOperation
    SDL-BLENDOPERATION-REV-SUBTRACT : BlendOperation
    SDL-BLENDOPERATION-MINIMUM      : BlendOperation
    SDL-BLENDOPERATION-MAXIMUM      : BlendOperation

{-# COMPILE GHC SDL-BLENDOPERATION-ADD          = SDL.Raw.Enum.SDL_BLENDOPERATION_ADD          #-}
{-# COMPILE GHC SDL-BLENDOPERATION-SUBTRACT     = SDL.Raw.Enum.SDL_BLENDOPERATION_SUBTRACT     #-}
{-# COMPILE GHC SDL-BLENDOPERATION-REV-SUBTRACT = SDL.Raw.Enum.SDL_BLENDOPERATION_REV_SUBTRACT #-}
{-# COMPILE GHC SDL-BLENDOPERATION-MINIMUM      = SDL.Raw.Enum.SDL_BLENDOPERATION_MINIMUM      #-}
{-# COMPILE GHC SDL-BLENDOPERATION-MAXIMUM      = SDL.Raw.Enum.SDL_BLENDOPERATION_MAXIMUM      #-}


BlendFactor : Set
BlendFactor = Word32

postulate
    SDL-BLENDFACTOR-ZERO                : BlendFactor
    SDL-BLENDFACTOR-ONE                 : BlendFactor
    SDL-BLENDFACTOR-SRC-COLOR           : BlendFactor
    SDL-BLENDFACTOR-ONE-MINUS-SRC-COLOR : BlendFactor
    SDL-BLENDFACTOR-SRC-ALPHA           : BlendFactor
    SDL-BLENDFACTOR-ONE-MINUS-SRC-ALPHA : BlendFactor
    SDL-BLENDFACTOR-DST-COLOR           : BlendFactor
    SDL-BLENDFACTOR-ONE-MINUS-DST-COLOR : BlendFactor
    SDL-BLENDFACTOR-DST-ALPHA           : BlendFactor
    SDL-BLENDFACTOR-ONE-MINUS-DST-ALPHA : BlendFactor

{-# COMPILE GHC SDL-BLENDFACTOR-ZERO                = SDL.Raw.Enum.SDL_BLENDFACTOR_ZERO                #-}
{-# COMPILE GHC SDL-BLENDFACTOR-ONE                 = SDL.Raw.Enum.SDL_BLENDFACTOR_ONE                 #-}
{-# COMPILE GHC SDL-BLENDFACTOR-SRC-COLOR           = SDL.Raw.Enum.SDL_BLENDFACTOR_SRC_COLOR           #-}
{-# COMPILE GHC SDL-BLENDFACTOR-ONE-MINUS-SRC-COLOR = SDL.Raw.Enum.SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR #-}
{-# COMPILE GHC SDL-BLENDFACTOR-SRC-ALPHA           = SDL.Raw.Enum.SDL_BLENDFACTOR_SRC_ALPHA           #-}
{-# COMPILE GHC SDL-BLENDFACTOR-ONE-MINUS-SRC-ALPHA = SDL.Raw.Enum.SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA #-}
{-# COMPILE GHC SDL-BLENDFACTOR-DST-COLOR           = SDL.Raw.Enum.SDL_BLENDFACTOR_DST_COLOR           #-}
{-# COMPILE GHC SDL-BLENDFACTOR-ONE-MINUS-DST-COLOR = SDL.Raw.Enum.SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR #-}
{-# COMPILE GHC SDL-BLENDFACTOR-DST-ALPHA           = SDL.Raw.Enum.SDL_BLENDFACTOR_DST_ALPHA           #-}
{-# COMPILE GHC SDL-BLENDFACTOR-ONE-MINUS-DST-ALPHA = SDL.Raw.Enum.SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA #-}


Endian : Set
Endian = CInt

postulate
    SDL-BYTEORDER  : Endian
    SDL-LIL-ENDIAN : Endian
    SDL-BIG-ENDIAN : Endian

{-# COMPILE GHC SDL-BYTEORDER  = SDL.Raw.Enum.SDL_BYTEORDER  #-}
{-# COMPILE GHC SDL-LIL-ENDIAN = SDL.Raw.Enum.SDL_LIL_ENDIAN #-}
{-# COMPILE GHC SDL-BIG-ENDIAN = SDL.Raw.Enum.SDL_BIG_ENDIAN #-}


EventAction : Set
EventAction = Word32

postulate
    SDL-ADDEVENT  : EventAction
    SDL-PEEKEVENT : EventAction
    SDL-GETEVENT  : EventAction

{-# COMPILE GHC SDL-ADDEVENT  = SDL.Raw.Enum.SDL_ADDEVENT  #-}
{-# COMPILE GHC SDL-PEEKEVENT = SDL.Raw.Enum.SDL_PEEKEVENT #-}
{-# COMPILE GHC SDL-GETEVENT  = SDL.Raw.Enum.SDL_GETEVENT  #-}


GameControllerAxis : Set
GameControllerAxis = Int32

postulate
    SDL-CONTROLLER-AXIS-INVALID      : GameControllerAxis
    SDL-CONTROLLER-AXIS-LEFTX        : GameControllerAxis
    SDL-CONTROLLER-AXIS-LEFTY        : GameControllerAxis
    SDL-CONTROLLER-AXIS-RIGHTX       : GameControllerAxis
    SDL-CONTROLLER-AXIS-RIGHTY       : GameControllerAxis
    SDL-CONTROLLER-AXIS-TRIGGERLEFT  : GameControllerAxis
    SDL-CONTROLLER-AXIS-TRIGGERRIGHT : GameControllerAxis
    SDL-CONTROLLER-AXIS-MAX          : GameControllerAxis

{-# COMPILE GHC SDL-CONTROLLER-AXIS-INVALID      = SDL.Raw.Enum.SDL_CONTROLLER_AXIS_INVALID      #-}
{-# COMPILE GHC SDL-CONTROLLER-AXIS-LEFTX        = SDL.Raw.Enum.SDL_CONTROLLER_AXIS_LEFTX        #-}
{-# COMPILE GHC SDL-CONTROLLER-AXIS-LEFTY        = SDL.Raw.Enum.SDL_CONTROLLER_AXIS_LEFTY        #-}
{-# COMPILE GHC SDL-CONTROLLER-AXIS-RIGHTX       = SDL.Raw.Enum.SDL_CONTROLLER_AXIS_RIGHTX       #-}
{-# COMPILE GHC SDL-CONTROLLER-AXIS-RIGHTY       = SDL.Raw.Enum.SDL_CONTROLLER_AXIS_RIGHTY       #-}
{-# COMPILE GHC SDL-CONTROLLER-AXIS-TRIGGERLEFT  = SDL.Raw.Enum.SDL_CONTROLLER_AXIS_TRIGGERLEFT  #-}
{-# COMPILE GHC SDL-CONTROLLER-AXIS-TRIGGERRIGHT = SDL.Raw.Enum.SDL_CONTROLLER_AXIS_TRIGGERRIGHT #-}
{-# COMPILE GHC SDL-CONTROLLER-AXIS-MAX          = SDL.Raw.Enum.SDL_CONTROLLER_AXIS_MAX          #-}


GameControllerButton : Set
GameControllerButton = Int32

postulate
    SDL-CONTROLLER-BUTTON-INVALID       : GameControllerButton
    SDL-CONTROLLER-BUTTON-A             : GameControllerButton
    SDL-CONTROLLER-BUTTON-B             : GameControllerButton
    SDL-CONTROLLER-BUTTON-X             : GameControllerButton
    SDL-CONTROLLER-BUTTON-Y             : GameControllerButton
    SDL-CONTROLLER-BUTTON-BACK          : GameControllerButton
    SDL-CONTROLLER-BUTTON-GUIDE         : GameControllerButton
    SDL-CONTROLLER-BUTTON-START         : GameControllerButton
    SDL-CONTROLLER-BUTTON-LEFTSTICK     : GameControllerButton
    SDL-CONTROLLER-BUTTON-RIGHTSTICK    : GameControllerButton
    SDL-CONTROLLER-BUTTON-LEFTSHOULDER  : GameControllerButton
    SDL-CONTROLLER-BUTTON-RIGHTSHOULDER : GameControllerButton
    SDL-CONTROLLER-BUTTON-DPAD-UP       : GameControllerButton
    SDL-CONTROLLER-BUTTON-DPAD-DOWN     : GameControllerButton
    SDL-CONTROLLER-BUTTON-DPAD-LEFT     : GameControllerButton
    SDL-CONTROLLER-BUTTON-DPAD-RIGHT    : GameControllerButton
    SDL-CONTROLLER-BUTTON-MAX           : GameControllerButton

{-# COMPILE GHC SDL-CONTROLLER-BUTTON-INVALID       = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_INVALID       #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-A             = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_A             #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-B             = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_B             #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-X             = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_X             #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-Y             = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_Y             #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-BACK          = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_BACK          #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-GUIDE         = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_GUIDE         #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-START         = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_START         #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-LEFTSTICK     = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_LEFTSTICK     #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-RIGHTSTICK    = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_RIGHTSTICK    #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-LEFTSHOULDER  = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_LEFTSHOULDER  #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-RIGHTSHOULDER = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-DPAD-UP       = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_DPAD_UP       #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-DPAD-DOWN     = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_DPAD_DOWN     #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-DPAD-LEFT     = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_DPAD_LEFT     #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-DPAD-RIGHT    = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_DPAD_RIGHT    #-}
{-# COMPILE GHC SDL-CONTROLLER-BUTTON-MAX           = SDL.Raw.Enum.SDL_CONTROLLER_BUTTON_MAX           #-}


GLattr : Set
GLattr = Word32

postulate
    SDL-GL-RED-SIZE                   : GLattr
    SDL-GL-GREEN-SIZE                 : GLattr
    SDL-GL-BLUE-SIZE                  : GLattr
    SDL-GL-ALPHA-SIZE                 : GLattr
    SDL-GL-BUFFER-SIZE                : GLattr
    SDL-GL-DOUBLEBUFFER               : GLattr
    SDL-GL-DEPTH-SIZE                 : GLattr
    SDL-GL-STENCIL-SIZE               : GLattr
    SDL-GL-ACCUM-RED-SIZE             : GLattr
    SDL-GL-ACCUM-GREEN-SIZE           : GLattr
    SDL-GL-ACCUM-BLUE-SIZE            : GLattr
    SDL-GL-ACCUM-ALPHA-SIZE           : GLattr
    SDL-GL-STEREO                     : GLattr
    SDL-GL-MULTISAMPLEBUFFERS         : GLattr
    SDL-GL-MULTISAMPLESAMPLES         : GLattr
    SDL-GL-ACCELERATED-VISUAL         : GLattr
    SDL-GL-RETAINED-BACKING           : GLattr
    SDL-GL-CONTEXT-MAJOR-VERSION      : GLattr
    SDL-GL-CONTEXT-MINOR-VERSION      : GLattr
    SDL-GL-CONTEXT-EGL                : GLattr
    SDL-GL-CONTEXT-FLAGS              : GLattr
    SDL-GL-CONTEXT-PROFILE-MASK       : GLattr
    SDL-GL-SHARE-WITH-CURRENT-CONTEXT : GLattr
    SDL-GL-FRAMEBUFFER-SRGB-CAPABLE   : GLattr
    SDL-GL-CONTEXT-RELEASE-BEHAVIOR   : GLattr

{-# COMPILE GHC SDL-GL-RED-SIZE                   = SDL.Raw.Enum.SDL_GL_RED_SIZE                   #-}
{-# COMPILE GHC SDL-GL-GREEN-SIZE                 = SDL.Raw.Enum.SDL_GL_GREEN_SIZE                 #-}
{-# COMPILE GHC SDL-GL-BLUE-SIZE                  = SDL.Raw.Enum.SDL_GL_BLUE_SIZE                  #-}
{-# COMPILE GHC SDL-GL-ALPHA-SIZE                 = SDL.Raw.Enum.SDL_GL_ALPHA_SIZE                 #-}
{-# COMPILE GHC SDL-GL-BUFFER-SIZE                = SDL.Raw.Enum.SDL_GL_BUFFER_SIZE                #-}
{-# COMPILE GHC SDL-GL-DOUBLEBUFFER               = SDL.Raw.Enum.SDL_GL_DOUBLEBUFFER               #-}
{-# COMPILE GHC SDL-GL-DEPTH-SIZE                 = SDL.Raw.Enum.SDL_GL_DEPTH_SIZE                 #-}
{-# COMPILE GHC SDL-GL-STENCIL-SIZE               = SDL.Raw.Enum.SDL_GL_STENCIL_SIZE               #-}
{-# COMPILE GHC SDL-GL-ACCUM-RED-SIZE             = SDL.Raw.Enum.SDL_GL_ACCUM_RED_SIZE             #-}
{-# COMPILE GHC SDL-GL-ACCUM-GREEN-SIZE           = SDL.Raw.Enum.SDL_GL_ACCUM_GREEN_SIZE           #-}
{-# COMPILE GHC SDL-GL-ACCUM-BLUE-SIZE            = SDL.Raw.Enum.SDL_GL_ACCUM_BLUE_SIZE            #-}
{-# COMPILE GHC SDL-GL-ACCUM-ALPHA-SIZE           = SDL.Raw.Enum.SDL_GL_ACCUM_ALPHA_SIZE           #-}
{-# COMPILE GHC SDL-GL-STEREO                     = SDL.Raw.Enum.SDL_GL_STEREO                     #-}
{-# COMPILE GHC SDL-GL-MULTISAMPLEBUFFERS         = SDL.Raw.Enum.SDL_GL_MULTISAMPLEBUFFERS         #-}
{-# COMPILE GHC SDL-GL-MULTISAMPLESAMPLES         = SDL.Raw.Enum.SDL_GL_MULTISAMPLESAMPLES         #-}
{-# COMPILE GHC SDL-GL-ACCELERATED-VISUAL         = SDL.Raw.Enum.SDL_GL_ACCELERATED_VISUAL         #-}
{-# COMPILE GHC SDL-GL-RETAINED-BACKING           = SDL.Raw.Enum.SDL_GL_RETAINED_BACKING           #-}
{-# COMPILE GHC SDL-GL-CONTEXT-MAJOR-VERSION      = SDL.Raw.Enum.SDL_GL_CONTEXT_MAJOR_VERSION      #-}
{-# COMPILE GHC SDL-GL-CONTEXT-MINOR-VERSION      = SDL.Raw.Enum.SDL_GL_CONTEXT_MINOR_VERSION      #-}
{-# COMPILE GHC SDL-GL-CONTEXT-EGL                = SDL.Raw.Enum.SDL_GL_CONTEXT_EGL                #-}
{-# COMPILE GHC SDL-GL-CONTEXT-FLAGS              = SDL.Raw.Enum.SDL_GL_CONTEXT_FLAGS              #-}
{-# COMPILE GHC SDL-GL-CONTEXT-PROFILE-MASK       = SDL.Raw.Enum.SDL_GL_CONTEXT_PROFILE_MASK       #-}
{-# COMPILE GHC SDL-GL-SHARE-WITH-CURRENT-CONTEXT = SDL.Raw.Enum.SDL_GL_SHARE_WITH_CURRENT_CONTEXT #-}
{-# COMPILE GHC SDL-GL-FRAMEBUFFER-SRGB-CAPABLE   = SDL.Raw.Enum.SDL_GL_FRAMEBUFFER_SRGB_CAPABLE   #-}
{-# COMPILE GHC SDL-GL-CONTEXT-RELEASE-BEHAVIOR   = SDL.Raw.Enum.SDL_GL_CONTEXT_RELEASE_BEHAVIOR   #-}


HintPriority : Set
HintPriority = Word32

postulate
    SDL-HINT-DEFAULT  : HintPriority
    SDL-HINT-NORMAL   : HintPriority
    SDL-HINT-OVERRIDE : HintPriority

{-# COMPILE GHC SDL-HINT-DEFAULT  = SDL.Raw.Enum.SDL_HINT_DEFAULT  #-}
{-# COMPILE GHC SDL-HINT-NORMAL   = SDL.Raw.Enum.SDL_HINT_NORMAL   #-}
{-# COMPILE GHC SDL-HINT-OVERRIDE = SDL.Raw.Enum.SDL_HINT_OVERRIDE #-}


InitFlag : Set
InitFlag = Word32

postulate
    SDL-INIT-TIMER          : InitFlag
    SDL-INIT-AUDIO          : InitFlag
    SDL-INIT-VIDEO          : InitFlag
    SDL-INIT-JOYSTICK       : InitFlag
    SDL-INIT-HAPTIC         : InitFlag
    SDL-INIT-GAMECONTROLLER : InitFlag
    SDL-INIT-EVENTS         : InitFlag
    SDL-INIT-NOPARACHUTE    : InitFlag
    SDL-INIT-EVERYTHING     : InitFlag

{-# COMPILE GHC SDL-INIT-TIMER          = SDL.Raw.Enum.SDL_INIT_TIMER          #-}
{-# COMPILE GHC SDL-INIT-AUDIO          = SDL.Raw.Enum.SDL_INIT_AUDIO          #-}
{-# COMPILE GHC SDL-INIT-VIDEO          = SDL.Raw.Enum.SDL_INIT_VIDEO          #-}
{-# COMPILE GHC SDL-INIT-JOYSTICK       = SDL.Raw.Enum.SDL_INIT_JOYSTICK       #-}
{-# COMPILE GHC SDL-INIT-HAPTIC         = SDL.Raw.Enum.SDL_INIT_HAPTIC         #-}
{-# COMPILE GHC SDL-INIT-GAMECONTROLLER = SDL.Raw.Enum.SDL_INIT_GAMECONTROLLER #-}
{-# COMPILE GHC SDL-INIT-EVENTS         = SDL.Raw.Enum.SDL_INIT_EVENTS         #-}
{-# COMPILE GHC SDL-INIT-NOPARACHUTE    = SDL.Raw.Enum.SDL_INIT_NOPARACHUTE    #-}
{-# COMPILE GHC SDL-INIT-EVERYTHING     = SDL.Raw.Enum.SDL_INIT_EVERYTHING     #-}


JoystickPowerLevel : Set
JoystickPowerLevel = Int32

postulate
    SDL-JOYSTICK-POWER-UNKNOWN : JoystickPowerLevel
    SDL-JOYSTICK-POWER-EMPTY   : JoystickPowerLevel
    SDL-JOYSTICK-POWER-LOW     : JoystickPowerLevel
    SDL-JOYSTICK-POWER-MEDIUM  : JoystickPowerLevel
    SDL-JOYSTICK-POWER-FULL    : JoystickPowerLevel
    SDL-JOYSTICK-POWER-WIRED   : JoystickPowerLevel
    SDL-JOYSTICK-POWER-MAX     : JoystickPowerLevel

{-# COMPILE GHC SDL-JOYSTICK-POWER-UNKNOWN = SDL.Raw.Enum.SDL_JOYSTICK_POWER_UNKNOWN #-}
{-# COMPILE GHC SDL-JOYSTICK-POWER-EMPTY   = SDL.Raw.Enum.SDL_JOYSTICK_POWER_EMPTY   #-}
{-# COMPILE GHC SDL-JOYSTICK-POWER-LOW     = SDL.Raw.Enum.SDL_JOYSTICK_POWER_LOW     #-}
{-# COMPILE GHC SDL-JOYSTICK-POWER-MEDIUM  = SDL.Raw.Enum.SDL_JOYSTICK_POWER_MEDIUM  #-}
{-# COMPILE GHC SDL-JOYSTICK-POWER-FULL    = SDL.Raw.Enum.SDL_JOYSTICK_POWER_FULL    #-}
{-# COMPILE GHC SDL-JOYSTICK-POWER-WIRED   = SDL.Raw.Enum.SDL_JOYSTICK_POWER_WIRED   #-}
{-# COMPILE GHC SDL-JOYSTICK-POWER-MAX     = SDL.Raw.Enum.SDL_JOYSTICK_POWER_MAX     #-}


Keycode : Set
Keycode = Int32

postulate
    SDLK-UNKNOWN            : Keycode
    SDLK-RETURN             : Keycode
    SDLK-ESCAPE             : Keycode
    SDLK-BACKSPACE          : Keycode
    SDLK-TAB                : Keycode
    SDLK-SPACE              : Keycode
    SDLK-EXCLAIM            : Keycode
    SDLK-QUOTEDBL           : Keycode
    SDLK-HASH               : Keycode
    SDLK-PERCENT            : Keycode
    SDLK-DOLLAR             : Keycode
    SDLK-AMPERSAND          : Keycode
    SDLK-QUOTE              : Keycode
    SDLK-LEFTPAREN          : Keycode
    SDLK-RIGHTPAREN         : Keycode
    SDLK-ASTERISK           : Keycode
    SDLK-PLUS               : Keycode
    SDLK-COMMA              : Keycode
    SDLK-MINUS              : Keycode
    SDLK-PERIOD             : Keycode
    SDLK-SLASH              : Keycode
    SDLK-0                  : Keycode
    SDLK-1                  : Keycode
    SDLK-2                  : Keycode
    SDLK-3                  : Keycode
    SDLK-4                  : Keycode
    SDLK-5                  : Keycode
    SDLK-6                  : Keycode
    SDLK-7                  : Keycode
    SDLK-8                  : Keycode
    SDLK-9                  : Keycode
    SDLK-COLON              : Keycode
    SDLK-SEMICOLON          : Keycode
    SDLK-LESS               : Keycode
    SDLK-EQUALS             : Keycode
    SDLK-GREATER            : Keycode
    SDLK-QUESTION           : Keycode
    SDLK-AT                 : Keycode
    SDLK-LEFTBRACKET        : Keycode
    SDLK-BACKSLASH          : Keycode
    SDLK-RIGHTBRACKET       : Keycode
    SDLK-CARET              : Keycode
    SDLK-UNDERSCORE         : Keycode
    SDLK-BACKQUOTE          : Keycode
    SDLK-a                  : Keycode
    SDLK-b                  : Keycode
    SDLK-c                  : Keycode
    SDLK-d                  : Keycode
    SDLK-e                  : Keycode
    SDLK-f                  : Keycode
    SDLK-g                  : Keycode
    SDLK-h                  : Keycode
    SDLK-i                  : Keycode
    SDLK-j                  : Keycode
    SDLK-k                  : Keycode
    SDLK-l                  : Keycode
    SDLK-m                  : Keycode
    SDLK-n                  : Keycode
    SDLK-o                  : Keycode
    SDLK-p                  : Keycode
    SDLK-q                  : Keycode
    SDLK-r                  : Keycode
    SDLK-s                  : Keycode
    SDLK-t                  : Keycode
    SDLK-u                  : Keycode
    SDLK-v                  : Keycode
    SDLK-w                  : Keycode
    SDLK-x                  : Keycode
    SDLK-y                  : Keycode
    SDLK-z                  : Keycode
    SDLK-CAPSLOCK           : Keycode
    SDLK-F1                 : Keycode
    SDLK-F2                 : Keycode
    SDLK-F3                 : Keycode
    SDLK-F4                 : Keycode
    SDLK-F5                 : Keycode
    SDLK-F6                 : Keycode
    SDLK-F7                 : Keycode
    SDLK-F8                 : Keycode
    SDLK-F9                 : Keycode
    SDLK-F10                : Keycode
    SDLK-F11                : Keycode
    SDLK-F12                : Keycode
    SDLK-PRINTSCREEN        : Keycode
    SDLK-SCROLLLOCK         : Keycode
    SDLK-PAUSE              : Keycode
    SDLK-INSERT             : Keycode
    SDLK-HOME               : Keycode
    SDLK-PAGEUP             : Keycode
    SDLK-DELETE             : Keycode
    SDLK-END                : Keycode
    SDLK-PAGEDOWN           : Keycode
    SDLK-RIGHT              : Keycode
    SDLK-LEFT               : Keycode
    SDLK-DOWN               : Keycode
    SDLK-UP                 : Keycode
    SDLK-NUMLOCKCLEAR       : Keycode
    SDLK-KP-DIVIDE          : Keycode
    SDLK-KP-MULTIPLY        : Keycode
    SDLK-KP-MINUS           : Keycode
    SDLK-KP-PLUS            : Keycode
    SDLK-KP-ENTER           : Keycode
    SDLK-KP-1               : Keycode
    SDLK-KP-2               : Keycode
    SDLK-KP-3               : Keycode
    SDLK-KP-4               : Keycode
    SDLK-KP-5               : Keycode
    SDLK-KP-6               : Keycode
    SDLK-KP-7               : Keycode
    SDLK-KP-8               : Keycode
    SDLK-KP-9               : Keycode
    SDLK-KP-0               : Keycode
    SDLK-KP-PERIOD          : Keycode
    SDLK-APPLICATION        : Keycode
    SDLK-POWER              : Keycode
    SDLK-KP-EQUALS          : Keycode
    SDLK-F13                : Keycode
    SDLK-F14                : Keycode
    SDLK-F15                : Keycode
    SDLK-F16                : Keycode
    SDLK-F17                : Keycode
    SDLK-F18                : Keycode
    SDLK-F19                : Keycode
    SDLK-F20                : Keycode
    SDLK-F21                : Keycode
    SDLK-F22                : Keycode
    SDLK-F23                : Keycode
    SDLK-F24                : Keycode
    SDLK-EXECUTE            : Keycode
    SDLK-HELP               : Keycode
    SDLK-MENU               : Keycode
    SDLK-SELECT             : Keycode
    SDLK-STOP               : Keycode
    SDLK-AGAIN              : Keycode
    SDLK-UNDO               : Keycode
    SDLK-CUT                : Keycode
    SDLK-COPY               : Keycode
    SDLK-PASTE              : Keycode
    SDLK-FIND               : Keycode
    SDLK-MUTE               : Keycode
    SDLK-VOLUMEUP           : Keycode
    SDLK-VOLUMEDOWN         : Keycode
    SDLK-KP-COMMA           : Keycode
    SDLK-KP-EQUALSAS400     : Keycode
    SDLK-ALTERASE           : Keycode
    SDLK-SYSREQ             : Keycode
    SDLK-CANCEL             : Keycode
    SDLK-CLEAR              : Keycode
    SDLK-PRIOR              : Keycode
    SDLK-RETURN2            : Keycode
    SDLK-SEPARATOR          : Keycode
    SDLK-OUT                : Keycode
    SDLK-OPER               : Keycode
    SDLK-CLEARAGAIN         : Keycode
    SDLK-CRSEL              : Keycode
    SDLK-EXSEL              : Keycode
    SDLK-KP-00              : Keycode
    SDLK-KP-000             : Keycode
    SDLK-THOUSANDSSEPARATOR : Keycode
    SDLK-DECIMALSEPARATOR   : Keycode
    SDLK-CURRENCYUNIT       : Keycode
    SDLK-CURRENCYSUBUNIT    : Keycode
    SDLK-KP-LEFTPAREN       : Keycode
    SDLK-KP-RIGHTPAREN      : Keycode
    SDLK-KP-LEFTBRACE       : Keycode
    SDLK-KP-RIGHTBRACE      : Keycode
    SDLK-KP-TAB             : Keycode
    SDLK-KP-BACKSPACE       : Keycode
    SDLK-KP-A               : Keycode
    SDLK-KP-B               : Keycode
    SDLK-KP-C               : Keycode
    SDLK-KP-D               : Keycode
    SDLK-KP-E               : Keycode
    SDLK-KP-F               : Keycode
    SDLK-KP-XOR             : Keycode
    SDLK-KP-POWER           : Keycode
    SDLK-KP-PERCENT         : Keycode
    SDLK-KP-LESS            : Keycode
    SDLK-KP-GREATER         : Keycode
    SDLK-KP-AMPERSAND       : Keycode
    SDLK-KP-DBLAMPERSAND    : Keycode
    SDLK-KP-VERTICALBAR     : Keycode
    SDLK-KP-DBLVERTICALBAR  : Keycode
    SDLK-KP-COLON           : Keycode
    SDLK-KP-HASH            : Keycode
    SDLK-KP-SPACE           : Keycode
    SDLK-KP-AT              : Keycode
    SDLK-KP-EXCLAM          : Keycode
    SDLK-KP-MEMSTORE        : Keycode
    SDLK-KP-MEMRECALL       : Keycode
    SDLK-KP-MEMCLEAR        : Keycode
    SDLK-KP-MEMADD          : Keycode
    SDLK-KP-MEMSUBTRACT     : Keycode
    SDLK-KP-MEMMULTIPLY     : Keycode
    SDLK-KP-MEMDIVIDE       : Keycode
    SDLK-KP-PLUSMINUS       : Keycode
    SDLK-KP-CLEAR           : Keycode
    SDLK-KP-CLEARENTRY      : Keycode
    SDLK-KP-BINARY          : Keycode
    SDLK-KP-OCTAL           : Keycode
    SDLK-KP-DECIMAL         : Keycode
    SDLK-KP-HEXADECIMAL     : Keycode
    SDLK-LCTRL              : Keycode
    SDLK-LSHIFT             : Keycode
    SDLK-LALT               : Keycode
    SDLK-LGUI               : Keycode
    SDLK-RCTRL              : Keycode
    SDLK-RSHIFT             : Keycode
    SDLK-RALT               : Keycode
    SDLK-RGUI               : Keycode
    SDLK-MODE               : Keycode
    SDLK-AUDIONEXT          : Keycode
    SDLK-AUDIOPREV          : Keycode
    SDLK-AUDIOSTOP          : Keycode
    SDLK-AUDIOPLAY          : Keycode
    SDLK-AUDIOMUTE          : Keycode
    SDLK-MEDIASELECT        : Keycode
    SDLK-WWW                : Keycode
    SDLK-MAIL               : Keycode
    SDLK-CALCULATOR         : Keycode
    SDLK-COMPUTER           : Keycode
    SDLK-AC-SEARCH          : Keycode
    SDLK-AC-HOME            : Keycode
    SDLK-AC-BACK            : Keycode
    SDLK-AC-FORWARD         : Keycode
    SDLK-AC-STOP            : Keycode
    SDLK-AC-REFRESH         : Keycode
    SDLK-AC-BOOKMARKS       : Keycode
    SDLK-BRIGHTNESSDOWN     : Keycode
    SDLK-BRIGHTNESSUP       : Keycode
    SDLK-DISPLAYSWITCH      : Keycode
    SDLK-KBDILLUMTOGGLE     : Keycode
    SDLK-KBDILLUMDOWN       : Keycode
    SDLK-KBDILLUMUP         : Keycode
    SDLK-EJECT              : Keycode
    SDLK-SLEEP              : Keycode

{-# COMPILE GHC SDLK-UNKNOWN            = SDL.Raw.Enum.SDLK_UNKNOWN            #-}
{-# COMPILE GHC SDLK-RETURN             = SDL.Raw.Enum.SDLK_RETURN             #-}
{-# COMPILE GHC SDLK-ESCAPE             = SDL.Raw.Enum.SDLK_ESCAPE             #-}
{-# COMPILE GHC SDLK-BACKSPACE          = SDL.Raw.Enum.SDLK_BACKSPACE          #-}
{-# COMPILE GHC SDLK-TAB                = SDL.Raw.Enum.SDLK_TAB                #-}
{-# COMPILE GHC SDLK-SPACE              = SDL.Raw.Enum.SDLK_SPACE              #-}
{-# COMPILE GHC SDLK-EXCLAIM            = SDL.Raw.Enum.SDLK_EXCLAIM            #-}
{-# COMPILE GHC SDLK-QUOTEDBL           = SDL.Raw.Enum.SDLK_QUOTEDBL           #-}
{-# COMPILE GHC SDLK-HASH               = SDL.Raw.Enum.SDLK_HASH               #-}
{-# COMPILE GHC SDLK-PERCENT            = SDL.Raw.Enum.SDLK_PERCENT            #-}
{-# COMPILE GHC SDLK-DOLLAR             = SDL.Raw.Enum.SDLK_DOLLAR             #-}
{-# COMPILE GHC SDLK-AMPERSAND          = SDL.Raw.Enum.SDLK_AMPERSAND          #-}
{-# COMPILE GHC SDLK-QUOTE              = SDL.Raw.Enum.SDLK_QUOTE              #-}
{-# COMPILE GHC SDLK-LEFTPAREN          = SDL.Raw.Enum.SDLK_LEFTPAREN          #-}
{-# COMPILE GHC SDLK-RIGHTPAREN         = SDL.Raw.Enum.SDLK_RIGHTPAREN         #-}
{-# COMPILE GHC SDLK-ASTERISK           = SDL.Raw.Enum.SDLK_ASTERISK           #-}
{-# COMPILE GHC SDLK-PLUS               = SDL.Raw.Enum.SDLK_PLUS               #-}
{-# COMPILE GHC SDLK-COMMA              = SDL.Raw.Enum.SDLK_COMMA              #-}
{-# COMPILE GHC SDLK-MINUS              = SDL.Raw.Enum.SDLK_MINUS              #-}
{-# COMPILE GHC SDLK-PERIOD             = SDL.Raw.Enum.SDLK_PERIOD             #-}
{-# COMPILE GHC SDLK-SLASH              = SDL.Raw.Enum.SDLK_SLASH              #-}
{-# COMPILE GHC SDLK-0                  = SDL.Raw.Enum.SDLK_0                  #-}
{-# COMPILE GHC SDLK-1                  = SDL.Raw.Enum.SDLK_1                  #-}
{-# COMPILE GHC SDLK-2                  = SDL.Raw.Enum.SDLK_2                  #-}
{-# COMPILE GHC SDLK-3                  = SDL.Raw.Enum.SDLK_3                  #-}
{-# COMPILE GHC SDLK-4                  = SDL.Raw.Enum.SDLK_4                  #-}
{-# COMPILE GHC SDLK-5                  = SDL.Raw.Enum.SDLK_5                  #-}
{-# COMPILE GHC SDLK-6                  = SDL.Raw.Enum.SDLK_6                  #-}
{-# COMPILE GHC SDLK-7                  = SDL.Raw.Enum.SDLK_7                  #-}
{-# COMPILE GHC SDLK-8                  = SDL.Raw.Enum.SDLK_8                  #-}
{-# COMPILE GHC SDLK-9                  = SDL.Raw.Enum.SDLK_9                  #-}
{-# COMPILE GHC SDLK-COLON              = SDL.Raw.Enum.SDLK_COLON              #-}
{-# COMPILE GHC SDLK-SEMICOLON          = SDL.Raw.Enum.SDLK_SEMICOLON          #-}
{-# COMPILE GHC SDLK-LESS               = SDL.Raw.Enum.SDLK_LESS               #-}
{-# COMPILE GHC SDLK-EQUALS             = SDL.Raw.Enum.SDLK_EQUALS             #-}
{-# COMPILE GHC SDLK-GREATER            = SDL.Raw.Enum.SDLK_GREATER            #-}
{-# COMPILE GHC SDLK-QUESTION           = SDL.Raw.Enum.SDLK_QUESTION           #-}
{-# COMPILE GHC SDLK-AT                 = SDL.Raw.Enum.SDLK_AT                 #-}
{-# COMPILE GHC SDLK-LEFTBRACKET        = SDL.Raw.Enum.SDLK_LEFTBRACKET        #-}
{-# COMPILE GHC SDLK-BACKSLASH          = SDL.Raw.Enum.SDLK_BACKSLASH          #-}
{-# COMPILE GHC SDLK-RIGHTBRACKET       = SDL.Raw.Enum.SDLK_RIGHTBRACKET       #-}
{-# COMPILE GHC SDLK-CARET              = SDL.Raw.Enum.SDLK_CARET              #-}
{-# COMPILE GHC SDLK-UNDERSCORE         = SDL.Raw.Enum.SDLK_UNDERSCORE         #-}
{-# COMPILE GHC SDLK-BACKQUOTE          = SDL.Raw.Enum.SDLK_BACKQUOTE          #-}
{-# COMPILE GHC SDLK-a                  = SDL.Raw.Enum.SDLK_a                  #-}
{-# COMPILE GHC SDLK-b                  = SDL.Raw.Enum.SDLK_b                  #-}
{-# COMPILE GHC SDLK-c                  = SDL.Raw.Enum.SDLK_c                  #-}
{-# COMPILE GHC SDLK-d                  = SDL.Raw.Enum.SDLK_d                  #-}
{-# COMPILE GHC SDLK-e                  = SDL.Raw.Enum.SDLK_e                  #-}
{-# COMPILE GHC SDLK-f                  = SDL.Raw.Enum.SDLK_f                  #-}
{-# COMPILE GHC SDLK-g                  = SDL.Raw.Enum.SDLK_g                  #-}
{-# COMPILE GHC SDLK-h                  = SDL.Raw.Enum.SDLK_h                  #-}
{-# COMPILE GHC SDLK-i                  = SDL.Raw.Enum.SDLK_i                  #-}
{-# COMPILE GHC SDLK-j                  = SDL.Raw.Enum.SDLK_j                  #-}
{-# COMPILE GHC SDLK-k                  = SDL.Raw.Enum.SDLK_k                  #-}
{-# COMPILE GHC SDLK-l                  = SDL.Raw.Enum.SDLK_l                  #-}
{-# COMPILE GHC SDLK-m                  = SDL.Raw.Enum.SDLK_m                  #-}
{-# COMPILE GHC SDLK-n                  = SDL.Raw.Enum.SDLK_n                  #-}
{-# COMPILE GHC SDLK-o                  = SDL.Raw.Enum.SDLK_o                  #-}
{-# COMPILE GHC SDLK-p                  = SDL.Raw.Enum.SDLK_p                  #-}
{-# COMPILE GHC SDLK-q                  = SDL.Raw.Enum.SDLK_q                  #-}
{-# COMPILE GHC SDLK-r                  = SDL.Raw.Enum.SDLK_r                  #-}
{-# COMPILE GHC SDLK-s                  = SDL.Raw.Enum.SDLK_s                  #-}
{-# COMPILE GHC SDLK-t                  = SDL.Raw.Enum.SDLK_t                  #-}
{-# COMPILE GHC SDLK-u                  = SDL.Raw.Enum.SDLK_u                  #-}
{-# COMPILE GHC SDLK-v                  = SDL.Raw.Enum.SDLK_v                  #-}
{-# COMPILE GHC SDLK-w                  = SDL.Raw.Enum.SDLK_w                  #-}
{-# COMPILE GHC SDLK-x                  = SDL.Raw.Enum.SDLK_x                  #-}
{-# COMPILE GHC SDLK-y                  = SDL.Raw.Enum.SDLK_y                  #-}
{-# COMPILE GHC SDLK-z                  = SDL.Raw.Enum.SDLK_z                  #-}
{-# COMPILE GHC SDLK-CAPSLOCK           = SDL.Raw.Enum.SDLK_CAPSLOCK           #-}
{-# COMPILE GHC SDLK-F1                 = SDL.Raw.Enum.SDLK_F1                 #-}
{-# COMPILE GHC SDLK-F2                 = SDL.Raw.Enum.SDLK_F2                 #-}
{-# COMPILE GHC SDLK-F3                 = SDL.Raw.Enum.SDLK_F3                 #-}
{-# COMPILE GHC SDLK-F4                 = SDL.Raw.Enum.SDLK_F4                 #-}
{-# COMPILE GHC SDLK-F5                 = SDL.Raw.Enum.SDLK_F5                 #-}
{-# COMPILE GHC SDLK-F6                 = SDL.Raw.Enum.SDLK_F6                 #-}
{-# COMPILE GHC SDLK-F7                 = SDL.Raw.Enum.SDLK_F7                 #-}
{-# COMPILE GHC SDLK-F8                 = SDL.Raw.Enum.SDLK_F8                 #-}
{-# COMPILE GHC SDLK-F9                 = SDL.Raw.Enum.SDLK_F9                 #-}
{-# COMPILE GHC SDLK-F10                = SDL.Raw.Enum.SDLK_F10                #-}
{-# COMPILE GHC SDLK-F11                = SDL.Raw.Enum.SDLK_F11                #-}
{-# COMPILE GHC SDLK-F12                = SDL.Raw.Enum.SDLK_F12                #-}
{-# COMPILE GHC SDLK-PRINTSCREEN        = SDL.Raw.Enum.SDLK_PRINTSCREEN        #-}
{-# COMPILE GHC SDLK-SCROLLLOCK         = SDL.Raw.Enum.SDLK_SCROLLLOCK         #-}
{-# COMPILE GHC SDLK-PAUSE              = SDL.Raw.Enum.SDLK_PAUSE              #-}
{-# COMPILE GHC SDLK-INSERT             = SDL.Raw.Enum.SDLK_INSERT             #-}
{-# COMPILE GHC SDLK-HOME               = SDL.Raw.Enum.SDLK_HOME               #-}
{-# COMPILE GHC SDLK-PAGEUP             = SDL.Raw.Enum.SDLK_PAGEUP             #-}
{-# COMPILE GHC SDLK-DELETE             = SDL.Raw.Enum.SDLK_DELETE             #-}
{-# COMPILE GHC SDLK-END                = SDL.Raw.Enum.SDLK_END                #-}
{-# COMPILE GHC SDLK-PAGEDOWN           = SDL.Raw.Enum.SDLK_PAGEDOWN           #-}
{-# COMPILE GHC SDLK-RIGHT              = SDL.Raw.Enum.SDLK_RIGHT              #-}
{-# COMPILE GHC SDLK-LEFT               = SDL.Raw.Enum.SDLK_LEFT               #-}
{-# COMPILE GHC SDLK-DOWN               = SDL.Raw.Enum.SDLK_DOWN               #-}
{-# COMPILE GHC SDLK-UP                 = SDL.Raw.Enum.SDLK_UP                 #-}
{-# COMPILE GHC SDLK-NUMLOCKCLEAR       = SDL.Raw.Enum.SDLK_NUMLOCKCLEAR       #-}
{-# COMPILE GHC SDLK-KP-DIVIDE          = SDL.Raw.Enum.SDLK_KP_DIVIDE          #-}
{-# COMPILE GHC SDLK-KP-MULTIPLY        = SDL.Raw.Enum.SDLK_KP_MULTIPLY        #-}
{-# COMPILE GHC SDLK-KP-MINUS           = SDL.Raw.Enum.SDLK_KP_MINUS           #-}
{-# COMPILE GHC SDLK-KP-PLUS            = SDL.Raw.Enum.SDLK_KP_PLUS            #-}
{-# COMPILE GHC SDLK-KP-ENTER           = SDL.Raw.Enum.SDLK_KP_ENTER           #-}
{-# COMPILE GHC SDLK-KP-1               = SDL.Raw.Enum.SDLK_KP_1               #-}
{-# COMPILE GHC SDLK-KP-2               = SDL.Raw.Enum.SDLK_KP_2               #-}
{-# COMPILE GHC SDLK-KP-3               = SDL.Raw.Enum.SDLK_KP_3               #-}
{-# COMPILE GHC SDLK-KP-4               = SDL.Raw.Enum.SDLK_KP_4               #-}
{-# COMPILE GHC SDLK-KP-5               = SDL.Raw.Enum.SDLK_KP_5               #-}
{-# COMPILE GHC SDLK-KP-6               = SDL.Raw.Enum.SDLK_KP_6               #-}
{-# COMPILE GHC SDLK-KP-7               = SDL.Raw.Enum.SDLK_KP_7               #-}
{-# COMPILE GHC SDLK-KP-8               = SDL.Raw.Enum.SDLK_KP_8               #-}
{-# COMPILE GHC SDLK-KP-9               = SDL.Raw.Enum.SDLK_KP_9               #-}
{-# COMPILE GHC SDLK-KP-0               = SDL.Raw.Enum.SDLK_KP_0               #-}
{-# COMPILE GHC SDLK-KP-PERIOD          = SDL.Raw.Enum.SDLK_KP_PERIOD          #-}
{-# COMPILE GHC SDLK-APPLICATION        = SDL.Raw.Enum.SDLK_APPLICATION        #-}
{-# COMPILE GHC SDLK-POWER              = SDL.Raw.Enum.SDLK_POWER              #-}
{-# COMPILE GHC SDLK-KP-EQUALS          = SDL.Raw.Enum.SDLK_KP_EQUALS          #-}
{-# COMPILE GHC SDLK-F13                = SDL.Raw.Enum.SDLK_F13                #-}
{-# COMPILE GHC SDLK-F14                = SDL.Raw.Enum.SDLK_F14                #-}
{-# COMPILE GHC SDLK-F15                = SDL.Raw.Enum.SDLK_F15                #-}
{-# COMPILE GHC SDLK-F16                = SDL.Raw.Enum.SDLK_F16                #-}
{-# COMPILE GHC SDLK-F17                = SDL.Raw.Enum.SDLK_F17                #-}
{-# COMPILE GHC SDLK-F18                = SDL.Raw.Enum.SDLK_F18                #-}
{-# COMPILE GHC SDLK-F19                = SDL.Raw.Enum.SDLK_F19                #-}
{-# COMPILE GHC SDLK-F20                = SDL.Raw.Enum.SDLK_F20                #-}
{-# COMPILE GHC SDLK-F21                = SDL.Raw.Enum.SDLK_F21                #-}
{-# COMPILE GHC SDLK-F22                = SDL.Raw.Enum.SDLK_F22                #-}
{-# COMPILE GHC SDLK-F23                = SDL.Raw.Enum.SDLK_F23                #-}
{-# COMPILE GHC SDLK-F24                = SDL.Raw.Enum.SDLK_F24                #-}
{-# COMPILE GHC SDLK-EXECUTE            = SDL.Raw.Enum.SDLK_EXECUTE            #-}
{-# COMPILE GHC SDLK-HELP               = SDL.Raw.Enum.SDLK_HELP               #-}
{-# COMPILE GHC SDLK-MENU               = SDL.Raw.Enum.SDLK_MENU               #-}
{-# COMPILE GHC SDLK-SELECT             = SDL.Raw.Enum.SDLK_SELECT             #-}
{-# COMPILE GHC SDLK-STOP               = SDL.Raw.Enum.SDLK_STOP               #-}
{-# COMPILE GHC SDLK-AGAIN              = SDL.Raw.Enum.SDLK_AGAIN              #-}
{-# COMPILE GHC SDLK-UNDO               = SDL.Raw.Enum.SDLK_UNDO               #-}
{-# COMPILE GHC SDLK-CUT                = SDL.Raw.Enum.SDLK_CUT                #-}
{-# COMPILE GHC SDLK-COPY               = SDL.Raw.Enum.SDLK_COPY               #-}
{-# COMPILE GHC SDLK-PASTE              = SDL.Raw.Enum.SDLK_PASTE              #-}
{-# COMPILE GHC SDLK-FIND               = SDL.Raw.Enum.SDLK_FIND               #-}
{-# COMPILE GHC SDLK-MUTE               = SDL.Raw.Enum.SDLK_MUTE               #-}
{-# COMPILE GHC SDLK-VOLUMEUP           = SDL.Raw.Enum.SDLK_VOLUMEUP           #-}
{-# COMPILE GHC SDLK-VOLUMEDOWN         = SDL.Raw.Enum.SDLK_VOLUMEDOWN         #-}
{-# COMPILE GHC SDLK-KP-COMMA           = SDL.Raw.Enum.SDLK_KP_COMMA           #-}
{-# COMPILE GHC SDLK-KP-EQUALSAS400     = SDL.Raw.Enum.SDLK_KP_EQUALSAS400     #-}
{-# COMPILE GHC SDLK-ALTERASE           = SDL.Raw.Enum.SDLK_ALTERASE           #-}
{-# COMPILE GHC SDLK-SYSREQ             = SDL.Raw.Enum.SDLK_SYSREQ             #-}
{-# COMPILE GHC SDLK-CANCEL             = SDL.Raw.Enum.SDLK_CANCEL             #-}
{-# COMPILE GHC SDLK-CLEAR              = SDL.Raw.Enum.SDLK_CLEAR              #-}
{-# COMPILE GHC SDLK-PRIOR              = SDL.Raw.Enum.SDLK_PRIOR              #-}
{-# COMPILE GHC SDLK-RETURN2            = SDL.Raw.Enum.SDLK_RETURN2            #-}
{-# COMPILE GHC SDLK-SEPARATOR          = SDL.Raw.Enum.SDLK_SEPARATOR          #-}
{-# COMPILE GHC SDLK-OUT                = SDL.Raw.Enum.SDLK_OUT                #-}
{-# COMPILE GHC SDLK-OPER               = SDL.Raw.Enum.SDLK_OPER               #-}
{-# COMPILE GHC SDLK-CLEARAGAIN         = SDL.Raw.Enum.SDLK_CLEARAGAIN         #-}
{-# COMPILE GHC SDLK-CRSEL              = SDL.Raw.Enum.SDLK_CRSEL              #-}
{-# COMPILE GHC SDLK-EXSEL              = SDL.Raw.Enum.SDLK_EXSEL              #-}
{-# COMPILE GHC SDLK-KP-00              = SDL.Raw.Enum.SDLK_KP_00              #-}
{-# COMPILE GHC SDLK-KP-000             = SDL.Raw.Enum.SDLK_KP_000             #-}
{-# COMPILE GHC SDLK-THOUSANDSSEPARATOR = SDL.Raw.Enum.SDLK_THOUSANDSSEPARATOR #-}
{-# COMPILE GHC SDLK-DECIMALSEPARATOR   = SDL.Raw.Enum.SDLK_DECIMALSEPARATOR   #-}
{-# COMPILE GHC SDLK-CURRENCYUNIT       = SDL.Raw.Enum.SDLK_CURRENCYUNIT       #-}
{-# COMPILE GHC SDLK-CURRENCYSUBUNIT    = SDL.Raw.Enum.SDLK_CURRENCYSUBUNIT    #-}
{-# COMPILE GHC SDLK-KP-LEFTPAREN       = SDL.Raw.Enum.SDLK_KP_LEFTPAREN       #-}
{-# COMPILE GHC SDLK-KP-RIGHTPAREN      = SDL.Raw.Enum.SDLK_KP_RIGHTPAREN      #-}
{-# COMPILE GHC SDLK-KP-LEFTBRACE       = SDL.Raw.Enum.SDLK_KP_LEFTBRACE       #-}
{-# COMPILE GHC SDLK-KP-RIGHTBRACE      = SDL.Raw.Enum.SDLK_KP_RIGHTBRACE      #-}
{-# COMPILE GHC SDLK-KP-TAB             = SDL.Raw.Enum.SDLK_KP_TAB             #-}
{-# COMPILE GHC SDLK-KP-BACKSPACE       = SDL.Raw.Enum.SDLK_KP_BACKSPACE       #-}
{-# COMPILE GHC SDLK-KP-A               = SDL.Raw.Enum.SDLK_KP_A               #-}
{-# COMPILE GHC SDLK-KP-B               = SDL.Raw.Enum.SDLK_KP_B               #-}
{-# COMPILE GHC SDLK-KP-C               = SDL.Raw.Enum.SDLK_KP_C               #-}
{-# COMPILE GHC SDLK-KP-D               = SDL.Raw.Enum.SDLK_KP_D               #-}
{-# COMPILE GHC SDLK-KP-E               = SDL.Raw.Enum.SDLK_KP_E               #-}
{-# COMPILE GHC SDLK-KP-F               = SDL.Raw.Enum.SDLK_KP_F               #-}
{-# COMPILE GHC SDLK-KP-XOR             = SDL.Raw.Enum.SDLK_KP_XOR             #-}
{-# COMPILE GHC SDLK-KP-POWER           = SDL.Raw.Enum.SDLK_KP_POWER           #-}
{-# COMPILE GHC SDLK-KP-PERCENT         = SDL.Raw.Enum.SDLK_KP_PERCENT         #-}
{-# COMPILE GHC SDLK-KP-LESS            = SDL.Raw.Enum.SDLK_KP_LESS            #-}
{-# COMPILE GHC SDLK-KP-GREATER         = SDL.Raw.Enum.SDLK_KP_GREATER         #-}
{-# COMPILE GHC SDLK-KP-AMPERSAND       = SDL.Raw.Enum.SDLK_KP_AMPERSAND       #-}
{-# COMPILE GHC SDLK-KP-DBLAMPERSAND    = SDL.Raw.Enum.SDLK_KP_DBLAMPERSAND    #-}
{-# COMPILE GHC SDLK-KP-VERTICALBAR     = SDL.Raw.Enum.SDLK_KP_VERTICALBAR     #-}
{-# COMPILE GHC SDLK-KP-DBLVERTICALBAR  = SDL.Raw.Enum.SDLK_KP_DBLVERTICALBAR  #-}
{-# COMPILE GHC SDLK-KP-COLON           = SDL.Raw.Enum.SDLK_KP_COLON           #-}
{-# COMPILE GHC SDLK-KP-HASH            = SDL.Raw.Enum.SDLK_KP_HASH            #-}
{-# COMPILE GHC SDLK-KP-SPACE           = SDL.Raw.Enum.SDLK_KP_SPACE           #-}
{-# COMPILE GHC SDLK-KP-AT              = SDL.Raw.Enum.SDLK_KP_AT              #-}
{-# COMPILE GHC SDLK-KP-EXCLAM          = SDL.Raw.Enum.SDLK_KP_EXCLAM          #-}
{-# COMPILE GHC SDLK-KP-MEMSTORE        = SDL.Raw.Enum.SDLK_KP_MEMSTORE        #-}
{-# COMPILE GHC SDLK-KP-MEMRECALL       = SDL.Raw.Enum.SDLK_KP_MEMRECALL       #-}
{-# COMPILE GHC SDLK-KP-MEMCLEAR        = SDL.Raw.Enum.SDLK_KP_MEMCLEAR        #-}
{-# COMPILE GHC SDLK-KP-MEMADD          = SDL.Raw.Enum.SDLK_KP_MEMADD          #-}
{-# COMPILE GHC SDLK-KP-MEMSUBTRACT     = SDL.Raw.Enum.SDLK_KP_MEMSUBTRACT     #-}
{-# COMPILE GHC SDLK-KP-MEMMULTIPLY     = SDL.Raw.Enum.SDLK_KP_MEMMULTIPLY     #-}
{-# COMPILE GHC SDLK-KP-MEMDIVIDE       = SDL.Raw.Enum.SDLK_KP_MEMDIVIDE       #-}
{-# COMPILE GHC SDLK-KP-PLUSMINUS       = SDL.Raw.Enum.SDLK_KP_PLUSMINUS       #-}
{-# COMPILE GHC SDLK-KP-CLEAR           = SDL.Raw.Enum.SDLK_KP_CLEAR           #-}
{-# COMPILE GHC SDLK-KP-CLEARENTRY      = SDL.Raw.Enum.SDLK_KP_CLEARENTRY      #-}
{-# COMPILE GHC SDLK-KP-BINARY          = SDL.Raw.Enum.SDLK_KP_BINARY          #-}
{-# COMPILE GHC SDLK-KP-OCTAL           = SDL.Raw.Enum.SDLK_KP_OCTAL           #-}
{-# COMPILE GHC SDLK-KP-DECIMAL         = SDL.Raw.Enum.SDLK_KP_DECIMAL         #-}
{-# COMPILE GHC SDLK-KP-HEXADECIMAL     = SDL.Raw.Enum.SDLK_KP_HEXADECIMAL     #-}
{-# COMPILE GHC SDLK-LCTRL              = SDL.Raw.Enum.SDLK_LCTRL              #-}
{-# COMPILE GHC SDLK-LSHIFT             = SDL.Raw.Enum.SDLK_LSHIFT             #-}
{-# COMPILE GHC SDLK-LALT               = SDL.Raw.Enum.SDLK_LALT               #-}
{-# COMPILE GHC SDLK-LGUI               = SDL.Raw.Enum.SDLK_LGUI               #-}
{-# COMPILE GHC SDLK-RCTRL              = SDL.Raw.Enum.SDLK_RCTRL              #-}
{-# COMPILE GHC SDLK-RSHIFT             = SDL.Raw.Enum.SDLK_RSHIFT             #-}
{-# COMPILE GHC SDLK-RALT               = SDL.Raw.Enum.SDLK_RALT               #-}
{-# COMPILE GHC SDLK-RGUI               = SDL.Raw.Enum.SDLK_RGUI               #-}
{-# COMPILE GHC SDLK-MODE               = SDL.Raw.Enum.SDLK_MODE               #-}
{-# COMPILE GHC SDLK-AUDIONEXT          = SDL.Raw.Enum.SDLK_AUDIONEXT          #-}
{-# COMPILE GHC SDLK-AUDIOPREV          = SDL.Raw.Enum.SDLK_AUDIOPREV          #-}
{-# COMPILE GHC SDLK-AUDIOSTOP          = SDL.Raw.Enum.SDLK_AUDIOSTOP          #-}
{-# COMPILE GHC SDLK-AUDIOPLAY          = SDL.Raw.Enum.SDLK_AUDIOPLAY          #-}
{-# COMPILE GHC SDLK-AUDIOMUTE          = SDL.Raw.Enum.SDLK_AUDIOMUTE          #-}
{-# COMPILE GHC SDLK-MEDIASELECT        = SDL.Raw.Enum.SDLK_MEDIASELECT        #-}
{-# COMPILE GHC SDLK-WWW                = SDL.Raw.Enum.SDLK_WWW                #-}
{-# COMPILE GHC SDLK-MAIL               = SDL.Raw.Enum.SDLK_MAIL               #-}
{-# COMPILE GHC SDLK-CALCULATOR         = SDL.Raw.Enum.SDLK_CALCULATOR         #-}
{-# COMPILE GHC SDLK-COMPUTER           = SDL.Raw.Enum.SDLK_COMPUTER           #-}
{-# COMPILE GHC SDLK-AC-SEARCH          = SDL.Raw.Enum.SDLK_AC_SEARCH          #-}
{-# COMPILE GHC SDLK-AC-HOME            = SDL.Raw.Enum.SDLK_AC_HOME            #-}
{-# COMPILE GHC SDLK-AC-BACK            = SDL.Raw.Enum.SDLK_AC_BACK            #-}
{-# COMPILE GHC SDLK-AC-FORWARD         = SDL.Raw.Enum.SDLK_AC_FORWARD         #-}
{-# COMPILE GHC SDLK-AC-STOP            = SDL.Raw.Enum.SDLK_AC_STOP            #-}
{-# COMPILE GHC SDLK-AC-REFRESH         = SDL.Raw.Enum.SDLK_AC_REFRESH         #-}
{-# COMPILE GHC SDLK-AC-BOOKMARKS       = SDL.Raw.Enum.SDLK_AC_BOOKMARKS       #-}
{-# COMPILE GHC SDLK-BRIGHTNESSDOWN     = SDL.Raw.Enum.SDLK_BRIGHTNESSDOWN     #-}
{-# COMPILE GHC SDLK-BRIGHTNESSUP       = SDL.Raw.Enum.SDLK_BRIGHTNESSUP       #-}
{-# COMPILE GHC SDLK-DISPLAYSWITCH      = SDL.Raw.Enum.SDLK_DISPLAYSWITCH      #-}
{-# COMPILE GHC SDLK-KBDILLUMTOGGLE     = SDL.Raw.Enum.SDLK_KBDILLUMTOGGLE     #-}
{-# COMPILE GHC SDLK-KBDILLUMDOWN       = SDL.Raw.Enum.SDLK_KBDILLUMDOWN       #-}
{-# COMPILE GHC SDLK-KBDILLUMUP         = SDL.Raw.Enum.SDLK_KBDILLUMUP         #-}
{-# COMPILE GHC SDLK-EJECT              = SDL.Raw.Enum.SDLK_EJECT              #-}
{-# COMPILE GHC SDLK-SLEEP              = SDL.Raw.Enum.SDLK_SLEEP              #-}


Keymod : Set
Keymod = Word32

postulate
    KMOD-NONE     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-LSHIFT   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-RSHIFT   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-SHIFT    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-LCTRL    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-RCTRL    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-CTRL     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-LALT     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-RALT     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-ALT      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-LGUI     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-RGUI     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-GUI      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-NUM      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-CAPS     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-MODE     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    KMOD-RESERVED : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

{-# COMPILE GHC KMOD-NONE     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_NONE     #-}
{-# COMPILE GHC KMOD-LSHIFT   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_LSHIFT   #-}
{-# COMPILE GHC KMOD-RSHIFT   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_RSHIFT   #-}
{-# COMPILE GHC KMOD-SHIFT    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_SHIFT    #-}
{-# COMPILE GHC KMOD-LCTRL    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_LCTRL    #-}
{-# COMPILE GHC KMOD-RCTRL    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_RCTRL    #-}
{-# COMPILE GHC KMOD-CTRL     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_CTRL     #-}
{-# COMPILE GHC KMOD-LALT     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_LALT     #-}
{-# COMPILE GHC KMOD-RALT     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_RALT     #-}
{-# COMPILE GHC KMOD-ALT      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_ALT      #-}
{-# COMPILE GHC KMOD-LGUI     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_LGUI     #-}
{-# COMPILE GHC KMOD-RGUI     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_RGUI     #-}
{-# COMPILE GHC KMOD-GUI      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_GUI      #-}
{-# COMPILE GHC KMOD-NUM      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_NUM      #-}
{-# COMPILE GHC KMOD-CAPS     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_CAPS     #-}
{-# COMPILE GHC KMOD-MODE     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_MODE     #-}
{-# COMPILE GHC KMOD-RESERVED = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.KMOD_RESERVED #-}


LogPriority : Set
LogPriority = Word32

postulate
    SDL-LOG-PRIORITY-VERBOSE  : LogPriority
    SDL-LOG-PRIORITY-DEBUG    : LogPriority
    SDL-LOG-PRIORITY-INFO     : LogPriority
    SDL-LOG-PRIORITY-WARN     : LogPriority
    SDL-LOG-PRIORITY-ERROR    : LogPriority
    SDL-LOG-PRIORITY-CRITICAL : LogPriority
    SDL-NUM-LOG-PRIORITIES    : LogPriority

{-# COMPILE GHC SDL-LOG-PRIORITY-VERBOSE  = SDL.Raw.Enum.SDL_LOG_PRIORITY_VERBOSE  #-}
{-# COMPILE GHC SDL-LOG-PRIORITY-DEBUG    = SDL.Raw.Enum.SDL_LOG_PRIORITY_DEBUG    #-}
{-# COMPILE GHC SDL-LOG-PRIORITY-INFO     = SDL.Raw.Enum.SDL_LOG_PRIORITY_INFO     #-}
{-# COMPILE GHC SDL-LOG-PRIORITY-WARN     = SDL.Raw.Enum.SDL_LOG_PRIORITY_WARN     #-}
{-# COMPILE GHC SDL-LOG-PRIORITY-ERROR    = SDL.Raw.Enum.SDL_LOG_PRIORITY_ERROR    #-}
{-# COMPILE GHC SDL-LOG-PRIORITY-CRITICAL = SDL.Raw.Enum.SDL_LOG_PRIORITY_CRITICAL #-}
{-# COMPILE GHC SDL-NUM-LOG-PRIORITIES    = SDL.Raw.Enum.SDL_NUM_LOG_PRIORITIES    #-}


PowerState : Set
PowerState = Word32

postulate
    SDL-POWERSTATE-UNKNOWN    : PowerState
    SDL-POWERSTATE-ON-BATTERY : PowerState
    SDL-POWERSTATE-NO-BATTERY : PowerState
    SDL-POWERSTATE-CHARGING   : PowerState
    SDL-POWERSTATE-CHARGED    : PowerState

{-# COMPILE GHC SDL-POWERSTATE-UNKNOWN    = SDL.Raw.Enum.SDL_POWERSTATE_UNKNOWN    #-}
{-# COMPILE GHC SDL-POWERSTATE-ON-BATTERY = SDL.Raw.Enum.SDL_POWERSTATE_ON_BATTERY #-}
{-# COMPILE GHC SDL-POWERSTATE-NO-BATTERY = SDL.Raw.Enum.SDL_POWERSTATE_NO_BATTERY #-}
{-# COMPILE GHC SDL-POWERSTATE-CHARGING   = SDL.Raw.Enum.SDL_POWERSTATE_CHARGING   #-}
{-# COMPILE GHC SDL-POWERSTATE-CHARGED    = SDL.Raw.Enum.SDL_POWERSTATE_CHARGED    #-}


RendererFlip : Set
RendererFlip = Word32

postulate
    SDL-FLIP-NONE       : RendererFlip
    SDL-FLIP-HORIZONTAL : RendererFlip
    SDL-FLIP-VERTICAL   : RendererFlip

{-# COMPILE GHC SDL-FLIP-NONE       = SDL.Raw.Enum.SDL_FLIP_NONE       #-}
{-# COMPILE GHC SDL-FLIP-HORIZONTAL = SDL.Raw.Enum.SDL_FLIP_HORIZONTAL #-}
{-# COMPILE GHC SDL-FLIP-VERTICAL   = SDL.Raw.Enum.SDL_FLIP_VERTICAL   #-}


Scancode : Set
Scancode = Word32

postulate
    SDL-SCANCODE-UNKNOWN            : Scancode
    SDL-SCANCODE-A                  : Scancode
    SDL-SCANCODE-B                  : Scancode
    SDL-SCANCODE-C                  : Scancode
    SDL-SCANCODE-D                  : Scancode
    SDL-SCANCODE-E                  : Scancode
    SDL-SCANCODE-F                  : Scancode
    SDL-SCANCODE-G                  : Scancode
    SDL-SCANCODE-H                  : Scancode
    SDL-SCANCODE-I                  : Scancode
    SDL-SCANCODE-J                  : Scancode
    SDL-SCANCODE-K                  : Scancode
    SDL-SCANCODE-L                  : Scancode
    SDL-SCANCODE-M                  : Scancode
    SDL-SCANCODE-N                  : Scancode
    SDL-SCANCODE-O                  : Scancode
    SDL-SCANCODE-P                  : Scancode
    SDL-SCANCODE-Q                  : Scancode
    SDL-SCANCODE-R                  : Scancode
    SDL-SCANCODE-S                  : Scancode
    SDL-SCANCODE-T                  : Scancode
    SDL-SCANCODE-U                  : Scancode
    SDL-SCANCODE-V                  : Scancode
    SDL-SCANCODE-W                  : Scancode
    SDL-SCANCODE-X                  : Scancode
    SDL-SCANCODE-Y                  : Scancode
    SDL-SCANCODE-Z                  : Scancode
    SDL-SCANCODE-1                  : Scancode
    SDL-SCANCODE-2                  : Scancode
    SDL-SCANCODE-3                  : Scancode
    SDL-SCANCODE-4                  : Scancode
    SDL-SCANCODE-5                  : Scancode
    SDL-SCANCODE-6                  : Scancode
    SDL-SCANCODE-7                  : Scancode
    SDL-SCANCODE-8                  : Scancode
    SDL-SCANCODE-9                  : Scancode
    SDL-SCANCODE-0                  : Scancode
    SDL-SCANCODE-RETURN             : Scancode
    SDL-SCANCODE-ESCAPE             : Scancode
    SDL-SCANCODE-BACKSPACE          : Scancode
    SDL-SCANCODE-TAB                : Scancode
    SDL-SCANCODE-SPACE              : Scancode
    SDL-SCANCODE-MINUS              : Scancode
    SDL-SCANCODE-EQUALS             : Scancode
    SDL-SCANCODE-LEFTBRACKET        : Scancode
    SDL-SCANCODE-RIGHTBRACKET       : Scancode
    SDL-SCANCODE-BACKSLASH          : Scancode
    SDL-SCANCODE-NONUSHASH          : Scancode
    SDL-SCANCODE-SEMICOLON          : Scancode
    SDL-SCANCODE-APOSTROPHE         : Scancode
    SDL-SCANCODE-GRAVE              : Scancode
    SDL-SCANCODE-COMMA              : Scancode
    SDL-SCANCODE-PERIOD             : Scancode
    SDL-SCANCODE-SLASH              : Scancode
    SDL-SCANCODE-CAPSLOCK           : Scancode
    SDL-SCANCODE-F1                 : Scancode
    SDL-SCANCODE-F2                 : Scancode
    SDL-SCANCODE-F3                 : Scancode
    SDL-SCANCODE-F4                 : Scancode
    SDL-SCANCODE-F5                 : Scancode
    SDL-SCANCODE-F6                 : Scancode
    SDL-SCANCODE-F7                 : Scancode
    SDL-SCANCODE-F8                 : Scancode
    SDL-SCANCODE-F9                 : Scancode
    SDL-SCANCODE-F10                : Scancode
    SDL-SCANCODE-F11                : Scancode
    SDL-SCANCODE-F12                : Scancode
    SDL-SCANCODE-PRINTSCREEN        : Scancode
    SDL-SCANCODE-SCROLLLOCK         : Scancode
    SDL-SCANCODE-PAUSE              : Scancode
    SDL-SCANCODE-INSERT             : Scancode
    SDL-SCANCODE-HOME               : Scancode
    SDL-SCANCODE-PAGEUP             : Scancode
    SDL-SCANCODE-DELETE             : Scancode
    SDL-SCANCODE-END                : Scancode
    SDL-SCANCODE-PAGEDOWN           : Scancode
    SDL-SCANCODE-RIGHT              : Scancode
    SDL-SCANCODE-LEFT               : Scancode
    SDL-SCANCODE-DOWN               : Scancode
    SDL-SCANCODE-UP                 : Scancode
    SDL-SCANCODE-NUMLOCKCLEAR       : Scancode
    SDL-SCANCODE-KP-DIVIDE          : Scancode
    SDL-SCANCODE-KP-MULTIPLY        : Scancode
    SDL-SCANCODE-KP-MINUS           : Scancode
    SDL-SCANCODE-KP-PLUS            : Scancode
    SDL-SCANCODE-KP-ENTER           : Scancode
    SDL-SCANCODE-KP-1               : Scancode
    SDL-SCANCODE-KP-2               : Scancode
    SDL-SCANCODE-KP-3               : Scancode
    SDL-SCANCODE-KP-4               : Scancode
    SDL-SCANCODE-KP-5               : Scancode
    SDL-SCANCODE-KP-6               : Scancode
    SDL-SCANCODE-KP-7               : Scancode
    SDL-SCANCODE-KP-8               : Scancode
    SDL-SCANCODE-KP-9               : Scancode
    SDL-SCANCODE-KP-0               : Scancode
    SDL-SCANCODE-KP-PERIOD          : Scancode
    SDL-SCANCODE-NONUSBACKSLASH     : Scancode
    SDL-SCANCODE-APPLICATION        : Scancode
    SDL-SCANCODE-POWER              : Scancode
    SDL-SCANCODE-KP-EQUALS          : Scancode
    SDL-SCANCODE-F13                : Scancode
    SDL-SCANCODE-F14                : Scancode
    SDL-SCANCODE-F15                : Scancode
    SDL-SCANCODE-F16                : Scancode
    SDL-SCANCODE-F17                : Scancode
    SDL-SCANCODE-F18                : Scancode
    SDL-SCANCODE-F19                : Scancode
    SDL-SCANCODE-F20                : Scancode
    SDL-SCANCODE-F21                : Scancode
    SDL-SCANCODE-F22                : Scancode
    SDL-SCANCODE-F23                : Scancode
    SDL-SCANCODE-F24                : Scancode
    SDL-SCANCODE-EXECUTE            : Scancode
    SDL-SCANCODE-HELP               : Scancode
    SDL-SCANCODE-MENU               : Scancode
    SDL-SCANCODE-SELECT             : Scancode
    SDL-SCANCODE-STOP               : Scancode
    SDL-SCANCODE-AGAIN              : Scancode
    SDL-SCANCODE-UNDO               : Scancode
    SDL-SCANCODE-CUT                : Scancode
    SDL-SCANCODE-COPY               : Scancode
    SDL-SCANCODE-PASTE              : Scancode
    SDL-SCANCODE-FIND               : Scancode
    SDL-SCANCODE-MUTE               : Scancode
    SDL-SCANCODE-VOLUMEUP           : Scancode
    SDL-SCANCODE-VOLUMEDOWN         : Scancode
    SDL-SCANCODE-KP-COMMA           : Scancode
    SDL-SCANCODE-KP-EQUALSAS400     : Scancode
    SDL-SCANCODE-INTERNATIONAL1     : Scancode
    SDL-SCANCODE-INTERNATIONAL2     : Scancode
    SDL-SCANCODE-INTERNATIONAL3     : Scancode
    SDL-SCANCODE-INTERNATIONAL4     : Scancode
    SDL-SCANCODE-INTERNATIONAL5     : Scancode
    SDL-SCANCODE-INTERNATIONAL6     : Scancode
    SDL-SCANCODE-INTERNATIONAL7     : Scancode
    SDL-SCANCODE-INTERNATIONAL8     : Scancode
    SDL-SCANCODE-INTERNATIONAL9     : Scancode
    SDL-SCANCODE-LANG1              : Scancode
    SDL-SCANCODE-LANG2              : Scancode
    SDL-SCANCODE-LANG3              : Scancode
    SDL-SCANCODE-LANG4              : Scancode
    SDL-SCANCODE-LANG5              : Scancode
    SDL-SCANCODE-LANG6              : Scancode
    SDL-SCANCODE-LANG7              : Scancode
    SDL-SCANCODE-LANG8              : Scancode
    SDL-SCANCODE-LANG9              : Scancode
    SDL-SCANCODE-ALTERASE           : Scancode
    SDL-SCANCODE-SYSREQ             : Scancode
    SDL-SCANCODE-CANCEL             : Scancode
    SDL-SCANCODE-CLEAR              : Scancode
    SDL-SCANCODE-PRIOR              : Scancode
    SDL-SCANCODE-RETURN2            : Scancode
    SDL-SCANCODE-SEPARATOR          : Scancode
    SDL-SCANCODE-OUT                : Scancode
    SDL-SCANCODE-OPER               : Scancode
    SDL-SCANCODE-CLEARAGAIN         : Scancode
    SDL-SCANCODE-CRSEL              : Scancode
    SDL-SCANCODE-EXSEL              : Scancode
    SDL-SCANCODE-KP-00              : Scancode
    SDL-SCANCODE-KP-000             : Scancode
    SDL-SCANCODE-THOUSANDSSEPARATOR : Scancode
    SDL-SCANCODE-DECIMALSEPARATOR   : Scancode
    SDL-SCANCODE-CURRENCYUNIT       : Scancode
    SDL-SCANCODE-CURRENCYSUBUNIT    : Scancode
    SDL-SCANCODE-KP-LEFTPAREN       : Scancode
    SDL-SCANCODE-KP-RIGHTPAREN      : Scancode
    SDL-SCANCODE-KP-LEFTBRACE       : Scancode
    SDL-SCANCODE-KP-RIGHTBRACE      : Scancode
    SDL-SCANCODE-KP-TAB             : Scancode
    SDL-SCANCODE-KP-BACKSPACE       : Scancode
    SDL-SCANCODE-KP-A               : Scancode
    SDL-SCANCODE-KP-B               : Scancode
    SDL-SCANCODE-KP-C               : Scancode
    SDL-SCANCODE-KP-D               : Scancode
    SDL-SCANCODE-KP-E               : Scancode
    SDL-SCANCODE-KP-F               : Scancode
    SDL-SCANCODE-KP-XOR             : Scancode
    SDL-SCANCODE-KP-POWER           : Scancode
    SDL-SCANCODE-KP-PERCENT         : Scancode
    SDL-SCANCODE-KP-LESS            : Scancode
    SDL-SCANCODE-KP-GREATER         : Scancode
    SDL-SCANCODE-KP-AMPERSAND       : Scancode
    SDL-SCANCODE-KP-DBLAMPERSAND    : Scancode
    SDL-SCANCODE-KP-VERTICALBAR     : Scancode
    SDL-SCANCODE-KP-DBLVERTICALBAR  : Scancode
    SDL-SCANCODE-KP-COLON           : Scancode
    SDL-SCANCODE-KP-HASH            : Scancode
    SDL-SCANCODE-KP-SPACE           : Scancode
    SDL-SCANCODE-KP-AT              : Scancode
    SDL-SCANCODE-KP-EXCLAM          : Scancode
    SDL-SCANCODE-KP-MEMSTORE        : Scancode
    SDL-SCANCODE-KP-MEMRECALL       : Scancode
    SDL-SCANCODE-KP-MEMCLEAR        : Scancode
    SDL-SCANCODE-KP-MEMADD          : Scancode
    SDL-SCANCODE-KP-MEMSUBTRACT     : Scancode
    SDL-SCANCODE-KP-MEMMULTIPLY     : Scancode
    SDL-SCANCODE-KP-MEMDIVIDE       : Scancode
    SDL-SCANCODE-KP-PLUSMINUS       : Scancode
    SDL-SCANCODE-KP-CLEAR           : Scancode
    SDL-SCANCODE-KP-CLEARENTRY      : Scancode
    SDL-SCANCODE-KP-BINARY          : Scancode
    SDL-SCANCODE-KP-OCTAL           : Scancode
    SDL-SCANCODE-KP-DECIMAL         : Scancode
    SDL-SCANCODE-KP-HEXADECIMAL     : Scancode
    SDL-SCANCODE-LCTRL              : Scancode
    SDL-SCANCODE-LSHIFT             : Scancode
    SDL-SCANCODE-LALT               : Scancode
    SDL-SCANCODE-LGUI               : Scancode
    SDL-SCANCODE-RCTRL              : Scancode
    SDL-SCANCODE-RSHIFT             : Scancode
    SDL-SCANCODE-RALT               : Scancode
    SDL-SCANCODE-RGUI               : Scancode
    SDL-SCANCODE-MODE               : Scancode
    SDL-SCANCODE-AUDIONEXT          : Scancode
    SDL-SCANCODE-AUDIOPREV          : Scancode
    SDL-SCANCODE-AUDIOSTOP          : Scancode
    SDL-SCANCODE-AUDIOPLAY          : Scancode
    SDL-SCANCODE-AUDIOMUTE          : Scancode
    SDL-SCANCODE-MEDIASELECT        : Scancode
    SDL-SCANCODE-WWW                : Scancode
    SDL-SCANCODE-MAIL               : Scancode
    SDL-SCANCODE-CALCULATOR         : Scancode
    SDL-SCANCODE-COMPUTER           : Scancode
    SDL-SCANCODE-AC-SEARCH          : Scancode
    SDL-SCANCODE-AC-HOME            : Scancode
    SDL-SCANCODE-AC-BACK            : Scancode
    SDL-SCANCODE-AC-FORWARD         : Scancode
    SDL-SCANCODE-AC-STOP            : Scancode
    SDL-SCANCODE-AC-REFRESH         : Scancode
    SDL-SCANCODE-AC-BOOKMARKS       : Scancode
    SDL-SCANCODE-BRIGHTNESSDOWN     : Scancode
    SDL-SCANCODE-BRIGHTNESSUP       : Scancode
    SDL-SCANCODE-DISPLAYSWITCH      : Scancode
    SDL-SCANCODE-KBDILLUMTOGGLE     : Scancode
    SDL-SCANCODE-KBDILLUMDOWN       : Scancode
    SDL-SCANCODE-KBDILLUMUP         : Scancode
    SDL-SCANCODE-EJECT              : Scancode
    SDL-SCANCODE-SLEEP              : Scancode
    SDL-SCANCODE-APP1               : Scancode
    SDL-SCANCODE-APP2               : Scancode
    SDL-NUM-SCANCODES               : Scancode

{-# COMPILE GHC SDL-SCANCODE-UNKNOWN            = SDL.Raw.Enum.SDL_SCANCODE_UNKNOWN            #-}
{-# COMPILE GHC SDL-SCANCODE-A                  = SDL.Raw.Enum.SDL_SCANCODE_A                  #-}
{-# COMPILE GHC SDL-SCANCODE-B                  = SDL.Raw.Enum.SDL_SCANCODE_B                  #-}
{-# COMPILE GHC SDL-SCANCODE-C                  = SDL.Raw.Enum.SDL_SCANCODE_C                  #-}
{-# COMPILE GHC SDL-SCANCODE-D                  = SDL.Raw.Enum.SDL_SCANCODE_D                  #-}
{-# COMPILE GHC SDL-SCANCODE-E                  = SDL.Raw.Enum.SDL_SCANCODE_E                  #-}
{-# COMPILE GHC SDL-SCANCODE-F                  = SDL.Raw.Enum.SDL_SCANCODE_F                  #-}
{-# COMPILE GHC SDL-SCANCODE-G                  = SDL.Raw.Enum.SDL_SCANCODE_G                  #-}
{-# COMPILE GHC SDL-SCANCODE-H                  = SDL.Raw.Enum.SDL_SCANCODE_H                  #-}
{-# COMPILE GHC SDL-SCANCODE-I                  = SDL.Raw.Enum.SDL_SCANCODE_I                  #-}
{-# COMPILE GHC SDL-SCANCODE-J                  = SDL.Raw.Enum.SDL_SCANCODE_J                  #-}
{-# COMPILE GHC SDL-SCANCODE-K                  = SDL.Raw.Enum.SDL_SCANCODE_K                  #-}
{-# COMPILE GHC SDL-SCANCODE-L                  = SDL.Raw.Enum.SDL_SCANCODE_L                  #-}
{-# COMPILE GHC SDL-SCANCODE-M                  = SDL.Raw.Enum.SDL_SCANCODE_M                  #-}
{-# COMPILE GHC SDL-SCANCODE-N                  = SDL.Raw.Enum.SDL_SCANCODE_N                  #-}
{-# COMPILE GHC SDL-SCANCODE-O                  = SDL.Raw.Enum.SDL_SCANCODE_O                  #-}
{-# COMPILE GHC SDL-SCANCODE-P                  = SDL.Raw.Enum.SDL_SCANCODE_P                  #-}
{-# COMPILE GHC SDL-SCANCODE-Q                  = SDL.Raw.Enum.SDL_SCANCODE_Q                  #-}
{-# COMPILE GHC SDL-SCANCODE-R                  = SDL.Raw.Enum.SDL_SCANCODE_R                  #-}
{-# COMPILE GHC SDL-SCANCODE-S                  = SDL.Raw.Enum.SDL_SCANCODE_S                  #-}
{-# COMPILE GHC SDL-SCANCODE-T                  = SDL.Raw.Enum.SDL_SCANCODE_T                  #-}
{-# COMPILE GHC SDL-SCANCODE-U                  = SDL.Raw.Enum.SDL_SCANCODE_U                  #-}
{-# COMPILE GHC SDL-SCANCODE-V                  = SDL.Raw.Enum.SDL_SCANCODE_V                  #-}
{-# COMPILE GHC SDL-SCANCODE-W                  = SDL.Raw.Enum.SDL_SCANCODE_W                  #-}
{-# COMPILE GHC SDL-SCANCODE-X                  = SDL.Raw.Enum.SDL_SCANCODE_X                  #-}
{-# COMPILE GHC SDL-SCANCODE-Y                  = SDL.Raw.Enum.SDL_SCANCODE_Y                  #-}
{-# COMPILE GHC SDL-SCANCODE-Z                  = SDL.Raw.Enum.SDL_SCANCODE_Z                  #-}
{-# COMPILE GHC SDL-SCANCODE-1                  = SDL.Raw.Enum.SDL_SCANCODE_1                  #-}
{-# COMPILE GHC SDL-SCANCODE-2                  = SDL.Raw.Enum.SDL_SCANCODE_2                  #-}
{-# COMPILE GHC SDL-SCANCODE-3                  = SDL.Raw.Enum.SDL_SCANCODE_3                  #-}
{-# COMPILE GHC SDL-SCANCODE-4                  = SDL.Raw.Enum.SDL_SCANCODE_4                  #-}
{-# COMPILE GHC SDL-SCANCODE-5                  = SDL.Raw.Enum.SDL_SCANCODE_5                  #-}
{-# COMPILE GHC SDL-SCANCODE-6                  = SDL.Raw.Enum.SDL_SCANCODE_6                  #-}
{-# COMPILE GHC SDL-SCANCODE-7                  = SDL.Raw.Enum.SDL_SCANCODE_7                  #-}
{-# COMPILE GHC SDL-SCANCODE-8                  = SDL.Raw.Enum.SDL_SCANCODE_8                  #-}
{-# COMPILE GHC SDL-SCANCODE-9                  = SDL.Raw.Enum.SDL_SCANCODE_9                  #-}
{-# COMPILE GHC SDL-SCANCODE-0                  = SDL.Raw.Enum.SDL_SCANCODE_0                  #-}
{-# COMPILE GHC SDL-SCANCODE-RETURN             = SDL.Raw.Enum.SDL_SCANCODE_RETURN             #-}
{-# COMPILE GHC SDL-SCANCODE-ESCAPE             = SDL.Raw.Enum.SDL_SCANCODE_ESCAPE             #-}
{-# COMPILE GHC SDL-SCANCODE-BACKSPACE          = SDL.Raw.Enum.SDL_SCANCODE_BACKSPACE          #-}
{-# COMPILE GHC SDL-SCANCODE-TAB                = SDL.Raw.Enum.SDL_SCANCODE_TAB                #-}
{-# COMPILE GHC SDL-SCANCODE-SPACE              = SDL.Raw.Enum.SDL_SCANCODE_SPACE              #-}
{-# COMPILE GHC SDL-SCANCODE-MINUS              = SDL.Raw.Enum.SDL_SCANCODE_MINUS              #-}
{-# COMPILE GHC SDL-SCANCODE-EQUALS             = SDL.Raw.Enum.SDL_SCANCODE_EQUALS             #-}
{-# COMPILE GHC SDL-SCANCODE-LEFTBRACKET        = SDL.Raw.Enum.SDL_SCANCODE_LEFTBRACKET        #-}
{-# COMPILE GHC SDL-SCANCODE-RIGHTBRACKET       = SDL.Raw.Enum.SDL_SCANCODE_RIGHTBRACKET       #-}
{-# COMPILE GHC SDL-SCANCODE-BACKSLASH          = SDL.Raw.Enum.SDL_SCANCODE_BACKSLASH          #-}
{-# COMPILE GHC SDL-SCANCODE-NONUSHASH          = SDL.Raw.Enum.SDL_SCANCODE_NONUSHASH          #-}
{-# COMPILE GHC SDL-SCANCODE-SEMICOLON          = SDL.Raw.Enum.SDL_SCANCODE_SEMICOLON          #-}
{-# COMPILE GHC SDL-SCANCODE-APOSTROPHE         = SDL.Raw.Enum.SDL_SCANCODE_APOSTROPHE         #-}
{-# COMPILE GHC SDL-SCANCODE-GRAVE              = SDL.Raw.Enum.SDL_SCANCODE_GRAVE              #-}
{-# COMPILE GHC SDL-SCANCODE-COMMA              = SDL.Raw.Enum.SDL_SCANCODE_COMMA              #-}
{-# COMPILE GHC SDL-SCANCODE-PERIOD             = SDL.Raw.Enum.SDL_SCANCODE_PERIOD             #-}
{-# COMPILE GHC SDL-SCANCODE-SLASH              = SDL.Raw.Enum.SDL_SCANCODE_SLASH              #-}
{-# COMPILE GHC SDL-SCANCODE-CAPSLOCK           = SDL.Raw.Enum.SDL_SCANCODE_CAPSLOCK           #-}
{-# COMPILE GHC SDL-SCANCODE-F1                 = SDL.Raw.Enum.SDL_SCANCODE_F1                 #-}
{-# COMPILE GHC SDL-SCANCODE-F2                 = SDL.Raw.Enum.SDL_SCANCODE_F2                 #-}
{-# COMPILE GHC SDL-SCANCODE-F3                 = SDL.Raw.Enum.SDL_SCANCODE_F3                 #-}
{-# COMPILE GHC SDL-SCANCODE-F4                 = SDL.Raw.Enum.SDL_SCANCODE_F4                 #-}
{-# COMPILE GHC SDL-SCANCODE-F5                 = SDL.Raw.Enum.SDL_SCANCODE_F5                 #-}
{-# COMPILE GHC SDL-SCANCODE-F6                 = SDL.Raw.Enum.SDL_SCANCODE_F6                 #-}
{-# COMPILE GHC SDL-SCANCODE-F7                 = SDL.Raw.Enum.SDL_SCANCODE_F7                 #-}
{-# COMPILE GHC SDL-SCANCODE-F8                 = SDL.Raw.Enum.SDL_SCANCODE_F8                 #-}
{-# COMPILE GHC SDL-SCANCODE-F9                 = SDL.Raw.Enum.SDL_SCANCODE_F9                 #-}
{-# COMPILE GHC SDL-SCANCODE-F10                = SDL.Raw.Enum.SDL_SCANCODE_F10                #-}
{-# COMPILE GHC SDL-SCANCODE-F11                = SDL.Raw.Enum.SDL_SCANCODE_F11                #-}
{-# COMPILE GHC SDL-SCANCODE-F12                = SDL.Raw.Enum.SDL_SCANCODE_F12                #-}
{-# COMPILE GHC SDL-SCANCODE-PRINTSCREEN        = SDL.Raw.Enum.SDL_SCANCODE_PRINTSCREEN        #-}
{-# COMPILE GHC SDL-SCANCODE-SCROLLLOCK         = SDL.Raw.Enum.SDL_SCANCODE_SCROLLLOCK         #-}
{-# COMPILE GHC SDL-SCANCODE-PAUSE              = SDL.Raw.Enum.SDL_SCANCODE_PAUSE              #-}
{-# COMPILE GHC SDL-SCANCODE-INSERT             = SDL.Raw.Enum.SDL_SCANCODE_INSERT             #-}
{-# COMPILE GHC SDL-SCANCODE-HOME               = SDL.Raw.Enum.SDL_SCANCODE_HOME               #-}
{-# COMPILE GHC SDL-SCANCODE-PAGEUP             = SDL.Raw.Enum.SDL_SCANCODE_PAGEUP             #-}
{-# COMPILE GHC SDL-SCANCODE-DELETE             = SDL.Raw.Enum.SDL_SCANCODE_DELETE             #-}
{-# COMPILE GHC SDL-SCANCODE-END                = SDL.Raw.Enum.SDL_SCANCODE_END                #-}
{-# COMPILE GHC SDL-SCANCODE-PAGEDOWN           = SDL.Raw.Enum.SDL_SCANCODE_PAGEDOWN           #-}
{-# COMPILE GHC SDL-SCANCODE-RIGHT              = SDL.Raw.Enum.SDL_SCANCODE_RIGHT              #-}
{-# COMPILE GHC SDL-SCANCODE-LEFT               = SDL.Raw.Enum.SDL_SCANCODE_LEFT               #-}
{-# COMPILE GHC SDL-SCANCODE-DOWN               = SDL.Raw.Enum.SDL_SCANCODE_DOWN               #-}
{-# COMPILE GHC SDL-SCANCODE-UP                 = SDL.Raw.Enum.SDL_SCANCODE_UP                 #-}
{-# COMPILE GHC SDL-SCANCODE-NUMLOCKCLEAR       = SDL.Raw.Enum.SDL_SCANCODE_NUMLOCKCLEAR       #-}
{-# COMPILE GHC SDL-SCANCODE-KP-DIVIDE          = SDL.Raw.Enum.SDL_SCANCODE_KP_DIVIDE          #-}
{-# COMPILE GHC SDL-SCANCODE-KP-MULTIPLY        = SDL.Raw.Enum.SDL_SCANCODE_KP_MULTIPLY        #-}
{-# COMPILE GHC SDL-SCANCODE-KP-MINUS           = SDL.Raw.Enum.SDL_SCANCODE_KP_MINUS           #-}
{-# COMPILE GHC SDL-SCANCODE-KP-PLUS            = SDL.Raw.Enum.SDL_SCANCODE_KP_PLUS            #-}
{-# COMPILE GHC SDL-SCANCODE-KP-ENTER           = SDL.Raw.Enum.SDL_SCANCODE_KP_ENTER           #-}
{-# COMPILE GHC SDL-SCANCODE-KP-1               = SDL.Raw.Enum.SDL_SCANCODE_KP_1               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-2               = SDL.Raw.Enum.SDL_SCANCODE_KP_2               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-3               = SDL.Raw.Enum.SDL_SCANCODE_KP_3               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-4               = SDL.Raw.Enum.SDL_SCANCODE_KP_4               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-5               = SDL.Raw.Enum.SDL_SCANCODE_KP_5               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-6               = SDL.Raw.Enum.SDL_SCANCODE_KP_6               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-7               = SDL.Raw.Enum.SDL_SCANCODE_KP_7               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-8               = SDL.Raw.Enum.SDL_SCANCODE_KP_8               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-9               = SDL.Raw.Enum.SDL_SCANCODE_KP_9               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-0               = SDL.Raw.Enum.SDL_SCANCODE_KP_0               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-PERIOD          = SDL.Raw.Enum.SDL_SCANCODE_KP_PERIOD          #-}
{-# COMPILE GHC SDL-SCANCODE-NONUSBACKSLASH     = SDL.Raw.Enum.SDL_SCANCODE_NONUSBACKSLASH     #-}
{-# COMPILE GHC SDL-SCANCODE-APPLICATION        = SDL.Raw.Enum.SDL_SCANCODE_APPLICATION        #-}
{-# COMPILE GHC SDL-SCANCODE-POWER              = SDL.Raw.Enum.SDL_SCANCODE_POWER              #-}
{-# COMPILE GHC SDL-SCANCODE-KP-EQUALS          = SDL.Raw.Enum.SDL_SCANCODE_KP_EQUALS          #-}
{-# COMPILE GHC SDL-SCANCODE-F13                = SDL.Raw.Enum.SDL_SCANCODE_F13                #-}
{-# COMPILE GHC SDL-SCANCODE-F14                = SDL.Raw.Enum.SDL_SCANCODE_F14                #-}
{-# COMPILE GHC SDL-SCANCODE-F15                = SDL.Raw.Enum.SDL_SCANCODE_F15                #-}
{-# COMPILE GHC SDL-SCANCODE-F16                = SDL.Raw.Enum.SDL_SCANCODE_F16                #-}
{-# COMPILE GHC SDL-SCANCODE-F17                = SDL.Raw.Enum.SDL_SCANCODE_F17                #-}
{-# COMPILE GHC SDL-SCANCODE-F18                = SDL.Raw.Enum.SDL_SCANCODE_F18                #-}
{-# COMPILE GHC SDL-SCANCODE-F19                = SDL.Raw.Enum.SDL_SCANCODE_F19                #-}
{-# COMPILE GHC SDL-SCANCODE-F20                = SDL.Raw.Enum.SDL_SCANCODE_F20                #-}
{-# COMPILE GHC SDL-SCANCODE-F21                = SDL.Raw.Enum.SDL_SCANCODE_F21                #-}
{-# COMPILE GHC SDL-SCANCODE-F22                = SDL.Raw.Enum.SDL_SCANCODE_F22                #-}
{-# COMPILE GHC SDL-SCANCODE-F23                = SDL.Raw.Enum.SDL_SCANCODE_F23                #-}
{-# COMPILE GHC SDL-SCANCODE-F24                = SDL.Raw.Enum.SDL_SCANCODE_F24                #-}
{-# COMPILE GHC SDL-SCANCODE-EXECUTE            = SDL.Raw.Enum.SDL_SCANCODE_EXECUTE            #-}
{-# COMPILE GHC SDL-SCANCODE-HELP               = SDL.Raw.Enum.SDL_SCANCODE_HELP               #-}
{-# COMPILE GHC SDL-SCANCODE-MENU               = SDL.Raw.Enum.SDL_SCANCODE_MENU               #-}
{-# COMPILE GHC SDL-SCANCODE-SELECT             = SDL.Raw.Enum.SDL_SCANCODE_SELECT             #-}
{-# COMPILE GHC SDL-SCANCODE-STOP               = SDL.Raw.Enum.SDL_SCANCODE_STOP               #-}
{-# COMPILE GHC SDL-SCANCODE-AGAIN              = SDL.Raw.Enum.SDL_SCANCODE_AGAIN              #-}
{-# COMPILE GHC SDL-SCANCODE-UNDO               = SDL.Raw.Enum.SDL_SCANCODE_UNDO               #-}
{-# COMPILE GHC SDL-SCANCODE-CUT                = SDL.Raw.Enum.SDL_SCANCODE_CUT                #-}
{-# COMPILE GHC SDL-SCANCODE-COPY               = SDL.Raw.Enum.SDL_SCANCODE_COPY               #-}
{-# COMPILE GHC SDL-SCANCODE-PASTE              = SDL.Raw.Enum.SDL_SCANCODE_PASTE              #-}
{-# COMPILE GHC SDL-SCANCODE-FIND               = SDL.Raw.Enum.SDL_SCANCODE_FIND               #-}
{-# COMPILE GHC SDL-SCANCODE-MUTE               = SDL.Raw.Enum.SDL_SCANCODE_MUTE               #-}
{-# COMPILE GHC SDL-SCANCODE-VOLUMEUP           = SDL.Raw.Enum.SDL_SCANCODE_VOLUMEUP           #-}
{-# COMPILE GHC SDL-SCANCODE-VOLUMEDOWN         = SDL.Raw.Enum.SDL_SCANCODE_VOLUMEDOWN         #-}
{-# COMPILE GHC SDL-SCANCODE-KP-COMMA           = SDL.Raw.Enum.SDL_SCANCODE_KP_COMMA           #-}
{-# COMPILE GHC SDL-SCANCODE-KP-EQUALSAS400     = SDL.Raw.Enum.SDL_SCANCODE_KP_EQUALSAS400     #-}
{-# COMPILE GHC SDL-SCANCODE-INTERNATIONAL1     = SDL.Raw.Enum.SDL_SCANCODE_INTERNATIONAL1     #-}
{-# COMPILE GHC SDL-SCANCODE-INTERNATIONAL2     = SDL.Raw.Enum.SDL_SCANCODE_INTERNATIONAL2     #-}
{-# COMPILE GHC SDL-SCANCODE-INTERNATIONAL3     = SDL.Raw.Enum.SDL_SCANCODE_INTERNATIONAL3     #-}
{-# COMPILE GHC SDL-SCANCODE-INTERNATIONAL4     = SDL.Raw.Enum.SDL_SCANCODE_INTERNATIONAL4     #-}
{-# COMPILE GHC SDL-SCANCODE-INTERNATIONAL5     = SDL.Raw.Enum.SDL_SCANCODE_INTERNATIONAL5     #-}
{-# COMPILE GHC SDL-SCANCODE-INTERNATIONAL6     = SDL.Raw.Enum.SDL_SCANCODE_INTERNATIONAL6     #-}
{-# COMPILE GHC SDL-SCANCODE-INTERNATIONAL7     = SDL.Raw.Enum.SDL_SCANCODE_INTERNATIONAL7     #-}
{-# COMPILE GHC SDL-SCANCODE-INTERNATIONAL8     = SDL.Raw.Enum.SDL_SCANCODE_INTERNATIONAL8     #-}
{-# COMPILE GHC SDL-SCANCODE-INTERNATIONAL9     = SDL.Raw.Enum.SDL_SCANCODE_INTERNATIONAL9     #-}
{-# COMPILE GHC SDL-SCANCODE-LANG1              = SDL.Raw.Enum.SDL_SCANCODE_LANG1              #-}
{-# COMPILE GHC SDL-SCANCODE-LANG2              = SDL.Raw.Enum.SDL_SCANCODE_LANG2              #-}
{-# COMPILE GHC SDL-SCANCODE-LANG3              = SDL.Raw.Enum.SDL_SCANCODE_LANG3              #-}
{-# COMPILE GHC SDL-SCANCODE-LANG4              = SDL.Raw.Enum.SDL_SCANCODE_LANG4              #-}
{-# COMPILE GHC SDL-SCANCODE-LANG5              = SDL.Raw.Enum.SDL_SCANCODE_LANG5              #-}
{-# COMPILE GHC SDL-SCANCODE-LANG6              = SDL.Raw.Enum.SDL_SCANCODE_LANG6              #-}
{-# COMPILE GHC SDL-SCANCODE-LANG7              = SDL.Raw.Enum.SDL_SCANCODE_LANG7              #-}
{-# COMPILE GHC SDL-SCANCODE-LANG8              = SDL.Raw.Enum.SDL_SCANCODE_LANG8              #-}
{-# COMPILE GHC SDL-SCANCODE-LANG9              = SDL.Raw.Enum.SDL_SCANCODE_LANG9              #-}
{-# COMPILE GHC SDL-SCANCODE-ALTERASE           = SDL.Raw.Enum.SDL_SCANCODE_ALTERASE           #-}
{-# COMPILE GHC SDL-SCANCODE-SYSREQ             = SDL.Raw.Enum.SDL_SCANCODE_SYSREQ             #-}
{-# COMPILE GHC SDL-SCANCODE-CANCEL             = SDL.Raw.Enum.SDL_SCANCODE_CANCEL             #-}
{-# COMPILE GHC SDL-SCANCODE-CLEAR              = SDL.Raw.Enum.SDL_SCANCODE_CLEAR              #-}
{-# COMPILE GHC SDL-SCANCODE-PRIOR              = SDL.Raw.Enum.SDL_SCANCODE_PRIOR              #-}
{-# COMPILE GHC SDL-SCANCODE-RETURN2            = SDL.Raw.Enum.SDL_SCANCODE_RETURN2            #-}
{-# COMPILE GHC SDL-SCANCODE-SEPARATOR          = SDL.Raw.Enum.SDL_SCANCODE_SEPARATOR          #-}
{-# COMPILE GHC SDL-SCANCODE-OUT                = SDL.Raw.Enum.SDL_SCANCODE_OUT                #-}
{-# COMPILE GHC SDL-SCANCODE-OPER               = SDL.Raw.Enum.SDL_SCANCODE_OPER               #-}
{-# COMPILE GHC SDL-SCANCODE-CLEARAGAIN         = SDL.Raw.Enum.SDL_SCANCODE_CLEARAGAIN         #-}
{-# COMPILE GHC SDL-SCANCODE-CRSEL              = SDL.Raw.Enum.SDL_SCANCODE_CRSEL              #-}
{-# COMPILE GHC SDL-SCANCODE-EXSEL              = SDL.Raw.Enum.SDL_SCANCODE_EXSEL              #-}
{-# COMPILE GHC SDL-SCANCODE-KP-00              = SDL.Raw.Enum.SDL_SCANCODE_KP_00              #-}
{-# COMPILE GHC SDL-SCANCODE-KP-000             = SDL.Raw.Enum.SDL_SCANCODE_KP_000             #-}
{-# COMPILE GHC SDL-SCANCODE-THOUSANDSSEPARATOR = SDL.Raw.Enum.SDL_SCANCODE_THOUSANDSSEPARATOR #-}
{-# COMPILE GHC SDL-SCANCODE-DECIMALSEPARATOR   = SDL.Raw.Enum.SDL_SCANCODE_DECIMALSEPARATOR   #-}
{-# COMPILE GHC SDL-SCANCODE-CURRENCYUNIT       = SDL.Raw.Enum.SDL_SCANCODE_CURRENCYUNIT       #-}
{-# COMPILE GHC SDL-SCANCODE-CURRENCYSUBUNIT    = SDL.Raw.Enum.SDL_SCANCODE_CURRENCYSUBUNIT    #-}
{-# COMPILE GHC SDL-SCANCODE-KP-LEFTPAREN       = SDL.Raw.Enum.SDL_SCANCODE_KP_LEFTPAREN       #-}
{-# COMPILE GHC SDL-SCANCODE-KP-RIGHTPAREN      = SDL.Raw.Enum.SDL_SCANCODE_KP_RIGHTPAREN      #-}
{-# COMPILE GHC SDL-SCANCODE-KP-LEFTBRACE       = SDL.Raw.Enum.SDL_SCANCODE_KP_LEFTBRACE       #-}
{-# COMPILE GHC SDL-SCANCODE-KP-RIGHTBRACE      = SDL.Raw.Enum.SDL_SCANCODE_KP_RIGHTBRACE      #-}
{-# COMPILE GHC SDL-SCANCODE-KP-TAB             = SDL.Raw.Enum.SDL_SCANCODE_KP_TAB             #-}
{-# COMPILE GHC SDL-SCANCODE-KP-BACKSPACE       = SDL.Raw.Enum.SDL_SCANCODE_KP_BACKSPACE       #-}
{-# COMPILE GHC SDL-SCANCODE-KP-A               = SDL.Raw.Enum.SDL_SCANCODE_KP_A               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-B               = SDL.Raw.Enum.SDL_SCANCODE_KP_B               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-C               = SDL.Raw.Enum.SDL_SCANCODE_KP_C               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-D               = SDL.Raw.Enum.SDL_SCANCODE_KP_D               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-E               = SDL.Raw.Enum.SDL_SCANCODE_KP_E               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-F               = SDL.Raw.Enum.SDL_SCANCODE_KP_F               #-}
{-# COMPILE GHC SDL-SCANCODE-KP-XOR             = SDL.Raw.Enum.SDL_SCANCODE_KP_XOR             #-}
{-# COMPILE GHC SDL-SCANCODE-KP-POWER           = SDL.Raw.Enum.SDL_SCANCODE_KP_POWER           #-}
{-# COMPILE GHC SDL-SCANCODE-KP-PERCENT         = SDL.Raw.Enum.SDL_SCANCODE_KP_PERCENT         #-}
{-# COMPILE GHC SDL-SCANCODE-KP-LESS            = SDL.Raw.Enum.SDL_SCANCODE_KP_LESS            #-}
{-# COMPILE GHC SDL-SCANCODE-KP-GREATER         = SDL.Raw.Enum.SDL_SCANCODE_KP_GREATER         #-}
{-# COMPILE GHC SDL-SCANCODE-KP-AMPERSAND       = SDL.Raw.Enum.SDL_SCANCODE_KP_AMPERSAND       #-}
{-# COMPILE GHC SDL-SCANCODE-KP-DBLAMPERSAND    = SDL.Raw.Enum.SDL_SCANCODE_KP_DBLAMPERSAND    #-}
{-# COMPILE GHC SDL-SCANCODE-KP-VERTICALBAR     = SDL.Raw.Enum.SDL_SCANCODE_KP_VERTICALBAR     #-}
{-# COMPILE GHC SDL-SCANCODE-KP-DBLVERTICALBAR  = SDL.Raw.Enum.SDL_SCANCODE_KP_DBLVERTICALBAR  #-}
{-# COMPILE GHC SDL-SCANCODE-KP-COLON           = SDL.Raw.Enum.SDL_SCANCODE_KP_COLON           #-}
{-# COMPILE GHC SDL-SCANCODE-KP-HASH            = SDL.Raw.Enum.SDL_SCANCODE_KP_HASH            #-}
{-# COMPILE GHC SDL-SCANCODE-KP-SPACE           = SDL.Raw.Enum.SDL_SCANCODE_KP_SPACE           #-}
{-# COMPILE GHC SDL-SCANCODE-KP-AT              = SDL.Raw.Enum.SDL_SCANCODE_KP_AT              #-}
{-# COMPILE GHC SDL-SCANCODE-KP-EXCLAM          = SDL.Raw.Enum.SDL_SCANCODE_KP_EXCLAM          #-}
{-# COMPILE GHC SDL-SCANCODE-KP-MEMSTORE        = SDL.Raw.Enum.SDL_SCANCODE_KP_MEMSTORE        #-}
{-# COMPILE GHC SDL-SCANCODE-KP-MEMRECALL       = SDL.Raw.Enum.SDL_SCANCODE_KP_MEMRECALL       #-}
{-# COMPILE GHC SDL-SCANCODE-KP-MEMCLEAR        = SDL.Raw.Enum.SDL_SCANCODE_KP_MEMCLEAR        #-}
{-# COMPILE GHC SDL-SCANCODE-KP-MEMADD          = SDL.Raw.Enum.SDL_SCANCODE_KP_MEMADD          #-}
{-# COMPILE GHC SDL-SCANCODE-KP-MEMSUBTRACT     = SDL.Raw.Enum.SDL_SCANCODE_KP_MEMSUBTRACT     #-}
{-# COMPILE GHC SDL-SCANCODE-KP-MEMMULTIPLY     = SDL.Raw.Enum.SDL_SCANCODE_KP_MEMMULTIPLY     #-}
{-# COMPILE GHC SDL-SCANCODE-KP-MEMDIVIDE       = SDL.Raw.Enum.SDL_SCANCODE_KP_MEMDIVIDE       #-}
{-# COMPILE GHC SDL-SCANCODE-KP-PLUSMINUS       = SDL.Raw.Enum.SDL_SCANCODE_KP_PLUSMINUS       #-}
{-# COMPILE GHC SDL-SCANCODE-KP-CLEAR           = SDL.Raw.Enum.SDL_SCANCODE_KP_CLEAR           #-}
{-# COMPILE GHC SDL-SCANCODE-KP-CLEARENTRY      = SDL.Raw.Enum.SDL_SCANCODE_KP_CLEARENTRY      #-}
{-# COMPILE GHC SDL-SCANCODE-KP-BINARY          = SDL.Raw.Enum.SDL_SCANCODE_KP_BINARY          #-}
{-# COMPILE GHC SDL-SCANCODE-KP-OCTAL           = SDL.Raw.Enum.SDL_SCANCODE_KP_OCTAL           #-}
{-# COMPILE GHC SDL-SCANCODE-KP-DECIMAL         = SDL.Raw.Enum.SDL_SCANCODE_KP_DECIMAL         #-}
{-# COMPILE GHC SDL-SCANCODE-KP-HEXADECIMAL     = SDL.Raw.Enum.SDL_SCANCODE_KP_HEXADECIMAL     #-}
{-# COMPILE GHC SDL-SCANCODE-LCTRL              = SDL.Raw.Enum.SDL_SCANCODE_LCTRL              #-}
{-# COMPILE GHC SDL-SCANCODE-LSHIFT             = SDL.Raw.Enum.SDL_SCANCODE_LSHIFT             #-}
{-# COMPILE GHC SDL-SCANCODE-LALT               = SDL.Raw.Enum.SDL_SCANCODE_LALT               #-}
{-# COMPILE GHC SDL-SCANCODE-LGUI               = SDL.Raw.Enum.SDL_SCANCODE_LGUI               #-}
{-# COMPILE GHC SDL-SCANCODE-RCTRL              = SDL.Raw.Enum.SDL_SCANCODE_RCTRL              #-}
{-# COMPILE GHC SDL-SCANCODE-RSHIFT             = SDL.Raw.Enum.SDL_SCANCODE_RSHIFT             #-}
{-# COMPILE GHC SDL-SCANCODE-RALT               = SDL.Raw.Enum.SDL_SCANCODE_RALT               #-}
{-# COMPILE GHC SDL-SCANCODE-RGUI               = SDL.Raw.Enum.SDL_SCANCODE_RGUI               #-}
{-# COMPILE GHC SDL-SCANCODE-MODE               = SDL.Raw.Enum.SDL_SCANCODE_MODE               #-}
{-# COMPILE GHC SDL-SCANCODE-AUDIONEXT          = SDL.Raw.Enum.SDL_SCANCODE_AUDIONEXT          #-}
{-# COMPILE GHC SDL-SCANCODE-AUDIOPREV          = SDL.Raw.Enum.SDL_SCANCODE_AUDIOPREV          #-}
{-# COMPILE GHC SDL-SCANCODE-AUDIOSTOP          = SDL.Raw.Enum.SDL_SCANCODE_AUDIOSTOP          #-}
{-# COMPILE GHC SDL-SCANCODE-AUDIOPLAY          = SDL.Raw.Enum.SDL_SCANCODE_AUDIOPLAY          #-}
{-# COMPILE GHC SDL-SCANCODE-AUDIOMUTE          = SDL.Raw.Enum.SDL_SCANCODE_AUDIOMUTE          #-}
{-# COMPILE GHC SDL-SCANCODE-MEDIASELECT        = SDL.Raw.Enum.SDL_SCANCODE_MEDIASELECT        #-}
{-# COMPILE GHC SDL-SCANCODE-WWW                = SDL.Raw.Enum.SDL_SCANCODE_WWW                #-}
{-# COMPILE GHC SDL-SCANCODE-MAIL               = SDL.Raw.Enum.SDL_SCANCODE_MAIL               #-}
{-# COMPILE GHC SDL-SCANCODE-CALCULATOR         = SDL.Raw.Enum.SDL_SCANCODE_CALCULATOR         #-}
{-# COMPILE GHC SDL-SCANCODE-COMPUTER           = SDL.Raw.Enum.SDL_SCANCODE_COMPUTER           #-}
{-# COMPILE GHC SDL-SCANCODE-AC-SEARCH          = SDL.Raw.Enum.SDL_SCANCODE_AC_SEARCH          #-}
{-# COMPILE GHC SDL-SCANCODE-AC-HOME            = SDL.Raw.Enum.SDL_SCANCODE_AC_HOME            #-}
{-# COMPILE GHC SDL-SCANCODE-AC-BACK            = SDL.Raw.Enum.SDL_SCANCODE_AC_BACK            #-}
{-# COMPILE GHC SDL-SCANCODE-AC-FORWARD         = SDL.Raw.Enum.SDL_SCANCODE_AC_FORWARD         #-}
{-# COMPILE GHC SDL-SCANCODE-AC-STOP            = SDL.Raw.Enum.SDL_SCANCODE_AC_STOP            #-}
{-# COMPILE GHC SDL-SCANCODE-AC-REFRESH         = SDL.Raw.Enum.SDL_SCANCODE_AC_REFRESH         #-}
{-# COMPILE GHC SDL-SCANCODE-AC-BOOKMARKS       = SDL.Raw.Enum.SDL_SCANCODE_AC_BOOKMARKS       #-}
{-# COMPILE GHC SDL-SCANCODE-BRIGHTNESSDOWN     = SDL.Raw.Enum.SDL_SCANCODE_BRIGHTNESSDOWN     #-}
{-# COMPILE GHC SDL-SCANCODE-BRIGHTNESSUP       = SDL.Raw.Enum.SDL_SCANCODE_BRIGHTNESSUP       #-}
{-# COMPILE GHC SDL-SCANCODE-DISPLAYSWITCH      = SDL.Raw.Enum.SDL_SCANCODE_DISPLAYSWITCH      #-}
{-# COMPILE GHC SDL-SCANCODE-KBDILLUMTOGGLE     = SDL.Raw.Enum.SDL_SCANCODE_KBDILLUMTOGGLE     #-}
{-# COMPILE GHC SDL-SCANCODE-KBDILLUMDOWN       = SDL.Raw.Enum.SDL_SCANCODE_KBDILLUMDOWN       #-}
{-# COMPILE GHC SDL-SCANCODE-KBDILLUMUP         = SDL.Raw.Enum.SDL_SCANCODE_KBDILLUMUP         #-}
{-# COMPILE GHC SDL-SCANCODE-EJECT              = SDL.Raw.Enum.SDL_SCANCODE_EJECT              #-}
{-# COMPILE GHC SDL-SCANCODE-SLEEP              = SDL.Raw.Enum.SDL_SCANCODE_SLEEP              #-}
{-# COMPILE GHC SDL-SCANCODE-APP1               = SDL.Raw.Enum.SDL_SCANCODE_APP1               #-}
{-# COMPILE GHC SDL-SCANCODE-APP2               = SDL.Raw.Enum.SDL_SCANCODE_APP2               #-}
{-# COMPILE GHC SDL-NUM-SCANCODES               = SDL.Raw.Enum.SDL_NUM_SCANCODES               #-}


SystemCursor : Set
SystemCursor = Word32

postulate
    SDL-SYSTEM-CURSOR-ARROW     : SystemCursor
    SDL-SYSTEM-CURSOR-IBEAM     : SystemCursor
    SDL-SYSTEM-CURSOR-WAIT      : SystemCursor
    SDL-SYSTEM-CURSOR-CROSSHAIR : SystemCursor
    SDL-SYSTEM-CURSOR-WAITARROW : SystemCursor
    SDL-SYSTEM-CURSOR-SIZENWSE  : SystemCursor
    SDL-SYSTEM-CURSOR-SIZENESW  : SystemCursor
    SDL-SYSTEM-CURSOR-SIZEWE    : SystemCursor
    SDL-SYSTEM-CURSOR-SIZENS    : SystemCursor
    SDL-SYSTEM-CURSOR-SIZEALL   : SystemCursor
    SDL-SYSTEM-CURSOR-NO        : SystemCursor
    SDL-SYSTEM-CURSOR-HAND      : SystemCursor
    SDL-NUM-SYSTEM-CURSORS      : SystemCursor

{-# COMPILE GHC SDL-SYSTEM-CURSOR-ARROW     = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_ARROW     #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-IBEAM     = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_IBEAM     #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-WAIT      = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_WAIT      #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-CROSSHAIR = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_CROSSHAIR #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-WAITARROW = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_WAITARROW #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-SIZENWSE  = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_SIZENWSE  #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-SIZENESW  = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_SIZENESW  #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-SIZEWE    = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_SIZEWE    #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-SIZENS    = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_SIZENS    #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-SIZEALL   = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_SIZEALL   #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-NO        = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_NO        #-}
{-# COMPILE GHC SDL-SYSTEM-CURSOR-HAND      = SDL.Raw.Enum.SDL_SYSTEM_CURSOR_HAND      #-}
{-# COMPILE GHC SDL-NUM-SYSTEM-CURSORS      = SDL.Raw.Enum.SDL_NUM_SYSTEM_CURSORS      #-}


ThreadPriority : Set
ThreadPriority = Word32

postulate
    SDL-THREAD-PRIORITY-LOW    : ThreadPriority
    SDL-THREAD-PRIORITY-NORMAL : ThreadPriority
    SDL-THREAD-PRIORITY-HIGH   : ThreadPriority

{-# COMPILE GHC SDL-THREAD-PRIORITY-LOW    = SDL.Raw.Enum.SDL_THREAD_PRIORITY_LOW    #-}
{-# COMPILE GHC SDL-THREAD-PRIORITY-NORMAL = SDL.Raw.Enum.SDL_THREAD_PRIORITY_NORMAL #-}
{-# COMPILE GHC SDL-THREAD-PRIORITY-HIGH   = SDL.Raw.Enum.SDL_THREAD_PRIORITY_HIGH   #-}

postulate
    SDL-AUDIO-ALLOW-FREQUENCY-CHANGE : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-AUDIO-ALLOW-FORMAT-CHANGE    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-AUDIO-ALLOW-CHANNELS-CHANGE  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-AUDIO-ALLOW-ANY-CHANGE       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-BUTTON-LEFT   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-BUTTON-MIDDLE : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-BUTTON-RIGHT  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-BUTTON-X1     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-BUTTON-X2     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-BUTTON-LMASK  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-BUTTON-MMASK  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-BUTTON-RMASK  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-BUTTON-X1MASK : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-BUTTON-X2MASK : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-MOUSEWHEEL-NORMAL  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-MOUSEWHEEL-FLIPPED : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-FIRSTEVENT               : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-QUIT                     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-APP-TERMINATING          : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-APP-LOWMEMORY            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-APP-WILLENTERBACKGROUND  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-APP-DIDENTERBACKGROUND   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-APP-WILLENTERFOREGROUND  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-APP-DIDENTERFOREGROUND   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT              : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-SYSWMEVENT               : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-KEYDOWN                  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-KEYUP                    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-TEXTEDITING              : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-TEXTINPUT                : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-KEYMAPCHANGED            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-MOUSEMOTION              : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-MOUSEBUTTONDOWN          : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-MOUSEBUTTONUP            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-MOUSEWHEEL               : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-JOYAXISMOTION            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-JOYBALLMOTION            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-JOYHATMOTION             : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-JOYBUTTONDOWN            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-JOYBUTTONUP              : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-JOYDEVICEADDED           : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-JOYDEVICEREMOVED         : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-CONTROLLERAXISMOTION     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-CONTROLLERBUTTONDOWN     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-CONTROLLERBUTTONUP       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-CONTROLLERDEVICEADDED    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-CONTROLLERDEVICEREMOVED  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-CONTROLLERDEVICEREMAPPED : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-FINGERDOWN               : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-FINGERUP                 : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-FINGERMOTION             : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-DOLLARGESTURE            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-DOLLARRECORD             : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-MULTIGESTURE             : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-CLIPBOARDUPDATE          : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-DROPFILE                 : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-AUDIODEVICEADDED         : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-AUDIODEVICEREMOVED       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-RENDER-TARGETS-RESET     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-RENDER-DEVICE-RESET      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-USEREVENT                : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LASTEVENT                : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-HAT-CENTERED  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-HAT-UP        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-HAT-RIGHT     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-HAT-DOWN      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-HAT-LEFT      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-HAT-RIGHTUP   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-HAT-RIGHTDOWN : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-HAT-LEFTUP    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-HAT-LEFTDOWN  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-PRESSED  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-RELEASED : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-LOG-CATEGORY-APPLICATION : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LOG-CATEGORY-ERROR       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LOG-CATEGORY-ASSERT      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LOG-CATEGORY-SYSTEM      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LOG-CATEGORY-AUDIO       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LOG-CATEGORY-VIDEO       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LOG-CATEGORY-RENDER      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LOG-CATEGORY-INPUT       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LOG-CATEGORY-TEST        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-LOG-CATEGORY-CUSTOM      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-MESSAGEBOX-ERROR       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-MESSAGEBOX-WARNING     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-MESSAGEBOX-INFORMATION : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-MESSAGEBOX-BUTTON-RETURNKEY-DEFAULT : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-MESSAGEBOX-BUTTON-ESCAPEKEY-DEFAULT : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-GL-CONTEXT-PROFILE-CORE          : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-GL-CONTEXT-PROFILE-COMPATIBILITY : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-GL-CONTEXT-PROFILE-ES            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-GL-CONTEXT-DEBUG-FLAG              : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-GL-CONTEXT-FORWARD-COMPATIBLE-FLAG : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-GL-CONTEXT-ROBUST-ACCESS-FLAG      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-GL-CONTEXT-RESET-ISOLATION-FLAG    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-GL-CONTEXT-RELEASE-BEHAVIOR-NONE  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-GL-CONTEXT-RELEASE-BEHAVIOR-FLUSH : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-PIXELFORMAT-UNKNOWN     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-INDEX1LSB   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-INDEX1MSB   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-INDEX4LSB   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-INDEX4MSB   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-INDEX8      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGB332      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGB444      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGB555      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-BGR555      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-ARGB4444    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGBA4444    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-ABGR4444    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-BGRA4444    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-ARGB1555    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGBA5551    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-ABGR1555    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-BGRA5551    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGB565      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-BGR565      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGB24       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-BGR24       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGB888      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGBX8888    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-BGR888      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-BGRX8888    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-ARGB8888    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-RGBA8888    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-ABGR8888    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-BGRA8888    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-ARGB2101010 : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-YV12        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-IYUV        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-YUY2        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-UYVY        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-PIXELFORMAT-YVYU        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-RENDERER-SOFTWARE      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-RENDERER-ACCELERATED   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-RENDERER-PRESENTVSYNC  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-RENDERER-TARGETTEXTURE : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-TEXTUREACCESS-STATIC    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-TEXTUREACCESS-STREAMING : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-TEXTUREACCESS-TARGET    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-TEXTUREMODULATE-NONE  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-TEXTUREMODULATE-COLOR : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-TEXTUREMODULATE-ALPHA : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-TOUCH-MOUSEID : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-WINDOWEVENT-NONE         : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-SHOWN        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-HIDDEN       : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-EXPOSED      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-MOVED        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-RESIZED      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-SIZE-CHANGED : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-MINIMIZED    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-MAXIMIZED    : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-RESTORED     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-ENTER        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-LEAVE        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-FOCUS-GAINED : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-FOCUS-LOST   : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWEVENT-CLOSE        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-WINDOW-FULLSCREEN         : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-OPENGL             : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-SHOWN              : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-HIDDEN             : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-BORDERLESS         : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-RESIZABLE          : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-MINIMIZED          : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-MAXIMIZED          : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-INPUT-GRABBED      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-INPUT-FOCUS        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-MOUSE-FOCUS        : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-FULLSCREEN-DESKTOP : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-FOREIGN            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-ALLOW-HIGHDPI      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-MOUSE-CAPTURE      : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOW-VULKAN             : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-WINDOWPOS-UNDEFINED           : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED            : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-0  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-1  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-2  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-3  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-4  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-5  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-6  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-7  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-8  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-9  : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-10 : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-11 : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-12 : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-13 : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-14 : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A
    SDL-WINDOWPOS-CENTERED-DISPLAY-15 : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

    SDL-HAPTIC-CONSTANT : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → A

{-# COMPILE GHC SDL-AUDIO-ALLOW-FREQUENCY-CHANGE = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_AUDIO_ALLOW_FREQUENCY_CHANGE #-}
{-# COMPILE GHC SDL-AUDIO-ALLOW-FORMAT-CHANGE    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_AUDIO_ALLOW_FORMAT_CHANGE    #-}
{-# COMPILE GHC SDL-AUDIO-ALLOW-CHANNELS-CHANGE  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_AUDIO_ALLOW_CHANNELS_CHANGE  #-}
{-# COMPILE GHC SDL-AUDIO-ALLOW-ANY-CHANGE       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_AUDIO_ALLOW_ANY_CHANGE       #-}

{-# COMPILE GHC SDL-BUTTON-LEFT   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_LEFT   #-}
{-# COMPILE GHC SDL-BUTTON-MIDDLE = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_MIDDLE #-}
{-# COMPILE GHC SDL-BUTTON-RIGHT  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_RIGHT  #-}
{-# COMPILE GHC SDL-BUTTON-X1     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_X1     #-}
{-# COMPILE GHC SDL-BUTTON-X2     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_X2     #-}

{-# COMPILE GHC SDL-BUTTON-LMASK  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_LMASK  #-}
{-# COMPILE GHC SDL-BUTTON-MMASK  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_MMASK  #-}
{-# COMPILE GHC SDL-BUTTON-RMASK  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_RMASK  #-}
{-# COMPILE GHC SDL-BUTTON-X1MASK = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_X1MASK #-}
{-# COMPILE GHC SDL-BUTTON-X2MASK = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_BUTTON_X2MASK #-}

{-# COMPILE GHC SDL-MOUSEWHEEL-NORMAL  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MOUSEWHEEL_NORMAL  #-}
{-# COMPILE GHC SDL-MOUSEWHEEL-FLIPPED = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MOUSEWHEEL_FLIPPED #-}

{-# COMPILE GHC SDL-FIRSTEVENT               = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_FIRSTEVENT               #-}
{-# COMPILE GHC SDL-QUIT                     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_QUIT                     #-}
{-# COMPILE GHC SDL-APP-TERMINATING          = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_APP_TERMINATING          #-}
{-# COMPILE GHC SDL-APP-LOWMEMORY            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_APP_LOWMEMORY            #-}
{-# COMPILE GHC SDL-APP-WILLENTERBACKGROUND  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_APP_WILLENTERBACKGROUND  #-}
{-# COMPILE GHC SDL-APP-DIDENTERBACKGROUND   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_APP_DIDENTERBACKGROUND   #-}
{-# COMPILE GHC SDL-APP-WILLENTERFOREGROUND  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_APP_WILLENTERFOREGROUND  #-}
{-# COMPILE GHC SDL-APP-DIDENTERFOREGROUND   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_APP_DIDENTERFOREGROUND   #-}
{-# COMPILE GHC SDL-WINDOWEVENT              = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT              #-}
{-# COMPILE GHC SDL-SYSWMEVENT               = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_SYSWMEVENT               #-}
{-# COMPILE GHC SDL-KEYDOWN                  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_KEYDOWN                  #-}
{-# COMPILE GHC SDL-KEYUP                    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_KEYUP                    #-}
{-# COMPILE GHC SDL-TEXTEDITING              = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_TEXTEDITING              #-}
{-# COMPILE GHC SDL-TEXTINPUT                = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_TEXTINPUT                #-}
{-# COMPILE GHC SDL-KEYMAPCHANGED            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_KEYMAPCHANGED            #-}
{-# COMPILE GHC SDL-MOUSEMOTION              = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MOUSEMOTION              #-}
{-# COMPILE GHC SDL-MOUSEBUTTONDOWN          = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MOUSEBUTTONDOWN          #-}
{-# COMPILE GHC SDL-MOUSEBUTTONUP            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MOUSEBUTTONUP            #-}
{-# COMPILE GHC SDL-MOUSEWHEEL               = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MOUSEWHEEL               #-}
{-# COMPILE GHC SDL-JOYAXISMOTION            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_JOYAXISMOTION            #-}
{-# COMPILE GHC SDL-JOYBALLMOTION            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_JOYBALLMOTION            #-}
{-# COMPILE GHC SDL-JOYHATMOTION             = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_JOYHATMOTION             #-}
{-# COMPILE GHC SDL-JOYBUTTONDOWN            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_JOYBUTTONDOWN            #-}
{-# COMPILE GHC SDL-JOYBUTTONUP              = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_JOYBUTTONUP              #-}
{-# COMPILE GHC SDL-JOYDEVICEADDED           = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_JOYDEVICEADDED           #-}
{-# COMPILE GHC SDL-JOYDEVICEREMOVED         = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_JOYDEVICEREMOVED         #-}
{-# COMPILE GHC SDL-CONTROLLERAXISMOTION     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_CONTROLLERAXISMOTION     #-}
{-# COMPILE GHC SDL-CONTROLLERBUTTONDOWN     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_CONTROLLERBUTTONDOWN     #-}
{-# COMPILE GHC SDL-CONTROLLERBUTTONUP       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_CONTROLLERBUTTONUP       #-}
{-# COMPILE GHC SDL-CONTROLLERDEVICEADDED    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_CONTROLLERDEVICEADDED    #-}
{-# COMPILE GHC SDL-CONTROLLERDEVICEREMOVED  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_CONTROLLERDEVICEREMOVED  #-}
{-# COMPILE GHC SDL-CONTROLLERDEVICEREMAPPED = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_CONTROLLERDEVICEREMAPPED #-}
{-# COMPILE GHC SDL-FINGERDOWN               = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_FINGERDOWN               #-}
{-# COMPILE GHC SDL-FINGERUP                 = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_FINGERUP                 #-}
{-# COMPILE GHC SDL-FINGERMOTION             = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_FINGERMOTION             #-}
{-# COMPILE GHC SDL-DOLLARGESTURE            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_DOLLARGESTURE            #-}
{-# COMPILE GHC SDL-DOLLARRECORD             = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_DOLLARRECORD             #-}
{-# COMPILE GHC SDL-MULTIGESTURE             = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MULTIGESTURE             #-}
{-# COMPILE GHC SDL-CLIPBOARDUPDATE          = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_CLIPBOARDUPDATE          #-}
{-# COMPILE GHC SDL-DROPFILE                 = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_DROPFILE                 #-}
{-# COMPILE GHC SDL-AUDIODEVICEADDED         = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_AUDIODEVICEADDED         #-}
{-# COMPILE GHC SDL-AUDIODEVICEREMOVED       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_AUDIODEVICEREMOVED       #-}
{-# COMPILE GHC SDL-RENDER-TARGETS-RESET     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_RENDER_TARGETS_RESET     #-}
{-# COMPILE GHC SDL-RENDER-DEVICE-RESET      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_RENDER_DEVICE_RESET      #-}
{-# COMPILE GHC SDL-USEREVENT                = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_USEREVENT                #-}
{-# COMPILE GHC SDL-LASTEVENT                = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LASTEVENT                #-}

{-# COMPILE GHC SDL-HAT-CENTERED  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAT_CENTERED  #-}
{-# COMPILE GHC SDL-HAT-UP        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAT_UP        #-}
{-# COMPILE GHC SDL-HAT-RIGHT     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAT_RIGHT     #-}
{-# COMPILE GHC SDL-HAT-DOWN      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAT_DOWN      #-}
{-# COMPILE GHC SDL-HAT-LEFT      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAT_LEFT      #-}
{-# COMPILE GHC SDL-HAT-RIGHTUP   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAT_RIGHTUP   #-}
{-# COMPILE GHC SDL-HAT-RIGHTDOWN = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAT_RIGHTDOWN #-}
{-# COMPILE GHC SDL-HAT-LEFTUP    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAT_LEFTUP    #-}
{-# COMPILE GHC SDL-HAT-LEFTDOWN  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAT_LEFTDOWN  #-}

{-# COMPILE GHC SDL-PRESSED  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PRESSED  #-}
{-# COMPILE GHC SDL-RELEASED = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_RELEASED #-}

{-# COMPILE GHC SDL-LOG-CATEGORY-APPLICATION = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_APPLICATION #-}
{-# COMPILE GHC SDL-LOG-CATEGORY-ERROR       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_ERROR       #-}
{-# COMPILE GHC SDL-LOG-CATEGORY-ASSERT      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_ASSERT      #-}
{-# COMPILE GHC SDL-LOG-CATEGORY-SYSTEM      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_SYSTEM      #-}
{-# COMPILE GHC SDL-LOG-CATEGORY-AUDIO       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_AUDIO       #-}
{-# COMPILE GHC SDL-LOG-CATEGORY-VIDEO       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_VIDEO       #-}
{-# COMPILE GHC SDL-LOG-CATEGORY-RENDER      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_RENDER      #-}
{-# COMPILE GHC SDL-LOG-CATEGORY-INPUT       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_INPUT       #-}
{-# COMPILE GHC SDL-LOG-CATEGORY-TEST        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_TEST        #-}
{-# COMPILE GHC SDL-LOG-CATEGORY-CUSTOM      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_LOG_CATEGORY_CUSTOM      #-}

{-# COMPILE GHC SDL-MESSAGEBOX-ERROR       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MESSAGEBOX_ERROR       #-}
{-# COMPILE GHC SDL-MESSAGEBOX-WARNING     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MESSAGEBOX_WARNING     #-}
{-# COMPILE GHC SDL-MESSAGEBOX-INFORMATION = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MESSAGEBOX_INFORMATION #-}

{-# COMPILE GHC SDL-MESSAGEBOX-BUTTON-RETURNKEY-DEFAULT = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT #-}
{-# COMPILE GHC SDL-MESSAGEBOX-BUTTON-ESCAPEKEY-DEFAULT = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT #-}

{-# COMPILE GHC SDL-GL-CONTEXT-PROFILE-CORE          = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_GL_CONTEXT_PROFILE_CORE          #-}
{-# COMPILE GHC SDL-GL-CONTEXT-PROFILE-COMPATIBILITY = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_GL_CONTEXT_PROFILE_COMPATIBILITY #-}
{-# COMPILE GHC SDL-GL-CONTEXT-PROFILE-ES            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_GL_CONTEXT_PROFILE_ES            #-}

{-# COMPILE GHC SDL-GL-CONTEXT-DEBUG-FLAG              = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_GL_CONTEXT_DEBUG_FLAG              #-}
{-# COMPILE GHC SDL-GL-CONTEXT-FORWARD-COMPATIBLE-FLAG = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG #-}
{-# COMPILE GHC SDL-GL-CONTEXT-ROBUST-ACCESS-FLAG      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG      #-}
{-# COMPILE GHC SDL-GL-CONTEXT-RESET-ISOLATION-FLAG    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_GL_CONTEXT_RESET_ISOLATION_FLAG    #-}

{-# COMPILE GHC SDL-GL-CONTEXT-RELEASE-BEHAVIOR-NONE  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE  #-}
{-# COMPILE GHC SDL-GL-CONTEXT-RELEASE-BEHAVIOR-FLUSH = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH #-}

{-# COMPILE GHC SDL-PIXELFORMAT-UNKNOWN     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_UNKNOWN     #-}
{-# COMPILE GHC SDL-PIXELFORMAT-INDEX1LSB   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_INDEX1LSB   #-}
{-# COMPILE GHC SDL-PIXELFORMAT-INDEX1MSB   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_INDEX1MSB   #-}
{-# COMPILE GHC SDL-PIXELFORMAT-INDEX4LSB   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_INDEX4LSB   #-}
{-# COMPILE GHC SDL-PIXELFORMAT-INDEX4MSB   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_INDEX4MSB   #-}
{-# COMPILE GHC SDL-PIXELFORMAT-INDEX8      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_INDEX8      #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGB332      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGB332      #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGB444      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGB444      #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGB555      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGB555      #-}
{-# COMPILE GHC SDL-PIXELFORMAT-BGR555      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_BGR555      #-}
{-# COMPILE GHC SDL-PIXELFORMAT-ARGB4444    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_ARGB4444    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGBA4444    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGBA4444    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-ABGR4444    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_ABGR4444    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-BGRA4444    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_BGRA4444    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-ARGB1555    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_ARGB1555    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGBA5551    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGBA5551    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-ABGR1555    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_ABGR1555    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-BGRA5551    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_BGRA5551    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGB565      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGB565      #-}
{-# COMPILE GHC SDL-PIXELFORMAT-BGR565      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_BGR565      #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGB24       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGB24       #-}
{-# COMPILE GHC SDL-PIXELFORMAT-BGR24       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_BGR24       #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGB888      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGB888      #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGBX8888    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGBX8888    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-BGR888      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_BGR888      #-}
{-# COMPILE GHC SDL-PIXELFORMAT-BGRX8888    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_BGRX8888    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-ARGB8888    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_ARGB8888    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-RGBA8888    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_RGBA8888    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-ABGR8888    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_ABGR8888    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-BGRA8888    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_BGRA8888    #-}
{-# COMPILE GHC SDL-PIXELFORMAT-ARGB2101010 = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_ARGB2101010 #-}
{-# COMPILE GHC SDL-PIXELFORMAT-YV12        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_YV12        #-}
{-# COMPILE GHC SDL-PIXELFORMAT-IYUV        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_IYUV        #-}
{-# COMPILE GHC SDL-PIXELFORMAT-YUY2        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_YUY2        #-}
{-# COMPILE GHC SDL-PIXELFORMAT-UYVY        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_UYVY        #-}
{-# COMPILE GHC SDL-PIXELFORMAT-YVYU        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_PIXELFORMAT_YVYU        #-}

{-# COMPILE GHC SDL-RENDERER-SOFTWARE      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_RENDERER_SOFTWARE      #-}
{-# COMPILE GHC SDL-RENDERER-ACCELERATED   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_RENDERER_ACCELERATED   #-}
{-# COMPILE GHC SDL-RENDERER-PRESENTVSYNC  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_RENDERER_PRESENTVSYNC  #-}
{-# COMPILE GHC SDL-RENDERER-TARGETTEXTURE = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_RENDERER_TARGETTEXTURE #-}

{-# COMPILE GHC SDL-TEXTUREACCESS-STATIC    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_TEXTUREACCESS_STATIC    #-}
{-# COMPILE GHC SDL-TEXTUREACCESS-STREAMING = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_TEXTUREACCESS_STREAMING #-}
{-# COMPILE GHC SDL-TEXTUREACCESS-TARGET    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_TEXTUREACCESS_TARGET    #-}

{-# COMPILE GHC SDL-TEXTUREMODULATE-NONE  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_TEXTUREMODULATE_NONE  #-}
{-# COMPILE GHC SDL-TEXTUREMODULATE-COLOR = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_TEXTUREMODULATE_COLOR #-}
{-# COMPILE GHC SDL-TEXTUREMODULATE-ALPHA = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_TEXTUREMODULATE_ALPHA #-}

{-# COMPILE GHC SDL-TOUCH-MOUSEID = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_TOUCH_MOUSEID #-}

{-# COMPILE GHC SDL-WINDOWEVENT-NONE         = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_NONE         #-}
{-# COMPILE GHC SDL-WINDOWEVENT-SHOWN        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_SHOWN        #-}
{-# COMPILE GHC SDL-WINDOWEVENT-HIDDEN       = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_HIDDEN       #-}
{-# COMPILE GHC SDL-WINDOWEVENT-EXPOSED      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_EXPOSED      #-}
{-# COMPILE GHC SDL-WINDOWEVENT-MOVED        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_MOVED        #-}
{-# COMPILE GHC SDL-WINDOWEVENT-RESIZED      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_RESIZED      #-}
{-# COMPILE GHC SDL-WINDOWEVENT-SIZE-CHANGED = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_SIZE_CHANGED #-}
{-# COMPILE GHC SDL-WINDOWEVENT-MINIMIZED    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_MINIMIZED    #-}
{-# COMPILE GHC SDL-WINDOWEVENT-MAXIMIZED    = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_MAXIMIZED    #-}
{-# COMPILE GHC SDL-WINDOWEVENT-RESTORED     = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_RESTORED     #-}
{-# COMPILE GHC SDL-WINDOWEVENT-ENTER        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_ENTER        #-}
{-# COMPILE GHC SDL-WINDOWEVENT-LEAVE        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_LEAVE        #-}
{-# COMPILE GHC SDL-WINDOWEVENT-FOCUS-GAINED = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_FOCUS_GAINED #-}
{-# COMPILE GHC SDL-WINDOWEVENT-FOCUS-LOST   = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_FOCUS_LOST   #-}
{-# COMPILE GHC SDL-WINDOWEVENT-CLOSE        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWEVENT_CLOSE        #-}

{-# COMPILE GHC SDL-WINDOW-FULLSCREEN         = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_FULLSCREEN         #-}
{-# COMPILE GHC SDL-WINDOW-OPENGL             = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_OPENGL             #-}
{-# COMPILE GHC SDL-WINDOW-SHOWN              = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_SHOWN              #-}
{-# COMPILE GHC SDL-WINDOW-HIDDEN             = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_HIDDEN             #-}
{-# COMPILE GHC SDL-WINDOW-BORDERLESS         = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_BORDERLESS         #-}
{-# COMPILE GHC SDL-WINDOW-RESIZABLE          = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_RESIZABLE          #-}
{-# COMPILE GHC SDL-WINDOW-MINIMIZED          = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_MINIMIZED          #-}
{-# COMPILE GHC SDL-WINDOW-MAXIMIZED          = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_MAXIMIZED          #-}
{-# COMPILE GHC SDL-WINDOW-INPUT-GRABBED      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_INPUT_GRABBED      #-}
{-# COMPILE GHC SDL-WINDOW-INPUT-FOCUS        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_INPUT_FOCUS        #-}
{-# COMPILE GHC SDL-WINDOW-MOUSE-FOCUS        = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_MOUSE_FOCUS        #-}
{-# COMPILE GHC SDL-WINDOW-FULLSCREEN-DESKTOP = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_FULLSCREEN_DESKTOP #-}
{-# COMPILE GHC SDL-WINDOW-FOREIGN            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_FOREIGN            #-}
{-# COMPILE GHC SDL-WINDOW-ALLOW-HIGHDPI      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_ALLOW_HIGHDPI      #-}
{-# COMPILE GHC SDL-WINDOW-MOUSE-CAPTURE      = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_MOUSE_CAPTURE      #-}
{-# COMPILE GHC SDL-WINDOW-VULKAN             = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOW_VULKAN             #-}

{-# COMPILE GHC SDL-WINDOWPOS-UNDEFINED           = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_UNDEFINED           #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED            = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED            #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-0  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_0  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-1  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_1  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-2  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_2  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-3  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_3  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-4  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_4  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-5  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_5  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-6  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_6  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-7  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_7  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-8  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_8  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-9  = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_9  #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-10 = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_10 #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-11 = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_11 #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-12 = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_12 #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-13 = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_13 #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-14 = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_14 #-}
{-# COMPILE GHC SDL-WINDOWPOS-CENTERED-DISPLAY-15 = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_WINDOWPOS_CENTERED_DISPLAY_15 #-}

{-# COMPILE GHC SDL-HAPTIC-CONSTANT = \ aℓ a AgdaEq AgdaNum -> SDL.Raw.Enum.SDL_HAPTIC_CONSTANT #-}
