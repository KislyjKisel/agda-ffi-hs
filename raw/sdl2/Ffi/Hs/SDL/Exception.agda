{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Exception where

open import Agda.Builtin.Char   using (Char)
open import Agda.Builtin.List   using (List)
open import Agda.Builtin.String using () renaming (String to Text)
open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Exception
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

-- todo: getters
data SDLException : Set where
    SDLCallFailed         : Text → Text → Text → SDLException
    SDLUnexpectedArgument : Text → Text → List Char → SDLException
    SDLUnknownHintValue   : List Char → List Char → SDLException

{-# COMPILE GHC SDLException = data SDL.Exception.SDLException
    ( SDL.Exception.SDLCallFailed
    | SDL.Exception.SDLUnexpectedArgument
    | SDL.Exception.SDLUnknownHintValue
    ) #-}

postulate
    Eq[SDLException]        : Eq SDLException
    Data[SDLException]      : Data SDLException
    Ord[SDLException]       : Ord SDLException
    Read[SDLException]      : Read SDLException
    Show[SDLException]      : Show SDLException
    Exception[SDLException] : Exception SDLException

{-# COMPILE GHC Eq[SDLException]        = AgdaEq        #-}
{-# COMPILE GHC Data[SDLException]      = AgdaData      #-}
{-# COMPILE GHC Ord[SDLException]       = AgdaOrd       #-}
{-# COMPILE GHC Read[SDLException]      = AgdaRead      #-}
{-# COMPILE GHC Show[SDLException]      = AgdaShow      #-}
{-# COMPILE GHC Exception[SDLException] = AgdaException #-}
