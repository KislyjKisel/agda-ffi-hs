{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Environment.Blank where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)

open import Ffi.Hs.System.Environment public
    using
    ( getArgs ; getProgName ; getExecutablePath
    ; withArgs ; withProgName ; getEnvironment
    ; getEnv ; setEnv ; unsetEnv
    )

{-# FOREIGN GHC
import qualified System.Environment.Blank
#-}

postulate
    getEnvDefault : List Char → List Char → IO (List Char)

{-# COMPILE GHC getEnvDefault = System.Environment.Blank.getEnvDefault #-}
