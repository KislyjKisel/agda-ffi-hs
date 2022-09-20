{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Environment where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit  using (⊤; ⊤′)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

private
    variable
        aℓ : Level
        A : Set aℓ

{-# FOREIGN GHC
import qualified System.Environment
#-}

postulate
    getArgs           : IO (List (List Char))
    getProgName       : IO (List Char)
    executablePath    : Maybe (IO (Maybe (List Char)))
    getExecutablePath : IO (List Char)
    getEnv            : List Char → IO (List Char)
    lookupEnv         : List Char → IO (Maybe (List Char))
    setEnv            : List Char → List Char → IO ⊤
    unsetEnv          : List Char → IO ⊤
    withArgs          : List (List Char) → IO A → IO A
    withProgName      : List Char → IO A → IO A
    getEnvironment    : IO (List (Tuple2 (List Char) (List Char)))

{-# COMPILE GHC getArgs           =           System.Environment.getArgs           #-}
{-# COMPILE GHC getProgName       =           System.Environment.getProgName       #-}
{-# COMPILE GHC executablePath    =           System.Environment.executablePath    #-}
{-# COMPILE GHC getExecutablePath =           System.Environment.getExecutablePath #-}
{-# COMPILE GHC getEnv            =           System.Environment.getEnv            #-}
{-# COMPILE GHC lookupEnv         =           System.Environment.lookupEnv         #-}
{-# COMPILE GHC setEnv            =           System.Environment.setEnv            #-}
{-# COMPILE GHC unsetEnv          =           System.Environment.unsetEnv          #-}
{-# COMPILE GHC withArgs          = \ aℓ a -> System.Environment.withArgs          #-}
{-# COMPILE GHC withProgName      = \ aℓ a -> System.Environment.withProgName      #-}
{-# COMPILE GHC getEnvironment    =           System.Environment.getEnvironment    #-}
