{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Exit where

open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Show; Read; Eq; Ord; Exception)
open import Ffi.Hs.Data.Int    using (Int)

open import Ffi.Hs.GHC.IO.Exception public
    using
    ( ExitCode; ExitSuccess; ExitFailure
    ; Show[ExitCode]; Exception[ExitCode]
    ; Read[ExitCode]; Eq[ExitCode]; Ord[ExitCode]
    )

{-# FOREIGN GHC
import qualified System.Exit
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    exitWith    : ExitCode → IO A
    exitFailure : IO A
    exitSuccess : IO A
    die         : List Char → IO A

{-# COMPILE GHC exitWith    = \ aℓ a -> System.Exit.exitWith    #-}
{-# COMPILE GHC exitFailure = \ aℓ a -> System.Exit.exitFailure #-}
{-# COMPILE GHC exitSuccess = \ aℓ a -> System.Exit.exitSuccess #-}
{-# COMPILE GHC die         = \ aℓ a -> System.Exit.die         #-}
